#' Safely delete plot(s) with all related data
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' This function safely deletes one or more plots and all related data in the
#' correct order to respect foreign key constraints:
#' 1. Individual measurement features
#' 2. Trait measurements
#' 3. Individual measurements (if exists)
#' 4. Individuals
#' 5. Subplot features
#' 6. Subplots
#' 7. Plot (skipped if \code{delete_plot = FALSE})
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Shows counts of all related data
#' - Requires explicit confirmation (unless force = TRUE)
#' - Uses database transaction (rolls back on error)
#' - Detailed logging of each step
#'
#' @param plot_ids Integer vector. Plot ID(s) to delete (id_liste_plots)
#' @param con Database connection. If NULL, will connect automatically.
#' @param dry_run Logical. If TRUE, shows what would be deleted without deleting.
#'   Default TRUE for safety.
#' @param force Logical. If TRUE, skips confirmation prompts. Default FALSE.
#'   **USE WITH EXTREME CAUTION!**
#' @param delete_individuals Logical. Delete individuals? Default TRUE.
#' @param delete_subplots Logical. Delete subplot features? Default TRUE.
#' @param delete_plot Logical. Delete the plot record itself? Default TRUE. Set to
#'   FALSE combined with \code{delete_subplots = FALSE} to remove only individuals
#'   and their features while preserving all plot metadata (same as
#'   \code{\link{safe_delete_individuals}}).
#' @param verbose Logical. Show detailed progress? Default TRUE.
#'
#' @return List with deletion summary (invisible)
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # STEP 1: Always do dry-run first!
#' safe_delete_plot(plot_ids = 123, dry_run = TRUE)
#'
#' # STEP 2: Review the output, then delete if sure
#' safe_delete_plot(plot_ids = 123, dry_run = FALSE)
#'
#' # Delete multiple plots
#' safe_delete_plot(plot_ids = c(123, 124, 125))
#'
#' # Delete plot but keep individuals (rare)
#' safe_delete_plot(plot_ids = 123, delete_individuals = FALSE)
#'
#' # Delete ONLY individuals and their features, keep plot metadata
#' safe_delete_plot(plot_ids = 123, delete_plot = FALSE, delete_subplots = FALSE)
#' }
#'
#' @export
safe_delete_plot <- function(plot_ids,
                             con = NULL,
                             dry_run = TRUE,
                             force = FALSE,
                             delete_individuals = TRUE,
                             delete_subplots = TRUE,
                             delete_plot = TRUE,
                             verbose = TRUE) {

  # Validate inputs
  if (length(plot_ids) == 0 || any(is.na(plot_ids))) {
    stop("plot_ids must be non-empty integer vector", call. = FALSE)
  }

  if (is.null(con)) {
    con <- call.mydb()
  }

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  # Storage for deletion summary
  summary <- list(
    plot_ids = plot_ids,
    dry_run = dry_run,
    deleted = list(),
    errors = list()
  )

  # ===== STEP 1: Check what exists and will be deleted =====
  if (verbose) cli::cli_h1("Analyzing Plot Deletion")

  # Check plots exist (only use columns that should always exist)
  plots_info <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT id_liste_plots, plot_name
      FROM data_liste_plots
      WHERE id_liste_plots IN (%s)
    ", paste(plot_ids, collapse = ",")))
  }, error = function(e) {
    stop("Failed to query plots: ", e$message, call. = FALSE)
  })

  if (nrow(plots_info) == 0) {
    cli::cli_alert_danger("No plots found with IDs: {paste(plot_ids, collapse = ', ')}")
    return(invisible(summary))
  }

  if (nrow(plots_info) < length(plot_ids)) {
    missing <- setdiff(plot_ids, plots_info$id_liste_plots)
    cli::cli_alert_warning("Some plot IDs not found: {paste(missing, collapse = ', ')}")
  }

  # Show plots to be deleted
  if (verbose) {
    cli::cli_h2("Plots to Delete")
    cli::cli_alert_info("Found {nrow(plots_info)} plot(s):")
    for (i in 1:nrow(plots_info)) {
      cli::cli_li("ID: {plots_info$id_liste_plots[i]} | Name: {plots_info$plot_name[i]}")
    }
  }

  # ===== STEP 2: Count related data =====
  if (verbose) cli::cli_h2("Related Data")

  # Count individuals
  n_individuals <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n
    FROM data_individuals
    WHERE id_table_liste_plots_n IN (%s)
  ", paste(plot_ids, collapse = ",")))$n

  summary$counts$individuals <- n_individuals

  if (n_individuals > 0) {
    cli::cli_alert_warning("{n_individuals} individual(s) will be deleted")

    # Count trait measurements for these individuals
    n_trait_measures <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) as n
      FROM data_traits_measures
      WHERE id_data_individuals IN (
        SELECT id_n FROM data_individuals
        WHERE id_table_liste_plots_n IN (%s)
      )
    ", paste(plot_ids, collapse = ",")))$n

    summary$counts$trait_measurements <- n_trait_measures
    if (n_trait_measures > 0) {
      cli::cli_alert_warning("  └─ {n_trait_measures} trait measurement(s)")
    }

    # Count measurement features
    n_meas_feat <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) as n
      FROM data_ind_measures_feat
      WHERE id_trait_measures IN (
        SELECT id_trait_measures FROM data_traits_measures
        WHERE id_data_individuals IN (
          SELECT id_n FROM data_individuals
          WHERE id_table_liste_plots_n IN (%s)
        )
      )
    ", paste(plot_ids, collapse = ",")))$n

    summary$counts$measurement_features <- n_meas_feat
    if (n_meas_feat > 0) {
      cli::cli_alert_warning("     └─ {n_meas_feat} measurement feature(s)")
    }
  } else {
    cli::cli_alert_success("No individuals found")
  }

  # Count subplots
  n_subplots <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n
    FROM data_liste_sub_plots
    WHERE id_table_liste_plots IN (%s)
  ", paste(plot_ids, collapse = ",")))$n

  summary$counts$subplots <- n_subplots
  if (n_subplots > 0) {
    cli::cli_alert_warning("{n_subplots} subplot feature(s) will be deleted")
  } else {
    cli::cli_alert_success("No subplot features found")
  }

  # ===== STEP 3: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN - nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    if (!delete_plot) {
      cli::cli_alert_success("Plot metadata will be preserved (delete_plot = FALSE)")
    }
    if (!delete_subplots) {
      cli::cli_alert_success("Subplot features will be preserved (delete_subplots = FALSE)")
    }
    return(invisible(summary))
  }

  # ===== STEP 4: Confirmation =====
  if (!force) {
    cli::cli_h2("⚠️  CONFIRMATION REQUIRED ⚠️")
    cli::cli_alert_danger("You are about to PERMANENTLY delete:")
    cli::cli_ul(c(
      if (delete_plot) "{nrow(plots_info)} plot(s)" else NULL,
      if (n_individuals > 0 && delete_individuals) "{n_individuals} individual(s)" else NULL,
      if (n_trait_measures > 0 && delete_individuals) "{n_trait_measures} trait measurement(s)" else NULL,
      if (n_meas_feat > 0 && delete_individuals) "{n_meas_feat} measurement feature(s)" else NULL,
      if (n_subplots > 0 && delete_subplots) "{n_subplots} subplot feature(s)" else NULL
    ))
    if (!delete_plot) {
      cli::cli_alert_success("Plot metadata will be PRESERVED (not deleted)")
    }

    confirm <- choose_prompt(message = "Are you ABSOLUTELY SURE you want to delete this data?")

    if (!confirm) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 5: Delete in correct order (batched) =====
  cli::cli_h2("Deleting Data")

  summary$success <- FALSE
  batch_size <- 2000

  # Pre-resolve IDs to avoid expensive nested subqueries during DELETE
  if (delete_individuals && n_individuals > 0) {
    if (verbose) cli::cli_alert_info("Resolving individual IDs...")
    individual_ids <- DBI::dbGetQuery(con, sprintf("
      SELECT id_n FROM data_individuals
      WHERE id_table_liste_plots_n IN (%s)
    ", paste(plot_ids, collapse = ",")))$id_n

    if (n_trait_measures > 0) {
      if (verbose) cli::cli_alert_info("Resolving trait measurement IDs...")
      trait_measure_ids <- DBI::dbGetQuery(con, sprintf("
        SELECT id_trait_measures FROM data_traits_measures
        WHERE id_data_individuals IN (%s)
      ", paste(individual_ids, collapse = ",")))$id_trait_measures
    } else {
      trait_measure_ids <- integer(0)
    }
  } else {
    individual_ids <- integer(0)
    trait_measure_ids <- integer(0)
  }

  # Helper: batch-delete by ID list, each batch in its own transaction
  batch_delete <- function(con, table, id_column, ids, label, verbose) {
    if (length(ids) == 0) return(0L)
    total <- 0L
    n_batches <- ceiling(length(ids) / batch_size)

    for (batch_idx in seq_len(n_batches)) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx   <- min(batch_idx * batch_size, length(ids))
      batch_ids <- ids[start_idx:end_idx]

      DBI::dbBegin(con)
      tryCatch({
        rs <- DBI::dbSendQuery(con, sprintf(
          "DELETE FROM %s WHERE %s IN (%s)",
          table, id_column, paste(batch_ids, collapse = ",")
        ))
        n_del <- DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)
        DBI::dbCommit(con)
        total <- total + n_del

        if (verbose && n_batches > 1) {
          cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_del} {label}")
        }
      }, error = function(e) {
        tryCatch(DBI::dbRollback(con), error = function(e2) {})
        stop(sprintf("Error deleting %s (batch %d/%d): %s",
                     label, batch_idx, n_batches, e$message), call. = FALSE)
      })
    }
    total
  }

  tryCatch({

    # Step 5.0: Delete specimen links (data_link_specimens) before individuals
    if (length(individual_ids) > 0) {
      n_specimen_links <- DBI::dbGetQuery(con, sprintf("
        SELECT COUNT(*) as n FROM data_link_specimens WHERE id_n IN (%s)
      ", paste(individual_ids, collapse = ",")))$n

      if (n_specimen_links > 0) {
        if (verbose) cli::cli_alert_info("Deleting {n_specimen_links} specimen link(s)...")
        n_deleted <- batch_delete(con, "data_link_specimens", "id_n",
                                  individual_ids, "specimen link(s)", verbose)
        summary$deleted$specimen_links <- n_deleted
        if (verbose) cli::cli_alert_success("  Deleted {n_deleted} specimen link(s)")
      }
    }

    # Step 5.1: Delete measurement features (by trait_measure_ids)
    if (length(trait_measure_ids) > 0 && n_meas_feat > 0) {
      if (verbose) cli::cli_alert_info("Deleting measurement features...")
      n_deleted <- batch_delete(con, "data_ind_measures_feat", "id_trait_measures",
                                trait_measure_ids, "measurement feature(s)", verbose)
      summary$deleted$measurement_features <- n_deleted
      if (verbose) cli::cli_alert_success("  Deleted {n_deleted} measurement feature(s)")
    }

    # Step 5.2: Delete trait measurements (by individual_ids)
    if (length(individual_ids) > 0 && n_trait_measures > 0) {
      if (verbose) cli::cli_alert_info("Deleting trait measurements...")
      n_deleted <- batch_delete(con, "data_traits_measures", "id_data_individuals",
                                individual_ids, "trait measurement(s)", verbose)
      summary$deleted$trait_measurements <- n_deleted
      if (verbose) cli::cli_alert_success("  Deleted {n_deleted} trait measurement(s)")
    }

    # Step 5.3: Delete individuals (by plot_ids)
    if (length(individual_ids) > 0 && delete_individuals) {
      if (verbose) cli::cli_alert_info("Deleting individuals...")
      n_deleted <- batch_delete(con, "data_individuals", "id_table_liste_plots_n",
                                plot_ids, "individual(s)", verbose)
      summary$deleted$individuals <- n_deleted
      if (verbose) cli::cli_alert_success("  Deleted {n_deleted} individual(s)")
    }

    # Step 5.4: Delete subplot features
    if (n_subplots > 0 && delete_subplots) {
      if (verbose) cli::cli_alert_info("Deleting subplot features...")
      n_deleted <- batch_delete(con, "data_liste_sub_plots", "id_table_liste_plots",
                                plot_ids, "subplot feature(s)", verbose)
      summary$deleted$subplots <- n_deleted
      if (verbose) cli::cli_alert_success("  Deleted {n_deleted} subplot feature(s)")
    }

    # Step 5.5: Delete plots (only if delete_plot = TRUE)
    if (delete_plot) {
      if (verbose) cli::cli_alert_info("Deleting plot(s)...")
      n_deleted <- batch_delete(con, "data_liste_plots", "id_liste_plots",
                                plot_ids, "plot(s)", verbose)
      summary$deleted$plots <- n_deleted
      if (verbose) cli::cli_alert_success("  Deleted {n_deleted} plot(s)")
    } else {
      if (verbose) cli::cli_alert_success("  Plot records preserved (delete_plot = FALSE)")
    }

    summary$success <- TRUE
    if (verbose) cli::cli_alert_success("All deletions completed successfully")

  }, error = function(e) {
    cli::cli_alert_danger("Error occurred during deletion: {e$message}")
    summary$errors <<- c(summary$errors, list(e$message))
  })

  # ===== STEP 6: Summary =====
  if (verbose && isTRUE(summary$success)) {
    cli::cli_h2("Deletion Summary")
    cli::cli_alert_success("Successfully deleted:")
    cli::cli_ul(c(
      if (!is.null(summary$deleted$plots)) "{summary$deleted$plots} plot(s)" else NULL,
      if (!is.null(summary$deleted$individuals)) "{summary$deleted$individuals} individual(s)" else NULL,
      if (!is.null(summary$deleted$trait_measurements)) "{summary$deleted$trait_measurements} trait measurement(s)" else NULL,
      if (!is.null(summary$deleted$measurement_features)) "{summary$deleted$measurement_features} measurement feature(s)" else NULL,
      if (!is.null(summary$deleted$subplots)) "{summary$deleted$subplots} subplot feature(s)" else NULL
    ))
    if (!delete_plot) {
      cli::cli_alert_success("Plot metadata preserved (plot, subplots, subplot features intact)")
    }
  }

  invisible(summary)
}
