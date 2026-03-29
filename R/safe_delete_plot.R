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
#' - Processes plots one-by-one (or in small batches) to avoid memory crashes
#'   with large datasets
#' - Uses per-batch transactions (rolls back each batch on error)
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
#' @param plot_batch_size Integer. Number of plots processed per iteration.
#'   Reduce to 1 (default) for very large plots to avoid memory/query size issues.
#' @param row_batch_size Integer. Number of rows deleted per SQL statement within
#'   each plot batch. Default 2000.
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
#' # Delete multiple plots (processed one by one by default)
#' safe_delete_plot(plot_ids = c(123, 124, 125))
#'
#' # Process 5 plots at a time (faster for many small plots)
#' safe_delete_plot(plot_ids = c(123, 124, 125), plot_batch_size = 5)
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
                             plot_batch_size = 1L,
                             row_batch_size = 2000L,
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

  plot_batch_size <- max(1L, as.integer(plot_batch_size))
  row_batch_size  <- max(100L, as.integer(row_batch_size))

  # Storage for deletion summary
  summary <- list(
    plot_ids = plot_ids,
    dry_run = dry_run,
    deleted = list(
      specimen_links       = 0L,
      measurement_features = 0L,
      trait_measurements   = 0L,
      individuals          = 0L,
      subplots             = 0L,
      plots                = 0L
    ),
    errors = list()
  )

  # ===== STEP 1: Check what exists and will be deleted =====
  if (verbose) cli::cli_h1("Analyzing Plot Deletion")

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

  # Only process IDs that actually exist
  valid_plot_ids <- plots_info$id_liste_plots

  if (verbose) {
    cli::cli_h2("Plots to Delete")
    cli::cli_alert_info("Found {nrow(plots_info)} plot(s):")
    for (i in seq_len(nrow(plots_info))) {
      cli::cli_li("ID: {plots_info$id_liste_plots[i]} | Name: {plots_info$plot_name[i]}")
    }
  }

  # ===== STEP 2: Count related data =====
  if (verbose) cli::cli_h2("Related Data")

  ids_sql <- paste(valid_plot_ids, collapse = ",")

  n_individuals <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n FROM data_individuals
    WHERE id_table_liste_plots_n IN (%s)
  ", ids_sql))$n

  summary$counts$individuals <- n_individuals

  if (n_individuals > 0) {
    cli::cli_alert_warning("{n_individuals} individual(s) will be deleted")

    n_trait_measures <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) as n FROM data_traits_measures
      WHERE id_data_individuals IN (
        SELECT id_n FROM data_individuals
        WHERE id_table_liste_plots_n IN (%s)
      )
    ", ids_sql))$n

    summary$counts$trait_measurements <- n_trait_measures
    if (n_trait_measures > 0) {
      cli::cli_alert_warning("  \u2514\u2500 {n_trait_measures} trait measurement(s)")
    }

    n_meas_feat <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) as n FROM data_ind_measures_feat
      WHERE id_trait_measures IN (
        SELECT id_trait_measures FROM data_traits_measures
        WHERE id_data_individuals IN (
          SELECT id_n FROM data_individuals
          WHERE id_table_liste_plots_n IN (%s)
        )
      )
    ", ids_sql))$n

    summary$counts$measurement_features <- n_meas_feat
    if (n_meas_feat > 0) {
      cli::cli_alert_warning("     \u2514\u2500 {n_meas_feat} measurement feature(s)")
    }
  } else {
    cli::cli_alert_success("No individuals found")
    n_trait_measures <- 0L
    n_meas_feat      <- 0L
  }

  n_subplots <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n FROM data_liste_sub_plots
    WHERE id_table_liste_plots IN (%s)
  ", ids_sql))$n

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
    if (!delete_plot)    cli::cli_alert_success("Plot metadata will be preserved (delete_plot = FALSE)")
    if (!delete_subplots) cli::cli_alert_success("Subplot features will be preserved (delete_subplots = FALSE)")
    return(invisible(summary))
  }

  # ===== STEP 4: Confirmation =====
  if (!force) {
    cli::cli_h2("\u26a0\ufe0f  CONFIRMATION REQUIRED \u26a0\ufe0f")
    cli::cli_alert_danger("You are about to PERMANENTLY delete:")
    cli::cli_ul(c(
      if (delete_plot)                                      "{nrow(plots_info)} plot(s)" else NULL,
      if (n_individuals > 0 && delete_individuals)          "{n_individuals} individual(s)" else NULL,
      if (n_trait_measures > 0 && delete_individuals)       "{n_trait_measures} trait measurement(s)" else NULL,
      if (n_meas_feat > 0 && delete_individuals)            "{n_meas_feat} measurement feature(s)" else NULL,
      if (n_subplots > 0 && delete_subplots)                "{n_subplots} subplot feature(s)" else NULL
    ))
    if (!delete_plot) cli::cli_alert_success("Plot metadata will be PRESERVED (not deleted)")

    confirm <- choose_prompt(message = "Are you ABSOLUTELY SURE you want to delete this data?")
    if (!confirm) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 5: Delete plot-by-plot (or in plot batches) =====
  cli::cli_h2("Deleting Data")
  summary$success <- FALSE

  # Handle Pool connections: checkout a raw connection for transaction work
  is_pool <- inherits(con, "Pool")
  if (is_pool) {
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)
  } else {
    raw_con <- con
  }

  # Helper: batch-delete by explicit ID list
  batch_delete_ids <- function(raw_con, table, id_column, ids, label) {
    if (length(ids) == 0) return(0L)
    total    <- 0L
    n_batch  <- ceiling(length(ids) / row_batch_size)
    for (bi in seq_len(n_batch)) {
      s   <- (bi - 1L) * row_batch_size + 1L
      e   <- min(bi * row_batch_size, length(ids))
      bids <- ids[s:e]
      DBI::dbBegin(raw_con)
      tryCatch({
        rs    <- DBI::dbSendQuery(raw_con, sprintf(
          "DELETE FROM %s WHERE %s IN (%s)",
          table, id_column, paste(bids, collapse = ",")
        ))
        n_del <- DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)
        DBI::dbCommit(raw_con)
        total <- total + n_del
        if (verbose && n_batch > 1) {
          cli::cli_alert_info("    Batch {bi}/{n_batch}: deleted {n_del} {label}")
        }
      }, error = function(e) {
        tryCatch(DBI::dbRollback(raw_con), error = function(e2) {})
        stop(sprintf("Error deleting %s (batch %d/%d): %s",
                     label, bi, n_batch, e$message), call. = FALSE)
      })
    }
    total
  }

  # Split valid plot IDs into plot-level batches
  plot_batches <- split(
    valid_plot_ids,
    ceiling(seq_along(valid_plot_ids) / plot_batch_size)
  )
  n_plot_batches <- length(plot_batches)

  tryCatch({

    for (pb_idx in seq_along(plot_batches)) {

      pb_ids     <- plot_batches[[pb_idx]]
      pb_ids_sql <- paste(pb_ids, collapse = ",")

      if (verbose && n_plot_batches > 1) {
        cli::cli_alert_info(
          "Plot batch {pb_idx}/{n_plot_batches} (plot IDs: {pb_ids_sql})"
        )
      }

      # ---- Resolve individual IDs for this plot batch ----
      ind_ids <- integer(0)
      tm_ids  <- integer(0)

      if (delete_individuals && n_individuals > 0) {
        ind_ids <- DBI::dbGetQuery(raw_con, sprintf("
          SELECT id_n FROM data_individuals
          WHERE id_table_liste_plots_n IN (%s)
        ", pb_ids_sql))$id_n
      }

      if (length(ind_ids) > 0) {
        tm_ids <- DBI::dbGetQuery(raw_con, sprintf("
          SELECT id_trait_measures FROM data_traits_measures
          WHERE id_data_individuals IN (%s)
        ", paste(ind_ids, collapse = ",")))$id_trait_measures
      }

      # ---- 5.0 Specimen links ----
      if (length(ind_ids) > 0) {
        n_sl <- DBI::dbGetQuery(raw_con, sprintf("
          SELECT COUNT(*) as n FROM data_link_specimens WHERE id_n IN (%s)
        ", paste(ind_ids, collapse = ",")))$n
        if (n_sl > 0) {
          if (verbose) cli::cli_alert_info("Deleting {n_sl} specimen link(s)...")
          n_del <- batch_delete_ids(raw_con, "data_link_specimens", "id_n",
                                    ind_ids, "specimen link(s)")
          summary$deleted$specimen_links <- summary$deleted$specimen_links + n_del
          if (verbose) cli::cli_alert_success("  Deleted {n_del} specimen link(s)")
        }
      }

      # ---- 5.1 Measurement features (by trait_measure_ids) ----
      if (length(tm_ids) > 0) {
        if (verbose) cli::cli_alert_info("Deleting measurement features...")
        n_del <- batch_delete_ids(raw_con, "data_ind_measures_feat", "id_trait_measures",
                                  tm_ids, "measurement feature(s)")
        summary$deleted$measurement_features <- summary$deleted$measurement_features + n_del
        if (verbose) cli::cli_alert_success("  Deleted {n_del} measurement feature(s)")
      }

      # ---- 5.2 Trait measurements (by individual_ids) ----
      if (length(ind_ids) > 0 && n_trait_measures > 0) {
        if (verbose) cli::cli_alert_info("Deleting trait measurements...")
        n_del <- batch_delete_ids(raw_con, "data_traits_measures", "id_data_individuals",
                                  ind_ids, "trait measurement(s)")
        summary$deleted$trait_measurements <- summary$deleted$trait_measurements + n_del
        if (verbose) cli::cli_alert_success("  Deleted {n_del} trait measurement(s)")
      }

      # ---- 5.3 Individuals ----
      if (length(ind_ids) > 0 && delete_individuals) {
        if (verbose) cli::cli_alert_info("Deleting individuals...")
        n_del <- batch_delete_ids(raw_con, "data_individuals", "id_n",
                                  ind_ids, "individual(s)")
        summary$deleted$individuals <- summary$deleted$individuals + n_del
        if (verbose) cli::cli_alert_success("  Deleted {n_del} individual(s)")
      }

      # ---- 5.4 Subplot features ----
      if (n_subplots > 0 && delete_subplots) {
        if (verbose) cli::cli_alert_info("Deleting subplot features...")
        n_del <- batch_delete_ids(raw_con, "data_liste_sub_plots", "id_table_liste_plots",
                                  pb_ids, "subplot feature(s)")
        summary$deleted$subplots <- summary$deleted$subplots + n_del
        if (verbose) cli::cli_alert_success("  Deleted {n_del} subplot feature(s)")
      }

      # ---- 5.5 Plots ----
      if (delete_plot) {
        if (verbose) cli::cli_alert_info("Deleting plot(s)...")
        n_del <- batch_delete_ids(raw_con, "data_liste_plots", "id_liste_plots",
                                  pb_ids, "plot(s)")
        summary$deleted$plots <- summary$deleted$plots + n_del
        if (verbose) cli::cli_alert_success("  Deleted {n_del} plot(s)")
      } else {
        if (verbose) cli::cli_alert_success("  Plot records preserved (delete_plot = FALSE)")
      }

    } # end plot batch loop

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
    d <- summary$deleted
    cli::cli_ul(c(
      if (d$plots                > 0) "{d$plots} plot(s)" else NULL,
      if (d$individuals          > 0) "{d$individuals} individual(s)" else NULL,
      if (d$trait_measurements   > 0) "{d$trait_measurements} trait measurement(s)" else NULL,
      if (d$measurement_features > 0) "{d$measurement_features} measurement feature(s)" else NULL,
      if (d$subplots             > 0) "{d$subplots} subplot feature(s)" else NULL,
      if (d$specimen_links       > 0) "{d$specimen_links} specimen link(s)" else NULL
    ))
    if (!delete_plot) {
      cli::cli_alert_success("Plot metadata preserved (plot, subplots, subplot features intact)")
    }
  }

  invisible(summary)
}
