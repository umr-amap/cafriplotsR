#' Safely delete individual(s) with all related data
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' This function safely deletes one or more individuals and all related data in the
#' correct order to respect foreign key constraints:
#' 1. Individual measurement features
#' 2. Trait measurements
#' 3. Individuals
#'
#' **Plot metadata is preserved** (plot, subplots, and subplot features remain intact).
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Shows counts of all related data
#' - Requires explicit confirmation (unless force = TRUE)
#' - Uses database transaction (rolls back on error)
#' - Detailed logging of each step
#'
#' @param plot_name Character vector. Plot name(s) to delete individuals from
#' @param individual_ids Integer vector. Specific individual ID(s) to delete (id_n from data_individuals).
#'   If NULL, deletes ALL individuals in the specified plot(s). Default NULL.
#' @param tags Numeric vector. Specific tag numbers to delete. Alternative to individual_ids.
#'   If provided along with plot_name, deletes individuals matching those tags in those plots.
#'   Default NULL.
#' @param con Database connection. If NULL, will connect automatically.
#' @param dry_run Logical. If TRUE, shows what would be deleted without deleting.
#'   Default TRUE for safety.
#' @param force Logical. If TRUE, skips confirmation prompts. Default FALSE.
#'   **USE WITH EXTREME CAUTION!**
#' @param verbose Logical. Show detailed progress? Default TRUE.
#'
#' @return List with deletion summary (invisible)
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # STEP 1: Always do dry-run first!
#' # Delete all individuals in a plot
#' safe_delete_individuals(plot_name = "mbalmayo010", dry_run = TRUE)
#'
#' # Delete specific individuals by ID
#' safe_delete_individuals(individual_ids = c(12345, 12346), dry_run = TRUE)
#'
#' # Delete specific individuals by tag and plot
#' safe_delete_individuals(plot_name = "mbalmayo010", tags = c(1, 2, 3), dry_run = TRUE)
#'
#' # STEP 2: Review the output, then delete if sure
#' safe_delete_individuals(plot_name = "mbalmayo010", dry_run = FALSE)
#'
#' # Delete multiple plots' individuals
#' safe_delete_individuals(plot_name = c("mbalmayo010", "mbalmayo011"))
#' }
#'
#' @export
safe_delete_individuals <- function(plot_name = NULL,
                                    individual_ids = NULL,
                                    tags = NULL,
                                    con = NULL,
                                    dry_run = TRUE,
                                    force = FALSE,
                                    verbose = TRUE) {

  # Validate inputs
  if (is.null(plot_name) && is.null(individual_ids)) {
    stop("Must provide either plot_name or individual_ids", call. = FALSE)
  }

  if (!is.null(plot_name) && !is.null(individual_ids)) {
    cli::cli_alert_warning("Both plot_name and individual_ids provided - using individual_ids only")
    plot_name <- NULL
  }

  if (is.null(con)) {
    con <- call.mydb()
  }

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  # Storage for deletion summary
  summary <- list(
    plot_name = plot_name,
    individual_ids = individual_ids,
    tags = tags,
    dry_run = dry_run,
    deleted = list(),
    errors = list(),
    success = FALSE  # Initialize to FALSE
  )

  # ===== STEP 1: Find individuals to delete =====
  if (verbose) cli::cli_h1("Finding Individuals to Delete")

  # Build WHERE clause based on inputs
  if (!is.null(individual_ids)) {
    # Delete by specific IDs
    where_clause <- sprintf("i.id_n IN (%s)", paste(individual_ids, collapse = ","))
    identifier_desc <- sprintf("%d individual ID(s)", length(individual_ids))

  } else if (!is.null(tags) && !is.null(plot_name)) {
    # Delete by plot + tags
    where_clause <- sprintf(
      "p.plot_name IN (%s) AND i.tag IN (%s)",
      paste0("'", plot_name, "'", collapse = ","),
      paste(tags, collapse = ",")
    )
    identifier_desc <- sprintf("%d tag(s) in %d plot(s)", length(tags), length(plot_name))

  } else {
    # Delete all individuals in plot(s)
    where_clause <- sprintf(
      "p.plot_name IN (%s)",
      paste0("'", plot_name, "'", collapse = ",")
    )
    identifier_desc <- sprintf("all individuals in %d plot(s)", length(plot_name))
  }

  # Get individuals to delete (join with plots table to get plot_name)
  individuals_info <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT i.id_n, p.plot_name, i.tag, i.idtax_n, i.original_tax_name
      FROM data_individuals i
      JOIN data_liste_plots p ON i.id_table_liste_plots_n = p.id_liste_plots
      WHERE %s
    ", where_clause))
  }, error = function(e) {
    stop("Failed to query individuals: ", e$message, call. = FALSE)
  })

  if (nrow(individuals_info) == 0) {
    cli::cli_alert_danger("No individuals found matching criteria: {identifier_desc}")
    return(invisible(summary))
  }

  # Show individuals to be deleted
  if (verbose) {
    cli::cli_h2("Individuals to Delete")
    cli::cli_alert_info("Found {nrow(individuals_info)} individual(s)")

    # Summary by plot
    plot_summary <- individuals_info %>%
      dplyr::group_by(plot_name) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    for (i in 1:nrow(plot_summary)) {
      cli::cli_li("Plot: {plot_summary$plot_name[i]} - {plot_summary$n[i]} individual(s)")
    }

    # Show first few individuals
    if (nrow(individuals_info) <= 10) {
      cat("\n")
      cli::cli_alert_info("Individual details:")
      for (i in 1:nrow(individuals_info)) {
        cli::cli_li("ID: {individuals_info$id_n[i]} | Plot: {individuals_info$plot_name[i]} | Tag: {individuals_info$tag[i]} | {individuals_info$original_tax_name[i]}")
      }
    } else {
      cat("\n")
      cli::cli_alert_info("Showing first 10 of {nrow(individuals_info)} individuals:")
      for (i in 1:10) {
        cli::cli_li("ID: {individuals_info$id_n[i]} | Plot: {individuals_info$plot_name[i]} | Tag: {individuals_info$tag[i]} | {individuals_info$original_tax_name[i]}")
      }
      cli::cli_alert_info("... and {nrow(individuals_info) - 10} more")
    }
  }

  individual_ids_to_delete <- individuals_info$id_n

  # ===== STEP 2: Count related data =====
  if (verbose) cli::cli_h2("Related Data")

  # Count trait measurements
  n_trait_measures <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n
    FROM data_traits_measures
    WHERE id_data_individuals IN (%s)
  ", paste(individual_ids_to_delete, collapse = ",")))$n

  summary$counts$trait_measurements <- n_trait_measures

  if (n_trait_measures > 0) {
    cli::cli_alert_warning("{n_trait_measures} trait measurement(s) will be deleted")

    # Count measurement features
    n_meas_feat <- DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) as n
      FROM data_ind_measures_feat
      WHERE id_trait_measures IN (
        SELECT id_trait_measures FROM data_traits_measures
        WHERE id_data_individuals IN (%s)
      )
    ", paste(individual_ids_to_delete, collapse = ",")))$n

    summary$counts$measurement_features <- n_meas_feat
    if (n_meas_feat > 0) {
      cli::cli_alert_warning("  └─ {n_meas_feat} measurement feature(s)")
    }
  } else {
    cli::cli_alert_success("No trait measurements found")
  }

  # Count specimen links
  n_specimen_links <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n FROM data_link_specimens WHERE id_n IN (%s)
  ", paste(individual_ids_to_delete, collapse = ",")))$n

  summary$counts$specimen_links <- n_specimen_links
  if (n_specimen_links > 0) {
    cli::cli_alert_warning("{n_specimen_links} specimen link(s) will be deleted")
  }

  # ===== STEP 3: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN - nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    cli::cli_alert_success("Plot metadata will be preserved (plot, subplots, subplot features)")
    return(invisible(summary))
  }

  # ===== STEP 4: Confirmation =====
  if (!force) {
    cli::cli_h2("⚠️  CONFIRMATION REQUIRED ⚠️")
    cli::cli_alert_danger("You are about to PERMANENTLY delete:")
    cli::cli_ul(c(
      "{nrow(individuals_info)} individual(s)",
      if (n_trait_measures > 0) "{n_trait_measures} trait measurement(s)" else NULL,
      if (n_meas_feat > 0) "{n_meas_feat} measurement feature(s)" else NULL,
      if (n_specimen_links > 0) "{n_specimen_links} specimen link(s)" else NULL
    ))
    cli::cli_alert_success("Plot metadata will be PRESERVED (not deleted)")

    confirm <- choose_prompt(message = "Are you ABSOLUTELY SURE you want to delete these individuals?")

    if (!confirm) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 5: Delete in correct order =====
  cli::cli_h2("Deleting Data")

  # Step 5.1: Get trait measurement IDs first
  trait_measure_ids <- NULL
  if (n_trait_measures > 0) {
    if (verbose) cli::cli_alert_info("Getting trait measurement IDs...")
    trait_measure_ids <- tryCatch({
      DBI::dbGetQuery(con, sprintf("
        SELECT id_trait_measures
        FROM data_traits_measures
        WHERE id_data_individuals IN (%s)
      ", paste(individual_ids_to_delete, collapse = ",")))$id_trait_measures
    }, error = function(e) {
      cli::cli_alert_danger("Failed to get trait measurement IDs: {e$message}")
      summary$success <- FALSE
      summary$errors <- c(summary$errors, list(e$message))
      return(invisible(summary))
    })

    if (verbose) cli::cli_alert_success("  ✔ Found {length(trait_measure_ids)} trait measurement(s)")
  }

  # Step 5.2: Delete measurement features (separate transaction)
  if (n_meas_feat > 0 && !is.null(trait_measure_ids)) {
    if (verbose) cli::cli_alert_info("Deleting measurement features in batches...")

    tryCatch({
      DBI::dbBegin(con)

      batch_size <- 5000
      unique_trait_ids <- unique(trait_measure_ids)
      n_batches <- ceiling(length(unique_trait_ids) / batch_size)
      total_deleted_features <- 0

      for (batch_idx in 1:n_batches) {
        start_idx <- (batch_idx - 1) * batch_size + 1
        end_idx <- min(batch_idx * batch_size, length(unique_trait_ids))
        batch_ids <- unique_trait_ids[start_idx:end_idx]

        query <- sprintf(
          "DELETE FROM data_ind_measures_feat WHERE id_trait_measures IN (%s)",
          paste(batch_ids, collapse = ",")
        )

        rs <- DBI::dbSendQuery(con, query)
        n_deleted <- DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)

        total_deleted_features <- total_deleted_features + n_deleted

        if (verbose && n_batches > 1) {
          cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_deleted} measurement feature(s)")
        }
      }

      DBI::dbCommit(con)
      summary$deleted$measurement_features <- total_deleted_features
      if (verbose) cli::cli_alert_success("  ✔ Deleted {total_deleted_features} measurement feature(s) total")

    }, error = function(e) {
      DBI::dbRollback(con)
      cli::cli_alert_danger("❌ Error deleting measurement features: {e$message}")
      summary$success <- FALSE
      summary$errors <- c(summary$errors, list(e$message))
      return(invisible(summary))
    })
  }

  # Step 5.3: Delete trait measurements (separate transaction)
  if (n_trait_measures > 0 && !is.null(trait_measure_ids)) {
    if (verbose) cli::cli_alert_info("Deleting trait measurements in batches...")

    tryCatch({
      DBI::dbBegin(con)

      batch_size <- 5000
      unique_trait_ids <- unique(trait_measure_ids)
      n_batches <- ceiling(length(unique_trait_ids) / batch_size)
      total_deleted_measures <- 0

      for (batch_idx in 1:n_batches) {
        start_idx <- (batch_idx - 1) * batch_size + 1
        end_idx <- min(batch_idx * batch_size, length(unique_trait_ids))
        batch_ids <- unique_trait_ids[start_idx:end_idx]

        query <- sprintf(
          "DELETE FROM data_traits_measures WHERE id_trait_measures IN (%s)",
          paste(batch_ids, collapse = ",")
        )

        rs <- DBI::dbSendQuery(con, query)
        n_deleted <- DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)

        total_deleted_measures <- total_deleted_measures + n_deleted

        if (verbose && n_batches > 1) {
          cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_deleted} measurement(s)")
        }
      }

      DBI::dbCommit(con)
      summary$deleted$trait_measurements <- total_deleted_measures
      if (verbose) cli::cli_alert_success("  ✔ Deleted {total_deleted_measures} trait measurement(s) total")

    }, error = function(e) {
      DBI::dbRollback(con)
      cli::cli_alert_danger("❌ Error deleting trait measurements: {e$message}")
      summary$success <- FALSE
      summary$errors <- c(summary$errors, list(e$message))
      return(invisible(summary))
    })
  }

  # Step 5.3b: Delete specimen links (data_link_specimens) before individuals
  n_specimen_links <- DBI::dbGetQuery(con, sprintf("
    SELECT COUNT(*) as n FROM data_link_specimens WHERE id_n IN (%s)
  ", paste(individual_ids_to_delete, collapse = ",")))$n

  if (n_specimen_links > 0) {
    if (verbose) cli::cli_alert_info("Deleting {n_specimen_links} specimen link(s)...")

    tryCatch({
      DBI::dbBegin(con)
      batch_size_links <- 5000
      unique_ind_ids <- unique(individual_ids_to_delete)
      n_batches_links <- ceiling(length(unique_ind_ids) / batch_size_links)
      total_deleted_links <- 0

      for (batch_idx in seq_len(n_batches_links)) {
        start_idx <- (batch_idx - 1) * batch_size_links + 1
        end_idx <- min(batch_idx * batch_size_links, length(unique_ind_ids))
        batch_ids <- unique_ind_ids[start_idx:end_idx]

        rs <- DBI::dbSendQuery(con, sprintf(
          "DELETE FROM data_link_specimens WHERE id_n IN (%s)",
          paste(batch_ids, collapse = ",")
        ))
        total_deleted_links <- total_deleted_links + DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)
      }

      DBI::dbCommit(con)
      summary$deleted$specimen_links <- total_deleted_links
      if (verbose) cli::cli_alert_success("  ✔ Deleted {total_deleted_links} specimen link(s)")

    }, error = function(e) {
      DBI::dbRollback(con)
      cli::cli_alert_danger("❌ Error deleting specimen links: {e$message}")
      summary$success <- FALSE
      summary$errors <- c(summary$errors, list(e$message))
      return(invisible(summary))
    })
  }

  # Step 5.4: Delete individuals (separate transaction per batch to avoid timeout)
  if (verbose) cli::cli_alert_info("Deleting individuals in batches...")

  batch_size <- 1000  # Smaller batches to avoid FK check timeout
  unique_ids <- unique(individual_ids_to_delete)
  n_batches <- ceiling(length(unique_ids) / batch_size)
  total_deleted_indiv <- 0

  batch_error <- FALSE
  for (batch_idx in 1:n_batches) {
    if (batch_error) break
    tryCatch({
      # Separate transaction for each batch
      DBI::dbBegin(con)

      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx <- min(batch_idx * batch_size, length(unique_ids))
      batch_ids <- unique_ids[start_idx:end_idx]

      query <- sprintf(
        "DELETE FROM data_individuals WHERE id_n IN (%s)",
        paste(batch_ids, collapse = ",")
      )

      rs <- DBI::dbSendQuery(con, query)
      n_deleted <- DBI::dbGetRowsAffected(rs)
      DBI::dbClearResult(rs)

      DBI::dbCommit(con)

      total_deleted_indiv <- total_deleted_indiv + n_deleted

      if (verbose) {
        cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_deleted} individual(s)")
      }

      # Small delay between batches to prevent connection issues
      if (batch_idx < n_batches) {
        Sys.sleep(0.1)
      }

    }, error = function(e) {
      tryCatch(DBI::dbRollback(con), error = function(e2) {})
      cli::cli_alert_danger("❌ Error deleting individuals batch {batch_idx}: {e$message}")
      summary$success <- FALSE
      summary$errors <- c(summary$errors, list(paste0("Batch ", batch_idx, ": ", e$message)))
      batch_error <<- TRUE  # Stop on first error
    })
  }

  if (total_deleted_indiv == length(unique_ids)) {
    summary$deleted$individuals <- total_deleted_indiv
    if (verbose) cli::cli_alert_success("  ✔ Deleted {total_deleted_indiv} individual(s) total")
    if (verbose) cli::cli_alert_success("✅ All deletions completed successfully")
    summary$success <- TRUE
  } else {
    cli::cli_alert_warning("Partial deletion: {total_deleted_indiv}/{length(unique_ids)} individuals deleted")
    summary$deleted$individuals <- total_deleted_indiv
  }

  # ===== STEP 6: Summary =====
  if (verbose && summary$success) {
    cli::cli_h2("Deletion Summary")
    cli::cli_alert_success("Successfully deleted:")
    cli::cli_ul(c(
      "{summary$deleted$individuals} individual(s)",
      if (!is.null(summary$deleted$trait_measurements)) "{summary$deleted$trait_measurements} trait measurement(s)" else NULL,
      if (!is.null(summary$deleted$measurement_features)) "{summary$deleted$measurement_features} measurement feature(s)" else NULL,
      if (!is.null(summary$deleted$specimen_links)) "{summary$deleted$specimen_links} specimen link(s)" else NULL
    ))
    cli::cli_alert_success("Plot metadata preserved (plot, subplots, subplot features intact)")
  }

  invisible(summary)
}
