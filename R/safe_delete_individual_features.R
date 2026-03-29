#' Safely delete individual feature measurements with all related data
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' This function safely deletes individual feature measurements (records in
#' `data_traits_measures`) and their associated sub-features (`data_ind_measures_feat`)
#' in the correct order to respect foreign key constraints:
#' 1. Measurement features (`data_ind_measures_feat`)
#' 2. Trait measurements (`data_traits_measures`)
#'
#' **Individuals are preserved** — only the feature measurements are removed.
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Shows counts of all related data
#' - Requires explicit confirmation (unless force = TRUE)
#' - Uses database transactions (rollback on error)
#' - Detailed logging of each step
#'
#' @param id_trait_measures Integer vector. Specific measurement ID(s) to delete
#'   (`id_trait_measures` from `data_traits_measures`). Takes precedence over
#'   `individual_ids` / `trait_ids`. Typically obtained from
#'   \code{query_individual_features(..., include_measurement_ids = TRUE)}.
#'   Default NULL.
#' @param individual_ids Integer vector. Delete all feature measurements belonging
#'   to these individual ID(s) (`id_n` from `data_individuals`). Can be combined
#'   with `trait_ids`. Default NULL.
#' @param trait_ids Integer vector. Filter measurements to these trait ID(s)
#'   (`id_trait` from `traitlist`). Only used when `individual_ids` is provided.
#'   Default NULL (all traits).
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
#' # STEP 1: Extract features and pick the ones to remove
#' feats <- query_individual_features(
#'   plot_ids = 42,
#'   include_measurement_ids = TRUE,
#'   format = "long"
#' )
#' ids_to_remove <- feats$id_trait_measures[feats$trait == "dbh" & feats$traitvalue < 0]
#'
#' # STEP 2: Always do a dry-run first!
#' safe_delete_individual_features(id_trait_measures = ids_to_remove, dry_run = TRUE)
#'
#' # STEP 3: Review output, then delete if sure
#' safe_delete_individual_features(id_trait_measures = ids_to_remove, dry_run = FALSE)
#'
#' # Alternative: delete all features of specific trait(s) for given individuals
#' safe_delete_individual_features(
#'   individual_ids = c(1001, 1002),
#'   trait_ids = c(5),
#'   dry_run = TRUE
#' )
#' }
#'
#' @export
safe_delete_individual_features <- function(id_trait_measures = NULL,
                                            individual_ids = NULL,
                                            trait_ids = NULL,
                                            con = NULL,
                                            dry_run = TRUE,
                                            force = FALSE,
                                            verbose = TRUE) {

  # --- Input validation ---
  if (is.null(id_trait_measures) && is.null(individual_ids)) {
    stop(
      "Must provide at least one of: id_trait_measures or individual_ids",
      call. = FALSE
    )
  }

  if (is.null(con)) {
    con <- call.mydb()
  }

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  summary <- list(
    id_trait_measures = id_trait_measures,
    individual_ids    = individual_ids,
    trait_ids         = trait_ids,
    dry_run           = dry_run,
    counts            = list(),
    deleted           = list(),
    errors            = list(),
    success           = FALSE
  )

  # ===== STEP 1: Resolve measurement IDs to delete =====
  if (verbose) cli::cli_h1("Finding Individual Feature Measurements to Delete")

  if (!is.null(id_trait_measures)) {
    # Direct specification takes precedence
    measure_ids_to_delete <- unique(id_trait_measures)
    identifier_desc <- sprintf("%d specific measurement record(s)", length(measure_ids_to_delete))

    measures_info <- tryCatch({
      DBI::dbGetQuery(con, sprintf("
        SELECT tm.id_trait_measures,
               tm.id_data_individuals,
               tl.trait,
               tm.traitvalue,
               tm.traitvalue_char
        FROM data_traits_measures tm
        LEFT JOIN traitlist tl ON tm.traitid = tl.id_trait
        WHERE tm.id_trait_measures IN (%s)
      ", paste(measure_ids_to_delete, collapse = ",")))
    }, error = function(e) {
      stop("Failed to query trait measurements: ", e$message, call. = FALSE)
    })

  } else {
    # Build WHERE clause from individual_ids + optional trait_ids
    conditions <- sprintf(
      "tm.id_data_individuals IN (%s)",
      paste(individual_ids, collapse = ",")
    )

    if (!is.null(trait_ids)) {
      conditions <- paste(conditions, sprintf(
        "AND tl.id_trait IN (%s)", paste(trait_ids, collapse = ",")
      ))
    }

    parts <- sprintf("%d individual(s)", length(individual_ids))
    if (!is.null(trait_ids)) {
      parts <- paste(parts, sprintf("and %d trait(s)", length(trait_ids)))
    }
    identifier_desc <- parts

    measures_info <- tryCatch({
      DBI::dbGetQuery(con, sprintf("
        SELECT tm.id_trait_measures,
               tm.id_data_individuals,
               tl.trait,
               tm.traitvalue,
               tm.traitvalue_char
        FROM data_traits_measures tm
        LEFT JOIN traitlist tl ON tm.traitid = tl.id_trait
        WHERE %s
      ", conditions))
    }, error = function(e) {
      stop("Failed to query trait measurements: ", e$message, call. = FALSE)
    })

    measure_ids_to_delete <- unique(measures_info$id_trait_measures)
  }

  if (length(measure_ids_to_delete) == 0 || nrow(measures_info) == 0) {
    cli::cli_alert_danger("No individual feature measurements found matching criteria: {identifier_desc}")
    return(invisible(summary))
  }

  summary$counts$trait_measurements <- length(measure_ids_to_delete)

  if (verbose) {
    cli::cli_h2("Feature Measurements to Delete")
    cli::cli_alert_info(
      "Found {length(measure_ids_to_delete)} measurement record(s) for {length(unique(measures_info$id_data_individuals))} individual(s)"
    )

    n_show <- min(nrow(measures_info), 10)
    cli::cli_alert_info("Showing first {n_show} records:")
    for (i in seq_len(n_show)) {
      row <- measures_info[i, ]
      val <- if (!is.na(row$traitvalue)) row$traitvalue else row$traitvalue_char
      cli::cli_li(
        "id_measures: {row$id_trait_measures} | individual: {row$id_data_individuals} | trait: {row$trait} | value: {val}"
      )
    }
    if (nrow(measures_info) > 10) {
      cli::cli_alert_info("... and {nrow(measures_info) - 10} more")
    }
  }

  # ===== STEP 2: Count related sub-features =====
  if (verbose) cli::cli_h2("Related Data")

  n_feat <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) AS n
      FROM data_ind_measures_feat
      WHERE id_trait_measures IN (%s)
    ", paste(measure_ids_to_delete, collapse = ",")))$n
  }, error = function(e) {
    cli::cli_alert_warning("Could not count measurement sub-features: {e$message}")
    0L
  })

  summary$counts$measurement_features <- n_feat

  if (n_feat > 0) {
    if (verbose) cli::cli_alert_warning("{n_feat} measurement sub-feature(s) will also be deleted")
  } else {
    if (verbose) cli::cli_alert_success("No measurement sub-features found")
  }

  # ===== STEP 3: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN - nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    cli::cli_alert_success("Individuals are preserved (only their feature measurements would be removed)")
    return(invisible(summary))
  }

  # ===== STEP 4: Confirmation =====
  if (!force) {
    cli::cli_h2("WARNING: CONFIRMATION REQUIRED")
    cli::cli_alert_danger("You are about to PERMANENTLY delete:")
    lines <- sprintf("%d individual feature measurement record(s)", length(measure_ids_to_delete))
    if (n_feat > 0) lines <- c(lines, sprintf("%d measurement sub-feature(s)", as.integer(n_feat)))
    cli::cli_ul(lines)
    cli::cli_alert_success("Individuals will be PRESERVED (only their feature measurements are deleted)")

    confirm <- choose_prompt(
      message = "Are you ABSOLUTELY SURE you want to delete these individual feature measurements?"
    )

    if (!confirm) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 5: Delete in correct order =====
  cli::cli_h2("Deleting Data")

  # Handle Pool connections: checkout a raw connection for transaction work
  is_pool <- inherits(con, "Pool")
  if (is_pool) {
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)
  } else {
    raw_con <- con
  }

  batch_size <- 5000
  unique_measure_ids <- unique(measure_ids_to_delete)
  n_batches <- ceiling(length(unique_measure_ids) / batch_size)

  # Step 5.1: Delete sub-features first (respect FK constraint)
  if (n_feat > 0) {
    if (verbose) cli::cli_alert_info("Deleting measurement sub-features...")

    total_deleted_feat <- 0L

    for (batch_idx in seq_len(n_batches)) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx   <- min(batch_idx * batch_size, length(unique_measure_ids))
      batch_ids <- unique_measure_ids[start_idx:end_idx]

      tryCatch({
        DBI::dbBegin(raw_con)
        rs <- DBI::dbSendQuery(raw_con, sprintf(
          "DELETE FROM data_ind_measures_feat WHERE id_trait_measures IN (%s)",
          paste(batch_ids, collapse = ",")
        ))
        n_del <- DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)
        DBI::dbCommit(raw_con)

        total_deleted_feat <- total_deleted_feat + n_del

        if (verbose && n_batches > 1) {
          cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_del} sub-feature(s)")
        }
      }, error = function(e) {
        tryCatch(DBI::dbRollback(raw_con), error = function(e2) {})
        cli::cli_alert_danger("Error deleting measurement sub-features (batch {batch_idx}): {e$message}")
        summary$success <- FALSE
        summary$errors  <- c(summary$errors, list(e$message))
      })
    }

    summary$deleted$measurement_features <- total_deleted_feat
    if (verbose) cli::cli_alert_success("  Deleted {total_deleted_feat} measurement sub-feature(s) total")
  }

  # Step 5.2: Delete trait measurements
  if (verbose) cli::cli_alert_info("Deleting individual feature measurements...")

  total_deleted_measures <- 0L

  for (batch_idx in seq_len(n_batches)) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx   <- min(batch_idx * batch_size, length(unique_measure_ids))
    batch_ids <- unique_measure_ids[start_idx:end_idx]

    tryCatch({
      DBI::dbBegin(raw_con)
      rs <- DBI::dbSendQuery(raw_con, sprintf(
        "DELETE FROM data_traits_measures WHERE id_trait_measures IN (%s)",
        paste(batch_ids, collapse = ",")
      ))
      n_del <- DBI::dbGetRowsAffected(rs)
      DBI::dbClearResult(rs)
      DBI::dbCommit(raw_con)

      total_deleted_measures <- total_deleted_measures + n_del

      if (verbose && n_batches > 1) {
        cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_del} measurement(s)")
      }
    }, error = function(e) {
      tryCatch(DBI::dbRollback(raw_con), error = function(e2) {})
      cli::cli_alert_danger("Error deleting feature measurements (batch {batch_idx}): {e$message}")
      summary$success <- FALSE
      summary$errors  <- c(summary$errors, list(e$message))
    })
  }

  summary$deleted$trait_measurements <- total_deleted_measures

  # ===== STEP 6: Summary =====
  if (total_deleted_measures == length(unique_measure_ids)) {
    summary$success <- TRUE
    if (verbose) {
      cli::cli_h2("Deletion Summary")
      cli::cli_alert_success("Successfully deleted:")
      lines <- sprintf("%d individual feature measurement record(s)", total_deleted_measures)
      if (!is.null(summary$deleted$measurement_features)) {
        lines <- c(lines, sprintf("%d measurement sub-feature(s)", as.integer(summary$deleted$measurement_features)))
      }
      cli::cli_ul(lines)
      cli::cli_alert_success("Individuals preserved (individuals table not modified)")
    }
  } else {
    cli::cli_alert_warning(
      "Partial deletion: {total_deleted_measures}/{length(unique_measure_ids)} measurements deleted"
    )
  }

  invisible(summary)
}
