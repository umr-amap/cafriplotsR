#' Safely delete taxa trait measurements with all related data
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' This function safely deletes taxa trait measurements and all related data in the
#' correct order to respect foreign key constraints:
#' 1. Trait measurement features (`taxa_traits_measures_feat`)
#' 2. Trait measurements (`taxa_traits_measures`)
#'
#' **Taxonomy is preserved** (taxa entries in the taxonomy database are not touched).
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Shows counts of all related data
#' - Requires explicit confirmation (unless force = TRUE)
#' - Uses database transactions (rollback on error)
#' - Detailed logging of each step
#'
#' @param idtax Integer vector. Taxon ID(s) whose trait measurements will be deleted.
#'   If NULL, `trait_ids` or `id_trait_measures` must be provided.
#' @param trait_ids Integer vector. Trait ID(s) to filter measurements (id_trait from traitlist).
#'   Can be combined with `idtax` to delete only specific traits for specific taxa.
#'   Default NULL (all traits).
#' @param id_trait_measures Integer vector. Specific measurement ID(s) to delete
#'   (id_trait_measures from taxa_traits_measures). Takes precedence over `idtax`/`trait_ids`.
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
#' # STEP 1: Always do a dry-run first!
#'
#' # Preview deletion of all trait measurements for a taxon
#' safe_delete_taxa_traits(idtax = 12345, dry_run = TRUE)
#'
#' # Preview deletion of specific traits for a taxon
#' safe_delete_taxa_traits(idtax = 12345, trait_ids = c(1, 2), dry_run = TRUE)
#'
#' # Preview deletion of specific measurement records
#' safe_delete_taxa_traits(id_trait_measures = c(999, 1000), dry_run = TRUE)
#'
#' # STEP 2: Review the output, then delete if sure
#' safe_delete_taxa_traits(idtax = 12345, dry_run = FALSE)
#' }
#'
#' @export
safe_delete_taxa_traits <- function(idtax = NULL,
                                    trait_ids = NULL,
                                    id_trait_measures = NULL,
                                    con = NULL,
                                    dry_run = TRUE,
                                    force = FALSE,
                                    verbose = TRUE) {

  # --- Input validation ---
  if (is.null(idtax) && is.null(trait_ids) && is.null(id_trait_measures)) {
    stop(
      "Must provide at least one of: idtax, trait_ids, or id_trait_measures",
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
    idtax            = idtax,
    trait_ids        = trait_ids,
    id_trait_measures = id_trait_measures,
    dry_run          = dry_run,
    counts           = list(),
    deleted          = list(),
    errors           = list(),
    success          = FALSE
  )

  # ===== STEP 1: Resolve measurement IDs to delete =====
  if (verbose) cli::cli_h1("Finding Taxa Trait Measurements to Delete")

  if (!is.null(id_trait_measures)) {
    # Direct specification takes precedence
    measure_ids_to_delete <- unique(id_trait_measures)
    identifier_desc <- sprintf("%d specific measurement record(s)", length(measure_ids_to_delete))

    # Retrieve info for display
    measures_info <- tryCatch({
      DBI::dbGetQuery(con, sprintf("
        SELECT tm.id_trait_measures, tm.idtax, tl.trait, tm.traitvalue, tm.traitvalue_char
        FROM taxa_traits_measures tm
        LEFT JOIN traitlist tl ON tm.fk_id_trait = tl.id_trait
        WHERE tm.id_trait_measures IN (%s)
      ", paste(measure_ids_to_delete, collapse = ",")))
    }, error = function(e) {
      stop("Failed to query trait measurements: ", e$message, call. = FALSE)
    })

  } else {
    # Build WHERE from idtax / trait_ids
    conditions <- "1=1"

    if (!is.null(idtax)) {
      conditions <- paste(conditions, sprintf(
        "AND tm.idtax IN (%s)", paste(idtax, collapse = ",")
      ))
    }
    if (!is.null(trait_ids)) {
      conditions <- paste(conditions, sprintf(
        "AND tl.id_trait IN (%s)", paste(trait_ids, collapse = ",")
      ))
    }

    parts <- c()
    if (!is.null(idtax))    parts <- c(parts, sprintf("%d taxon/taxa", length(idtax)))
    if (!is.null(trait_ids)) parts <- c(parts, sprintf("%d trait(s)", length(trait_ids)))
    identifier_desc <- paste(parts, collapse = " and ")

    measures_info <- tryCatch({
      DBI::dbGetQuery(con, sprintf("
        SELECT tm.id_trait_measures, tm.idtax, tl.trait, tm.traitvalue, tm.traitvalue_char
        FROM taxa_traits_measures tm
        LEFT JOIN traitlist tl ON tm.fk_id_trait = tl.id_trait
        WHERE %s
      ", conditions))
    }, error = function(e) {
      stop("Failed to query trait measurements: ", e$message, call. = FALSE)
    })

    measure_ids_to_delete <- unique(measures_info$id_trait_measures)
  }

  if (length(measure_ids_to_delete) == 0 || nrow(measures_info) == 0) {
    cli::cli_alert_danger("No trait measurements found matching criteria: {identifier_desc}")
    return(invisible(summary))
  }

  summary$counts$trait_measurements <- length(measure_ids_to_delete)

  if (verbose) {
    cli::cli_h2("Trait Measurements to Delete")
    cli::cli_alert_info("Found {length(measure_ids_to_delete)} measurement record(s) for {length(unique(measures_info$idtax))} taxon/taxa")

    n_show <- min(nrow(measures_info), 10)
    cli::cli_alert_info("Showing first {n_show} records:")
    for (i in seq_len(n_show)) {
      row <- measures_info[i, ]
      val <- if (!is.na(row$traitvalue)) row$traitvalue else row$traitvalue_char
      cli::cli_li(
        "id_measures: {row$id_trait_measures} | idtax: {row$idtax} | trait: {row$trait} | value: {val}"
      )
    }
    if (nrow(measures_info) > 10) {
      cli::cli_alert_info("... and {nrow(measures_info) - 10} more")
    }
  }

  # ===== STEP 2: Count related features =====
  if (verbose) cli::cli_h2("Related Data")

  n_feat <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT COUNT(*) AS n
      FROM taxa_traits_measures_feat
      WHERE id_trait_measures IN (%s)
    ", paste(measure_ids_to_delete, collapse = ",")))$n
  }, error = function(e) {
    cli::cli_alert_warning("Could not count measurement features: {e$message}")
    0L
  })

  summary$counts$measurement_features <- n_feat

  if (n_feat > 0) {
    if (verbose) cli::cli_alert_warning("{n_feat} trait measurement feature(s) will be deleted")
  } else {
    if (verbose) cli::cli_alert_success("No measurement features found")
  }

  # ===== STEP 3: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN - nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    cli::cli_alert_success("Taxonomy is preserved (no taxa table entries affected)")
    return(invisible(summary))
  }

  # ===== STEP 4: Confirmation =====
  if (!force) {
    cli::cli_h2("WARNING: CONFIRMATION REQUIRED")
    cli::cli_alert_danger("You are about to PERMANENTLY delete:")
    lines <- c(
      sprintf("%d trait measurement record(s)", length(measure_ids_to_delete))
    )
    if (n_feat > 0) lines <- c(lines, sprintf("%d measurement feature(s)", as.integer(n_feat)))
    cli::cli_ul(lines)
    cli::cli_alert_success("Taxonomy will be PRESERVED (taxa entries not deleted)")

    confirm <- choose_prompt(
      message = "Are you ABSOLUTELY SURE you want to delete these trait measurements?"
    )

    if (!confirm) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 5: Delete in correct order =====
  cli::cli_h2("Deleting Data")

  batch_size <- 5000
  unique_measure_ids <- unique(measure_ids_to_delete)
  n_batches <- ceiling(length(unique_measure_ids) / batch_size)

  # Step 5.1: Delete measurement features first (respect FK)
  if (n_feat > 0) {
    if (verbose) cli::cli_alert_info("Deleting measurement features...")

    total_deleted_feat <- 0L

    for (batch_idx in seq_len(n_batches)) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx   <- min(batch_idx * batch_size, length(unique_measure_ids))
      batch_ids <- unique_measure_ids[start_idx:end_idx]

      tryCatch({
        DBI::dbBegin(con)
        rs <- DBI::dbSendQuery(con, sprintf(
          "DELETE FROM taxa_traits_measures_feat WHERE id_trait_measures IN (%s)",
          paste(batch_ids, collapse = ",")
        ))
        n_del <- DBI::dbGetRowsAffected(rs)
        DBI::dbClearResult(rs)
        DBI::dbCommit(con)

        total_deleted_feat <- total_deleted_feat + n_del

        if (verbose && n_batches > 1) {
          cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_del} feature(s)")
        }
      }, error = function(e) {
        tryCatch(DBI::dbRollback(con), error = function(e2) {})
        cli::cli_alert_danger("Error deleting measurement features (batch {batch_idx}): {e$message}")
        summary$success <- FALSE
        summary$errors  <- c(summary$errors, list(e$message))
      })
    }

    summary$deleted$measurement_features <- total_deleted_feat
    if (verbose) cli::cli_alert_success("  Deleted {total_deleted_feat} measurement feature(s) total")
  }

  # Step 5.2: Delete trait measurements
  if (verbose) cli::cli_alert_info("Deleting trait measurements...")

  total_deleted_measures <- 0L

  for (batch_idx in seq_len(n_batches)) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx   <- min(batch_idx * batch_size, length(unique_measure_ids))
    batch_ids <- unique_measure_ids[start_idx:end_idx]

    tryCatch({
      DBI::dbBegin(con)
      rs <- DBI::dbSendQuery(con, sprintf(
        "DELETE FROM taxa_traits_measures WHERE id_trait_measures IN (%s)",
        paste(batch_ids, collapse = ",")
      ))
      n_del <- DBI::dbGetRowsAffected(rs)
      DBI::dbClearResult(rs)
      DBI::dbCommit(con)

      total_deleted_measures <- total_deleted_measures + n_del

      if (verbose && n_batches > 1) {
        cli::cli_alert_info("    Batch {batch_idx}/{n_batches}: deleted {n_del} measurement(s)")
      }
    }, error = function(e) {
      tryCatch(DBI::dbRollback(con), error = function(e2) {})
      cli::cli_alert_danger("Error deleting trait measurements (batch {batch_idx}): {e$message}")
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
      lines <- c(sprintf("%d trait measurement record(s)", total_deleted_measures))
      if (!is.null(summary$deleted$measurement_features)) {
        lines <- c(lines, sprintf("%d measurement feature(s)", as.integer(summary$deleted$measurement_features)))
      }
      cli::cli_ul(lines)
      cli::cli_alert_success("Taxonomy preserved (taxa entries intact)")
    }
  } else {
    cli::cli_alert_warning(
      "Partial deletion: {total_deleted_measures}/{length(unique_measure_ids)} measurements deleted"
    )
  }

  invisible(summary)
}
