#' Safely delete subplot observation features
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' Deletes records of `data_subplot_feat`: the features that describe a
#' subplot, such as the people of a census (`team_leader`,
#' `additional_people`, ...). Nothing references `data_subplot_feat`, so only
#' the selected records are removed. **Subplots and plots are preserved.**
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Requires explicit confirmation (unless force = TRUE)
#' - Uses a database transaction (rollback on error)
#'
#' @param id_subplot_feat Integer vector. Specific record ID(s) to delete
#'   (`id_subplot_feat` from `data_subplot_feat`). Takes precedence over
#'   `subplot_ids` / `types`. Typically obtained from
#'   \code{query_plot_features(..., format = "long",
#'   include_subplot_obs_features = TRUE)$subplot_obs_features}.
#'   Default NULL.
#' @param subplot_ids Integer vector. Delete all observation features of these
#'   subplot(s) (`id_sub_plots` from `data_liste_sub_plots`). Can be combined
#'   with `types`. Default NULL.
#' @param types Character vector. Restrict to these feature types (`type`
#'   from `subplotype_list`, e.g. `"additional_people"`). Only used with
#'   `subplot_ids`. Default NULL (all types).
#' @param con Database connection. If NULL, will connect automatically.
#' @param dry_run Logical. If TRUE, shows what would be deleted without deleting.
#'   Default TRUE for safety.
#' @param force Logical. If TRUE, skips confirmation prompts. Default FALSE.
#'   **USE WITH EXTREME CAUTION!**
#' @param verbose Logical. Show detailed progress? Default TRUE.
#'
#' @return List with deletion summary (invisible). Includes `feat_info`, a
#'   data frame with one row per record to be deleted (`id_subplot_feat`,
#'   `id_sub_plots`, `type`, `typevalue`, `typevalue_char`, `colnam`).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # STEP 1: Extract the records and pick the ones to remove
#' obs <- query_plot_features(subplot_ids = 15063, format = "long",
#'                            include_subplot_obs_features = TRUE,
#'                            con = con)$subplot_obs_features
#' ids_to_remove <- obs$id_subplot_feat[obs$colnam == "Some Person"]
#'
#' # STEP 2: Always do a dry-run first!
#' safe_delete_subplot_obs_features(id_subplot_feat = ids_to_remove, con = con)
#'
#' # STEP 3: Review output, then delete if sure
#' safe_delete_subplot_obs_features(id_subplot_feat = ids_to_remove,
#'                                  con = con, dry_run = FALSE)
#'
#' # Alternative: every additional_people record of a census
#' safe_delete_subplot_obs_features(subplot_ids = 15063,
#'                                  types = "additional_people", con = con)
#' }
#'
#' @export
safe_delete_subplot_obs_features <- function(id_subplot_feat = NULL,
                                             subplot_ids = NULL,
                                             types = NULL,
                                             con = NULL,
                                             dry_run = TRUE,
                                             force = FALSE,
                                             verbose = TRUE) {

  # --- Input validation ---
  if (is.null(id_subplot_feat) && is.null(subplot_ids)) {
    stop(
      "Must provide at least one of: id_subplot_feat or subplot_ids",
      call. = FALSE
    )
  }

  # The ids are pasted into SQL, so only whole numbers get through
  check_ids <- function(x, arg) {
    if (!is.null(x) && (!is.numeric(x) || length(x) == 0 ||
                        anyNA(x) || any(x != round(x)))) {
      stop(arg, " must be a non-empty vector of whole numbers", call. = FALSE)
    }
  }
  check_ids(id_subplot_feat, "id_subplot_feat")
  check_ids(subplot_ids, "subplot_ids")

  if (is.null(con)) {
    con <- call.mydb()
  }

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  summary <- list(
    id_subplot_feat = id_subplot_feat,
    subplot_ids     = subplot_ids,
    types           = types,
    dry_run         = dry_run,
    counts          = list(),
    deleted         = list(subplot_obs_features = 0L),
    success         = FALSE
  )

  # ===== STEP 1: Resolve the records to delete =====
  if (verbose) cli::cli_h1("Finding Subplot Observation Features to Delete")

  if (!is.null(id_subplot_feat)) {
    where <- sprintf("sf.id_subplot_feat IN (%s)",
                     paste(format(unique(id_subplot_feat), scientific = FALSE, trim = TRUE),
                           collapse = ","))
    identifier_desc <- sprintf("%d specific record(s)",
                               length(unique(id_subplot_feat)))
  } else {
    where <- sprintf("sf.id_sub_plots IN (%s)",
                     paste(format(unique(subplot_ids), scientific = FALSE, trim = TRUE),
                           collapse = ","))
    identifier_desc <- sprintf("%d subplot(s)", length(unique(subplot_ids)))
    if (!is.null(types)) {
      identifier_desc <- paste(identifier_desc,
                               sprintf("and %d type(s)", length(types)))
    }
  }

  feat_info <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT sf.id_subplot_feat,
             sf.id_sub_plots,
             spt.type,
             sf.typevalue,
             sf.typevalue_char,
             tc.colnam
      FROM data_subplot_feat sf
      LEFT JOIN subplotype_list spt ON sf.id_type_sub_plot = spt.id_subplotype
      LEFT JOIN table_colnam tc
             ON spt.valuetype = 'table_colnam'
            AND sf.typevalue = tc.id_table_colnam
      WHERE %s
      ORDER BY sf.id_sub_plots, spt.type, sf.id_subplot_feat
    ", where))
  }, error = function(e) {
    stop("Failed to query subplot observation features: ", e$message,
         call. = FALSE)
  })

  # Type filter applied here, so no user text is pasted into the SQL
  if (is.null(id_subplot_feat) && !is.null(types)) {
    feat_info <- feat_info[feat_info$type %in% types, , drop = FALSE]
  }

  if (nrow(feat_info) == 0) {
    cli::cli_alert_danger(
      "No subplot observation features found matching criteria: {identifier_desc}"
    )
    return(invisible(summary))
  }

  if (!is.null(id_subplot_feat)) {
    missing_ids <- setdiff(id_subplot_feat, feat_info$id_subplot_feat)
    if (length(missing_ids) > 0) {
      cli::cli_alert_warning(
        "{length(missing_ids)} id_subplot_feat not found: {missing_ids}"
      )
    }
  }

  ids_to_delete <- unique(feat_info$id_subplot_feat)
  summary$counts$subplot_obs_features <- length(ids_to_delete)
  summary$feat_info <- feat_info

  if (verbose) {
    cli::cli_h2("Records to Delete")
    cli::cli_alert_info(
      "Found {length(ids_to_delete)} record(s) for {length(unique(feat_info$id_sub_plots))} subplot(s)"
    )

    n_show <- min(nrow(feat_info), 10)
    cli::cli_alert_info("Showing first {n_show} records:")
    for (i in seq_len(n_show)) {
      row <- feat_info[i, ]
      val <- if (!is.na(row$colnam)) {
        row$colnam
      } else if (!is.na(row$typevalue)) {
        row$typevalue
      } else {
        row$typevalue_char
      }
      cli::cli_li(
        "id_subplot_feat: {row$id_subplot_feat} | subplot: {row$id_sub_plots} | type: {row$type} | value: {val}"
      )
    }
    if (nrow(feat_info) > 10) {
      cli::cli_alert_info("... and {nrow(feat_info) - 10} more")
    }
  }

  # ===== STEP 2: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN - nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    return(invisible(summary))
  }

  # ===== STEP 3: Confirmation =====
  if (!force) {
    cli::cli_h2("WARNING: CONFIRMATION REQUIRED")
    cli::cli_alert_danger(
      "You are about to PERMANENTLY delete {length(ids_to_delete)} subplot observation feature record(s)"
    )
    cli::cli_alert_success("Subplots and plots will be PRESERVED")

    confirm <- choose_prompt(
      message = "Are you ABSOLUTELY SURE you want to delete these subplot observation features?"
    )

    if (!isTRUE(confirm)) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 4: Delete =====
  is_pool <- inherits(con, "Pool")
  if (is_pool) {
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)
  } else {
    raw_con <- con
  }

  tryCatch({
    DBI::dbBegin(raw_con)
    n_del <- DBI::dbExecute(raw_con, sprintf(
      "DELETE FROM data_subplot_feat WHERE id_subplot_feat IN (%s)",
      paste(format(ids_to_delete, scientific = FALSE, trim = TRUE), collapse = ",")
    ))
    DBI::dbCommit(raw_con)

    summary$deleted$subplot_obs_features <- n_del
    summary$success <- TRUE
    if (verbose) {
      cli::cli_alert_success("Deleted {n_del} subplot observation feature record(s)")
    }
  }, error = function(e) {
    tryCatch(DBI::dbRollback(raw_con), error = function(e2) {})
    cli::cli_alert_danger("Error deleting subplot observation features: {e$message}")
    cli::cli_alert_warning("Deletion rolled back - nothing was deleted")
  })

  invisible(summary)
}
