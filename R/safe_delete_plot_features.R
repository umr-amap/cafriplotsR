#' Safely delete plot features
#'
#' @description
#' **DANGER: This permanently deletes data from the database!**
#'
#' Deletes records of `data_liste_sub_plots`: the features of a plot, such as
#' a census, a soil sample or a `principal_investigator` (see
#' [query_plot_features()]). **The plot itself is preserved**, as are its
#' individuals.
#'
#' A plot feature can have things attached to it, and they are dealt with
#' before the feature is removed:
#' - its observation features (`data_subplot_feat`, e.g. the people of a
#'   census) are deleted with it - they describe nothing else;
#' - measurements linked to it (`data_traits_measures.id_sub_plots` and
#'   `data_ind_measures_feat.id_sub_plots`, i.e. the measurements taken during
#'   a census) are **not** deleted. See `linked_measurements`.
#'
#' **Safety features:**
#' - Dry-run mode to preview what will be deleted
#' - Refuses to strand census-linked measurements unless told what to do
#' - Requires explicit confirmation (unless force = TRUE)
#' - Uses a single database transaction (rollback on error)
#'
#' @param subplot_ids Integer vector. Specific plot feature record(s) to delete
#'   (`id_sub_plots` from `data_liste_sub_plots`). Takes precedence over
#'   `plot_ids`. Typically obtained from
#'   \code{query_plot_features(plot_ids = ..., format = "long")$features_raw}.
#'   Default NULL.
#' @param plot_ids Integer vector. Delete the features of these plot(s)
#'   (`id_liste_plots`). Best combined with `types`. Default NULL.
#' @param types Character vector. Restrict to these feature types (`type`
#'   from `subplotype_list`, e.g. `"soil_sample"`). Default NULL (all types).
#' @param linked_measurements Character. What to do when individual
#'   measurements are linked to a feature being deleted (typically the
#'   diameters of a census): `"stop"` (default) refuses and lists them;
#'   `"unlink"` keeps the measurements and sets their `id_sub_plots` to NULL,
#'   so they no longer belong to any census. To delete them instead, use
#'   [safe_delete_individual_features()] first.
#' @param con Database connection. If NULL, will connect automatically.
#' @param dry_run Logical. If TRUE, shows what would be deleted without deleting.
#'   Default TRUE for safety.
#' @param force Logical. If TRUE, skips confirmation prompts. Default FALSE.
#'   **USE WITH EXTREME CAUTION!**
#' @param verbose Logical. Show detailed progress? Default TRUE.
#'
#' @return List with deletion summary (invisible). Includes `feat_info`, a
#'   data frame with one row per plot feature to be deleted (`id_sub_plots`,
#'   `id_table_liste_plots`, `plot_name`, `type`, `year`, `typevalue`,
#'   `typevalue_char`, `colnam`, and the counts `n_obs_features`,
#'   `n_measures`, `n_measure_features` of what is attached to it), `blocked`
#'   (TRUE when linked measurements stopped the deletion), and `error`, the
#'   database message, when the deletion failed.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # STEP 1: Extract the features and pick the ones to remove
#' feats <- query_plot_features(plot_ids = 123, format = "long",
#'                              con = con)$features_raw
#' ids_to_remove <- feats$id_sub_plots[feats$type == "soil_sample"]
#'
#' # STEP 2: Always do a dry-run first!
#' safe_delete_plot_features(subplot_ids = ids_to_remove, con = con)
#'
#' # STEP 3: Review output, then delete if sure
#' safe_delete_plot_features(subplot_ids = ids_to_remove,
#'                           con = con, dry_run = FALSE)
#'
#' # Alternative: every soil_sample feature of a plot
#' safe_delete_plot_features(plot_ids = 123, types = "soil_sample", con = con)
#'
#' # A census whose measurements must be kept, detached from it
#' safe_delete_plot_features(subplot_ids = 15063,
#'                           linked_measurements = "unlink", con = con)
#' }
#'
#' @seealso [safe_delete_subplot_obs_features()] to delete only the
#'   observation features of a plot feature, [safe_delete_plot()] to delete
#'   the whole plot.
#'
#' @export
safe_delete_plot_features <- function(subplot_ids = NULL,
                                      plot_ids = NULL,
                                      types = NULL,
                                      linked_measurements = c("stop", "unlink"),
                                      con = NULL,
                                      dry_run = TRUE,
                                      force = FALSE,
                                      verbose = TRUE) {

  linked_measurements <- match.arg(linked_measurements)

  # --- Input validation ---
  if (is.null(subplot_ids) && is.null(plot_ids)) {
    stop("Must provide at least one of: subplot_ids or plot_ids", call. = FALSE)
  }

  # The ids are pasted into SQL, so only whole numbers get through
  check_ids <- function(x, arg) {
    if (!is.null(x) && (!is.numeric(x) || length(x) == 0 ||
                        anyNA(x) || any(x != round(x)))) {
      stop(arg, " must be a non-empty vector of whole numbers", call. = FALSE)
    }
  }
  check_ids(subplot_ids, "subplot_ids")
  check_ids(plot_ids, "plot_ids")

  if (is.null(con)) {
    con <- call.mydb()
  }

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  ids_sql <- function(x) {
    paste(format(unique(x), scientific = FALSE, trim = TRUE), collapse = ",")
  }

  summary <- list(
    subplot_ids         = subplot_ids,
    plot_ids            = plot_ids,
    types               = types,
    linked_measurements = linked_measurements,
    dry_run             = dry_run,
    counts              = list(),
    deleted             = list(subplot_obs_features = 0L, plot_features = 0L),
    unlinked            = list(measurements = 0L, measurement_features = 0L),
    blocked             = FALSE,
    success             = FALSE
  )

  # ===== STEP 1: Resolve the features to delete =====
  if (verbose) cli::cli_h1("Finding Plot Features to Delete")

  if (!is.null(subplot_ids)) {
    where <- sprintf("sp.id_sub_plots IN (%s)", ids_sql(subplot_ids))
    identifier_desc <- sprintf("%d specific record(s)",
                               length(unique(subplot_ids)))
  } else {
    where <- sprintf("sp.id_table_liste_plots IN (%s)", ids_sql(plot_ids))
    identifier_desc <- sprintf("%d plot(s)", length(unique(plot_ids)))
  }
  if (!is.null(types)) {
    identifier_desc <- paste(identifier_desc,
                             sprintf("and %d type(s)", length(types)))
  }

  feat_info <- tryCatch({
    DBI::dbGetQuery(con, sprintf("
      SELECT sp.id_sub_plots,
             sp.id_table_liste_plots,
             p.plot_name,
             spt.type,
             sp.year,
             sp.typevalue,
             sp.typevalue_char,
             tc.colnam
      FROM data_liste_sub_plots sp
      LEFT JOIN data_liste_plots p ON sp.id_table_liste_plots = p.id_liste_plots
      LEFT JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
      LEFT JOIN table_colnam tc
             ON spt.valuetype = 'table_colnam'
            AND sp.typevalue = tc.id_table_colnam
      WHERE %s
      ORDER BY sp.id_table_liste_plots, spt.type, sp.id_sub_plots
    ", where))
  }, error = function(e) {
    stop("Failed to query plot features: ", e$message, call. = FALSE)
  })

  # Type filter applied here, so no user text is pasted into the SQL
  if (!is.null(types)) {
    feat_info <- feat_info[feat_info$type %in% types, , drop = FALSE]
  }

  if (nrow(feat_info) == 0) {
    cli::cli_alert_danger(
      "No plot features found matching criteria: {identifier_desc}"
    )
    return(invisible(summary))
  }

  if (!is.null(subplot_ids)) {
    missing_ids <- setdiff(subplot_ids, feat_info$id_sub_plots)
    if (length(missing_ids) > 0) {
      cli::cli_alert_warning(
        "{length(missing_ids)} id_sub_plots not found: {missing_ids}"
      )
    }
  }

  ids_to_delete <- unique(feat_info$id_sub_plots)
  del_sql <- ids_sql(ids_to_delete)

  # ===== STEP 2: What is attached to them =====
  count_by_subplot <- function(table) {
    tryCatch(
      DBI::dbGetQuery(con, sprintf("
        SELECT id_sub_plots, COUNT(*) AS n FROM %s
        WHERE id_sub_plots IN (%s) GROUP BY id_sub_plots
      ", table, del_sql)),
      error = function(e) {
        stop("Failed to count rows of ", table, " linked to these features: ",
             e$message, call. = FALSE)
      }
    )
  }
  attach_counts <- function(counts) {
    n <- counts$n[match(feat_info$id_sub_plots, counts$id_sub_plots)]
    n[is.na(n)] <- 0L
    as.integer(n)
  }

  feat_info$n_obs_features <- attach_counts(count_by_subplot("data_subplot_feat"))
  feat_info$n_measures <- attach_counts(count_by_subplot("data_traits_measures"))
  feat_info$n_measure_features <-
    attach_counts(count_by_subplot("data_ind_measures_feat"))

  summary$feat_info <- feat_info
  summary$counts <- list(
    plot_features        = length(ids_to_delete),
    subplot_obs_features = sum(feat_info$n_obs_features),
    measurements         = sum(feat_info$n_measures),
    measurement_features = sum(feat_info$n_measure_features)
  )
  counts <- summary$counts
  n_linked <- counts$measurements + counts$measurement_features

  if (verbose) {
    cli::cli_h2("Plot Features to Delete")
    cli::cli_alert_info(
      "Found {counts$plot_features} feature(s) on {length(unique(feat_info$id_table_liste_plots))} plot(s)"
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
      cli::cli_li(paste0(
        "id_sub_plots: {row$id_sub_plots} | plot: {row$plot_name} | ",
        "type: {row$type} | year: {row$year} | value: {val}"
      ))
    }
    if (nrow(feat_info) > 10) {
      cli::cli_alert_info("... and {nrow(feat_info) - 10} more")
    }

    if (counts$subplot_obs_features > 0) {
      cli::cli_alert_warning(
        "{counts$subplot_obs_features} observation feature(s) attached to them will be deleted too"
      )
    }
  }

  if (any(feat_info$type %in% "census")) {
    cli::cli_alert_warning(
      "{sum(feat_info$type %in% 'census')} of these feature(s) {?is a/are} census{?es}"
    )
  }

  # ===== STEP 3: Linked measurements =====
  if (n_linked > 0) {
    linked <- feat_info[feat_info$n_measures + feat_info$n_measure_features > 0,
                        c("id_sub_plots", "plot_name", "type", "year",
                          "n_measures", "n_measure_features")]
    cli::cli_h2("Linked Measurements")
    cli::cli_alert_warning(
      "{counts$measurements} measurement(s) and {counts$measurement_features} measurement feature(s) are linked to {nrow(linked)} of these feature(s)"
    )
    if (verbose) print(linked, row.names = FALSE)

    if (linked_measurements == "stop") {
      summary$blocked <- TRUE
      cli::cli_alert_danger(
        "Refusing to delete: these measurements would point at a feature that no longer exists"
      )
      cli::cli_alert_info(
        "Delete them first with safe_delete_individual_features(), or re-run with linked_measurements = \"unlink\" to keep them without a census"
      )
      return(invisible(summary))
    }

    cli::cli_alert_info(
      "linked_measurements = \"unlink\": they will be kept, with id_sub_plots set to NULL"
    )
  }

  # ===== STEP 4: Dry-run exit =====
  if (dry_run) {
    cli::cli_alert_info("This was a DRY-RUN - nothing was deleted")
    cli::cli_alert_info("To actually delete, run with dry_run = FALSE")
    return(invisible(summary))
  }

  # ===== STEP 5: Confirmation =====
  if (!force) {
    cli::cli_h2("WARNING: CONFIRMATION REQUIRED")
    cli::cli_alert_danger(
      "You are about to PERMANENTLY delete {counts$plot_features} plot feature(s) and {counts$subplot_obs_features} observation feature(s)"
    )
    if (n_linked > 0) {
      cli::cli_alert_warning(
        "{n_linked} linked measurement row(s) will be detached from their census"
      )
    }
    cli::cli_alert_success("Plots and individuals will be PRESERVED")

    confirm <- choose_prompt(
      message = "Are you ABSOLUTELY SURE you want to delete these plot features?"
    )

    if (!isTRUE(confirm)) {
      cli::cli_alert_info("Deletion cancelled by user")
      return(invisible(summary))
    }
  }

  # ===== STEP 6: Delete, children first, in one transaction =====
  is_pool <- inherits(con, "Pool")
  if (is_pool) {
    raw_con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(raw_con), add = TRUE)
  } else {
    raw_con <- con
  }

  tryCatch({
    DBI::dbBegin(raw_con)

    n_obs <- DBI::dbExecute(raw_con, sprintf(
      "DELETE FROM data_subplot_feat WHERE id_sub_plots IN (%s)", del_sql
    ))
    n_unl_meas <- 0L
    n_unl_feat <- 0L
    if (n_linked > 0) {
      n_unl_meas <- DBI::dbExecute(raw_con, sprintf(
        "UPDATE data_traits_measures SET id_sub_plots = NULL WHERE id_sub_plots IN (%s)",
        del_sql
      ))
      n_unl_feat <- DBI::dbExecute(raw_con, sprintf(
        "UPDATE data_ind_measures_feat SET id_sub_plots = NULL WHERE id_sub_plots IN (%s)",
        del_sql
      ))
    }
    n_del <- DBI::dbExecute(raw_con, sprintf(
      "DELETE FROM data_liste_sub_plots WHERE id_sub_plots IN (%s)", del_sql
    ))

    # Row-level security hides rows instead of refusing them, so a short count
    # means part of the selection was not ours to delete
    if (n_del != length(ids_to_delete)) {
      stop(sprintf(
        "only %d of %d plot feature(s) could be deleted - check your rights on these plots",
        n_del, length(ids_to_delete)
      ), call. = FALSE)
    }

    DBI::dbCommit(raw_con)

    summary$deleted$subplot_obs_features <- n_obs
    summary$deleted$plot_features <- n_del
    summary$unlinked$measurements <- n_unl_meas
    summary$unlinked$measurement_features <- n_unl_feat
    summary$success <- TRUE
    if (verbose) {
      cli::cli_alert_success("Deleted {n_del} plot feature(s)")
      if (n_obs > 0) {
        cli::cli_alert_success("Deleted {n_obs} observation feature(s)")
      }
      if (n_unl_meas + n_unl_feat > 0) {
        cli::cli_alert_success(
          "Detached {n_unl_meas} measurement(s) and {n_unl_feat} measurement feature(s)"
        )
      }
    }
  }, error = function(e) {
    tryCatch(DBI::dbRollback(raw_con), error = function(e2) {})
    summary$error <<- conditionMessage(e)
    cli::cli_alert_danger("Error deleting plot features: {conditionMessage(e)}")
    cli::cli_alert_warning("Deletion rolled back - nothing was deleted")
  })

  invisible(summary)
}
