# MIGRATION - not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran.
#
#   source(system.file("migrations", "delete_orphan_plot_rows.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()
#   migrate_delete_orphan_plot_rows(con)                   # exports, deletes nothing
#   migrate_delete_orphan_plot_rows(con, dry_run = FALSE)  # exports, then deletes


#' Migration: export and delete the plot rows nothing can attribute
#'
#' Three groups of rows cannot be attached to a plot, and so would become
#' invisible to everyone but the table owner the moment row-level security
#' reaches these tables. None of them is recoverable from inside the database.
#' They are written to CSV and then removed.
#'
#' \describe{
#'   \item{890 `data_liste_sub_plots` rows}{23 with `id_table_liste_plots` NULL,
#'     867 naming a plot id that no longer exists. The column has no foreign
#'     key, so nothing prevented either state.}
#'   \item{139 `data_subplot_feat` rows}{everything hanging off those subplots.}
#'   \item{3 `data_traits_measures` rows}{`id_data_individuals` NULL.}
#' }
#'
#' @details
#' **Why none of them is recoverable.**
#'
#' The 23 NULL rows are transect endpoint coordinates and elevations entered in
#' June 2019, carrying `original_subplot_name` values `GD0301`-`GD0314`. No plot
#' of any of those names exists, so neither the measurements (there are none)
#' nor the names identify a plot.
#'
#' The 867 dangling rows name 193 plot ids that are absent from
#' `data_liste_plots`, and only 4 of the 193 appear in
#' `followup_updates_liste_plots`, so 189 plots were never recorded as having
#' existed. The ids run in contiguous blocks - 1998-2000, 2453-2473 - each block
#' carrying exactly 10 or 44 subplots across 7 or 21 feature types, and every row
#' was last modified in **2025**. That is the shape of a plot import that wrote
#' its subplots and then lost its plots, not of historical deletions.
#'
#' **This treats the symptom.** If the import path that produced them is still
#' in use, orphans will reappear. Worth establishing before relying on this
#' being a one-off; the contiguous 2025 id blocks are where to start.
#'
#' The 3 measurements are `stem_diameter` on a `LivingSpecimen`, with a plot and
#' a subplot but no specimen, no feature rows and no `original_tag_plot` - so
#' nothing names the tree they were measured on.
#'
#' @section Safety:
#' Every row is exported and the files are verified on disk **before** anything
#' is deleted, in dry run as well, so the CSVs can be read before the decision.
#'
#' Rows are deleted by explicit id, taken from the exported sets, never by
#' re-running the predicate. A predicate evaluated twice can match different
#' rows; this way what is deleted is exactly what was exported.
#'
#' The migration refuses to run if anything references these rows beyond the 139
#' feature rows it already accounts for - a measurement on one of the subplots,
#' or a feature row on one of the measurements. Those counts are 0 today, and if
#' they are not, the premise that these are leaf rows is wrong.
#'
#' Deletes run in one transaction, children before parents.
#'
#' @param con Database connection to `plots_transects`, as the table owner.
#' @param out_dir Directory for the CSVs. Defaults to a timestamped folder under
#'   `%LOCALAPPDATA%`, outside the repository, falling back to `tempdir()`.
#' @param dry_run If TRUE (the default), export and report, delete nothing.
#' @return Invisibly, a list of the exported frames, the file paths and the
#'   deletion counts.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_delete_orphan_plot_rows(con)                   # export and read the CSVs
#' migrate_delete_orphan_plot_rows(con, dry_run = FALSE)  # then delete
#' }
#'
#' @keywords internal
migrate_delete_orphan_plot_rows <- function(con, out_dir = NULL, dry_run = TRUE) {

  cli::cli_h1("Migration: export and delete the unattributable plot rows")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  # -- Step 1: preflight -----------------------------------------------------
  cli::cli_h2("Step 1: Preflight")

  whoami <- DBI::dbGetQuery(con, "SELECT current_database() AS db, current_user AS usr")
  if (whoami$db != "plots_transects") {
    cli::cli_abort("This migration belongs to plots_transects, not {.val {whoami$db}}")
  }
  cli::cli_alert_info("Connected to {.val {whoami$db}} as {.val {whoami$usr}}")

  if (is.null(out_dir)) {
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base <- if (nzchar(Sys.getenv("LOCALAPPDATA"))) {
      file.path(Sys.getenv("LOCALAPPDATA"), "cafri_deleted_rows")
    } else {
      file.path(tempdir(), "cafri_deleted_rows")
    }
    out_dir <- file.path(base, stamp)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir)) cli::cli_abort("Could not create {.path {out_dir}}")
  cli::cli_alert_info("CSVs will be written to {.path {out_dir}}")

  before <- DBI::dbGetQuery(con, "
    SELECT (SELECT count(*)::int FROM data_liste_sub_plots) AS sub_plots,
           (SELECT count(*)::int FROM data_subplot_feat)    AS subplot_feat,
           (SELECT count(*)::int FROM data_traits_measures) AS traits_measures")
  cli::cli_alert_info(
    "Before: {before$sub_plots} subplots, {before$subplot_feat} subplot features, \\
     {before$traits_measures} measurements")

  # -- Step 2: collect the rows ----------------------------------------------
  cli::cli_h2("Step 2: Collecting")

  # One predicate, written once: NULL plot, or a plot that is not there.
  orphan_pred <- "
    sp.id_table_liste_plots IS NULL
    OR NOT EXISTS (SELECT 1 FROM data_liste_plots p
                    WHERE p.id_liste_plots = sp.id_table_liste_plots)"

  sub_plots <- DBI::dbGetQuery(con, sprintf("
    SELECT sp.*,
           CASE WHEN sp.id_table_liste_plots IS NULL
                THEN 'no plot id' ELSE 'plot id absent' END AS orphan_reason
      FROM data_liste_sub_plots sp
     WHERE %s
     ORDER BY sp.id_sub_plots", orphan_pred))

  cli::cli_alert_info("{nrow(sub_plots)} orphan subplot row{?s}")
  if (nrow(sub_plots) > 0) {
    print(table(sub_plots$orphan_reason))
  }

  sub_ids <- as.integer(sub_plots$id_sub_plots)

  subplot_feat <- if (length(sub_ids) > 0) {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT * FROM data_subplot_feat WHERE id_sub_plots IN ({sub_ids*})
        ORDER BY id_subplot_feat", .con = con))
  } else {
    data.frame()
  }
  cli::cli_alert_info("{nrow(subplot_feat)} feature row{?s} hanging off them")

  measures <- DBI::dbGetQuery(con, "
    SELECT * FROM data_traits_measures
     WHERE id_data_individuals IS NULL
     ORDER BY id_trait_measures")
  cli::cli_alert_info("{nrow(measures)} measurement{?s} with no individual")

  measure_ids <- as.integer(measures$id_trait_measures)

  if (nrow(sub_plots) == 0 && nrow(measures) == 0) {
    cli::cli_alert_success("Nothing to do - no unattributable rows remain")
    return(invisible(list(sub_plots = sub_plots, subplot_feat = subplot_feat,
                          measures = measures, files = character(), deleted = NULL)))
  }

  # -- Step 3: is anything else attached? ------------------------------------
  # The premise is that these are leaf rows. If they are not, deleting them
  # would take something unaccounted for with them.
  cli::cli_h2("Step 3: Checking nothing else depends on them")

  n_meas_on_subplots <- if (length(sub_ids) > 0) {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT count(*)::int AS n FROM data_traits_measures
        WHERE id_sub_plots IN ({sub_ids*})", .con = con))$n
  } else 0L

  n_feat_on_measures <- if (length(measure_ids) > 0) {
    DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT count(*)::int AS n FROM data_ind_measures_feat
        WHERE id_trait_measures IN ({measure_ids*})", .con = con))$n
  } else 0L

  cli::cli_alert_info("Measurements on the orphan subplots: {n_meas_on_subplots}")
  cli::cli_alert_info("Feature rows on the orphan measurements: {n_feat_on_measures}")

  if (n_meas_on_subplots > 0 || n_feat_on_measures > 0) {
    cli::cli_abort(c(
      "These are not leaf rows - something references them.",
      x = "{n_meas_on_subplots} measurement(s) sit on the orphan subplots",
      x = "{n_feat_on_measures} feature row(s) sit on the orphan measurements",
      i = "Deleting now would remove data this migration has not exported.",
      i = "Re-run {.fn report_plot_scope_orphans} and settle those first."
    ))
  }
  cli::cli_alert_success("Leaf rows confirmed - nothing else points at them")

  # -- Step 4: export, and prove it landed ----------------------------------
  cli::cli_h2("Step 4: Exporting")

  write_one <- function(x, name) {
    if (nrow(x) == 0) {
      cli::cli_alert_info("{name}: no rows, no file")
      return(NA_character_)
    }

    # A bigint column arrives as integer64, which is a double vector wearing a
    # class attribute. write.csv() would print the reinterpreted double and the
    # export would be quietly worthless - and it is the only copy once the rows
    # are gone. Render those as text instead.
    for (j in seq_along(x)) {
      if (inherits(x[[j]], "integer64")) {
        x[[j]] <- format(x[[j]], scientific = FALSE, trim = TRUE)
        cli::cli_alert_info("{name}: column {.field {names(x)[j]}} was integer64, written as text")
      }
    }

    f <- file.path(out_dir, paste0(name, ".csv"))
    utils::write.csv(x, f, row.names = FALSE, na = "")

    # Verified by reading back, not by write.csv() having returned quietly. A
    # truncated or unwritten file must not be discovered after the delete.
    if (!file.exists(f)) cli::cli_abort("{.path {f}} was not written")
    back <- utils::read.csv(f, na.strings = "", check.names = FALSE)
    if (nrow(back) != nrow(x)) {
      cli::cli_abort(
        "{.path {f}} holds {nrow(back)} row(s), expected {nrow(x)} - refusing to continue")
    }
    cli::cli_alert_success("{name}: {nrow(x)} row{?s} -> {.path {basename(f)}}")
    f
  }

  files <- c(
    sub_plots    = write_one(sub_plots,    "data_liste_sub_plots_orphans"),
    subplot_feat = write_one(subplot_feat, "data_subplot_feat_orphans"),
    measures     = write_one(measures,     "data_traits_measures_orphans")
  )

  # A short note beside the CSVs, so the folder explains itself later.
  readme <- file.path(out_dir, "README.txt")
  writeLines(c(
    "Rows removed from plots_transects by inst/migrations/delete_orphan_plot_rows.R",
    paste("exported:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("database:", whoami$db, " as:", whoami$usr),
    "",
    "None of these rows could be attached to a plot, so row-level security would",
    "have hidden them from everyone including their owner.",
    "",
    sprintf("data_liste_sub_plots_orphans.csv  %d rows (%d with no plot id, %d naming a plot that no longer exists)",
            nrow(sub_plots),
            sum(sub_plots$orphan_reason == "no plot id"),
            sum(sub_plots$orphan_reason == "plot id absent")),
    sprintf("data_subplot_feat_orphans.csv     %d rows (everything hanging off those subplots)",
            nrow(subplot_feat)),
    sprintf("data_traits_measures_orphans.csv  %d rows (id_data_individuals NULL)",
            nrow(measures)),
    "",
    "The orphan_reason column is added by the export and is not a database column.",
    "Read back with: read.csv(f, na.strings = \"\")"
  ), readme)
  cli::cli_alert_success("Wrote {.path {basename(readme)}}")

  # -- Step 5: delete -------------------------------------------------------
  cli::cli_h2("Step 5: Deleting")

  if (dry_run) {
    cli::cli_alert_info("Would delete, children first:")
    cli::cli_ul(c(
      "{nrow(subplot_feat)} row(s) from data_subplot_feat",
      "{nrow(sub_plots)} row(s) from data_liste_sub_plots",
      "{nrow(measures)} row(s) from data_traits_measures"
    ))
    cli::cli_alert_info(
      "Dry run - nothing was deleted. The CSVs are written; read them, then \\
       re-run with {.code dry_run = FALSE}.")
    return(invisible(list(sub_plots = sub_plots, subplot_feat = subplot_feat,
                          measures = measures, files = files, deleted = NULL)))
  }

  # Chunked because an IN list of several hundred thousand ids is not a good
  # idea; these sets are small, but the helper does not assume that.
  delete_by_id <- function(tbl, id_col, ids, chunk = 5000L) {
    if (length(ids) == 0L) return(0L)
    total <- 0L
    for (start in seq.int(1L, length(ids), by = chunk)) {
      part <- ids[start:min(start + chunk - 1L, length(ids))]
      total <- total + DBI::dbExecute(con, glue::glue_sql(
        "DELETE FROM {`tbl`} WHERE {`id_col`} IN ({part*})", .con = con))
    }
    total
  }

  DBI::dbExecute(con, "SET lock_timeout = '30s'")

  DBI::dbBegin(con)
  deleted <- tryCatch({
    d_feat <- delete_by_id("data_subplot_feat", "id_subplot_feat",
                           as.integer(subplot_feat$id_subplot_feat))
    cli::cli_alert_info("data_subplot_feat: {d_feat} deleted")

    d_sub <- delete_by_id("data_liste_sub_plots", "id_sub_plots", sub_ids)
    cli::cli_alert_info("data_liste_sub_plots: {d_sub} deleted")

    d_meas <- delete_by_id("data_traits_measures", "id_trait_measures", measure_ids)
    cli::cli_alert_info("data_traits_measures: {d_meas} deleted")

    # Each count must match what was exported, or something moved underneath us.
    if (d_feat != nrow(subplot_feat) || d_sub != nrow(sub_plots) ||
        d_meas != nrow(measures)) {
      cli::cli_abort(paste(
        "Deleted counts do not match the export",
        "({d_feat}/{nrow(subplot_feat)}, {d_sub}/{nrow(sub_plots)},",
        "{d_meas}/{nrow(measures)}) - rolling back"))
    }

    DBI::dbCommit(con)
    cli::cli_alert_success("COMMITTED")
    list(subplot_feat = d_feat, sub_plots = d_sub, traits_measures = d_meas)
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Rolled back: {e$message}")
    NULL
  })

  if (is.null(deleted)) {
    cli::cli_alert_info("The CSVs at {.path {out_dir}} are still valid - nothing was removed.")
    stop("Migration failed - no rows were deleted.", call. = FALSE)
  }

  # -- Step 6: verify -------------------------------------------------------
  cli::cli_h2("Step 6: Verifying")

  after <- DBI::dbGetQuery(con, "
    SELECT (SELECT count(*)::int FROM data_liste_sub_plots) AS sub_plots,
           (SELECT count(*)::int FROM data_subplot_feat)    AS subplot_feat,
           (SELECT count(*)::int FROM data_traits_measures) AS traits_measures")

  print(data.frame(
    table    = c("data_liste_sub_plots", "data_subplot_feat", "data_traits_measures"),
    before   = c(before$sub_plots, before$subplot_feat, before$traits_measures),
    deleted  = c(deleted$sub_plots, deleted$subplot_feat, deleted$traits_measures),
    after    = c(after$sub_plots, after$subplot_feat, after$traits_measures),
    expected = c(before$sub_plots - deleted$sub_plots,
                 before$subplot_feat - deleted$subplot_feat,
                 before$traits_measures - deleted$traits_measures),
    row.names = NULL), row.names = FALSE)

  remaining <- DBI::dbGetQuery(con, "
    SELECT (SELECT count(*)::int FROM data_liste_sub_plots sp
             WHERE sp.id_table_liste_plots IS NULL
                OR NOT EXISTS (SELECT 1 FROM data_liste_plots p
                                WHERE p.id_liste_plots = sp.id_table_liste_plots))
             AS orphan_subplots,
           (SELECT count(*)::int FROM data_traits_measures
             WHERE id_data_individuals IS NULL) AS orphan_measures")
  show_ok <- remaining$orphan_subplots == 0 && remaining$orphan_measures == 0

  if (show_ok) {
    cli::cli_alert_success(
      "No unattributable row remains in either table - the foreign key on \\
       data_liste_sub_plots.id_table_liste_plots would now be accepted")
  } else {
    cli::cli_alert_warning(
      "{remaining$orphan_subplots} orphan subplot{?s} and \\
       {remaining$orphan_measures} orphan measurement{?s} remain - re-run")
  }

  cli::cli_alert_info("Deleted rows are at {.path {out_dir}}")
  invisible(list(sub_plots = sub_plots, subplot_feat = subplot_feat,
                 measures = measures, files = files, deleted = deleted))
}
