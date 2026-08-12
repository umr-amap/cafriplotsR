#' Migration: declare which features belong to a census
#'
#' Whether a measurement carries `id_sub_plots` decides how
#' `query_plots(show_multiple_census = TRUE)` reports it: linked features are
#' pivoted into one column per census (`stem_diameter_census1`,
#' `stem_diameter_census2`, ...). For a diameter that is the point. For a
#' quadrat it is wrong — the tree does not move between campaigns, and the
#' pivot would repeat one unchanging value under a different name per census.
#'
#' @details
#' The rule is a property of the feature, so it belongs next to the feature
#' definition where every import path can see it, rather than in one importer's
#' code. This adds `traitlist.census_link`, constrained to `'always'` or
#' `'never'`, and seeds `'never'` for the features in
#' [.default_census_link_policy()].
#'
#' Everything else is left `NULL`, which [.feature_census_link()] reads as
#' `'always'`. That is deliberate: `NULL` means "no exception claimed" rather
#' than "confirmed to belong to a census", and seeding a value for all 108
#' features would assert a classification that has not been made. The seeded
#' set was confirmed against [census_link_evidence()] — each of these features
#' is unlinked in every row already recorded.
#'
#' Existing rows of `data_traits_measures` are **not** touched. The column
#' governs what future imports write; rewriting history would change what
#' published queries return.
#'
#' @param con Database connection with rights to ALTER `traitlist`.
#' @param dry_run If TRUE (the default), report what would happen without
#'   altering anything.
#' @return Invisibly TRUE.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' migrate_traitlist_census_link(con)                   # checks only
#' migrate_traitlist_census_link(con, dry_run = FALSE)  # apply
#' }
#' @keywords internal
migrate_traitlist_census_link <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: census link policy on traitlist")

  if (!DBI::dbIsValid(con)) cli::cli_abort("Invalid database connection")

  never <- names(.default_census_link_policy())

  existing <- DBI::dbGetQuery(con, "
    SELECT column_name FROM information_schema.columns
     WHERE table_schema = 'public' AND table_name = 'traitlist'")$column_name
  has_column <- "census_link" %in% existing

  # Seeding a name that is not a feature would silently do nothing, so say so
  known <- DBI::dbGetQuery(con, "SELECT trait FROM traitlist")$trait
  unknown <- setdiff(never, known)
  if (length(unknown) > 0) {
    cli::cli_alert_warning(
      "Not in traitlist, will be skipped: {unknown}")
  }
  to_seed <- intersect(never, known)

  if (has_column) {
    cli::cli_alert_info("Column {.field census_link} already exists")
    current <- DBI::dbGetQuery(con, "
      SELECT trait, census_link FROM traitlist WHERE census_link IS NOT NULL")
    cli::cli_alert_info("{nrow(current)} feature{?s} already declared")
  } else {
    cli::cli_alert_info("Column to add: {.field census_link}")
  }
  cli::cli_alert_info(
    "{length(to_seed)} feature{?s} will be marked {.val never}: {to_seed}")
  cli::cli_alert_info(
    "Every other feature stays NULL, which reads as {.val always}")
  cli::cli_alert_info(
    "Existing measurements are not rewritten — this governs future imports")

  statements <- character(0)
  if (!has_column) {
    statements <- c(
      statements,
      "ALTER TABLE traitlist ADD COLUMN census_link TEXT",
      paste("ALTER TABLE traitlist ADD CONSTRAINT traitlist_census_link_check",
            "CHECK (census_link IS NULL OR census_link IN ('always', 'never'))")
    )
  }
  if (length(to_seed) > 0) {
    statements <- c(statements, sprintf(
      "UPDATE traitlist SET census_link = 'never' WHERE trait IN (%s)",
      paste(sprintf("'%s'", gsub("'", "''", to_seed)), collapse = ", ")
    ))
  }

  if (length(statements) == 0) {
    cli::cli_alert_success("Nothing to do")
    return(invisible(TRUE))
  }

  if (dry_run) {
    for (s in statements) cli::cli_alert_info("Would execute: {.code {s}}")
    cli::cli_alert_info("Dry run — nothing was altered. Re-run with {.code dry_run = FALSE}.")
    return(invisible(TRUE))
  }

  DBI::dbBegin(con)
  ok <- tryCatch({
    for (s in statements) {
      cli::cli_alert_info("Executing: {.code {substr(s, 1, 90)}}")
      DBI::dbExecute(con, s)
    }
    DBI::dbCommit(con)
    TRUE
  }, error = function(e) {
    try(DBI::dbRollback(con), silent = TRUE)
    cli::cli_alert_danger("Migration rolled back: {e$message}")
    FALSE
  })
  if (!ok) stop("Migration failed — no change was committed.", call. = FALSE)

  print(DBI::dbGetQuery(con, "
    SELECT census_link, count(*) AS n FROM traitlist
     GROUP BY census_link ORDER BY census_link NULLS LAST"))

  cli::cli_alert_success("Migration complete")
  invisible(TRUE)
}
