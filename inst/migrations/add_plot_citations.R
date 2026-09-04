# ARCHIVED MIGRATION - not yet applied
#
# This file is not part of the package namespace. It is installed under
# inst/migrations/ so that what was done to the database stays readable.
# See README.md in this directory for what each migration changed and the
# evidence that it ran. Update that status table once this one has actually
# been applied and verified.
#
# To run it (should not be necessary more than once):
#   source(system.file("migrations", "add_plot_citations.R", package = "CafriplotsR"))
#   con <- CafriplotsR::call.mydb()


#' Migration: Add id_citation FK to data_liste_plots
#'
#' Adds an `id_citation` foreign key column to `data_liste_plots`, pointing at
#' the `table_citations` lookup created by `add_citations_table.R`. Reuses
#' that same table rather than creating a second one - a citation describes a
#' compiled dataset/database, whether it contributed taxon-level trait
#' measurements or plot inventory data.
#'
#' @details
#' This is the plot-level counterpart of the `id_citation` column
#' `add_citations_table.R` added to `taxa_traits_measures`. It lets a plot
#' record which dataset/study its inventory data comes from, the same way
#' `method` and `country` already record their lookups as FK ids directly on
#' `data_liste_plots`.
#'
#' `table_citations` already grants SELECT/INSERT/UPDATE to PUBLIC (from
#' `add_citations_table.R`), so no new grant is needed for the lookup table
#' itself.
#'
#' @param con Database connection to `plots_transects` (must have privileges
#'   to alter `data_liste_plots`)
#' @param dry_run If TRUE, only print SQL without executing (default: TRUE)
#' @return Invisible TRUE on success
#'
#' @examples
#' \dontrun{
#' con <- call.mydb(user = "admin", password = "xxx")
#'
#' # Preview the migration
#' migrate_add_plot_citations(con, dry_run = TRUE)
#'
#' # Run the migration
#' migrate_add_plot_citations(con, dry_run = FALSE)
#' }
#'
#' @keywords internal
migrate_add_plot_citations <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: Add id_citation to data_liste_plots")

  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Invalid database connection")
  }

  # -------------------------------------------------------------------------
  # Step 1: Require table_citations to already exist
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 1: Check table_citations exists")

  citations_exists <- DBI::dbGetQuery(con, "
    SELECT COUNT(*) AS n FROM information_schema.tables
    WHERE table_name = 'table_citations'
  ")$n > 0

  if (!citations_exists) {
    cli::cli_abort(paste(
      "table_citations does not exist.",
      "Run migrate_add_citations_table() first",
      "(see inst/migrations/add_citations_table.R)."
    ))
  }
  cli::cli_alert_success("table_citations exists")

  # -------------------------------------------------------------------------
  # Step 2: Add id_citation FK to data_liste_plots
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 2: Add id_citation to data_liste_plots")

  sql_add_fk <- "
    ALTER TABLE data_liste_plots
      ADD COLUMN IF NOT EXISTS id_citation INTEGER
        REFERENCES table_citations(id_citation);
  "

  if (dry_run) {
    cli::cli_alert_info("Would execute: {.code ALTER TABLE data_liste_plots ADD COLUMN IF NOT EXISTS id_citation INTEGER REFERENCES table_citations(id_citation)}")
  } else {
    tryCatch({
      DBI::dbExecute(con, sql_add_fk)
      cli::cli_alert_success("Column id_citation added to data_liste_plots")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to add id_citation column: {e$message}")
      stop(e)
    })
  }

  # -------------------------------------------------------------------------
  # Step 3: Verify
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 3: Verify migration")

  if (!dry_run) {
    col_exists <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM information_schema.columns
      WHERE table_name = 'data_liste_plots'
        AND column_name = 'id_citation'
    ")$n > 0

    if (col_exists) {
      cli::cli_alert_success("Column id_citation exists on data_liste_plots")
    } else {
      cli::cli_alert_danger("Column id_citation NOT found on data_liste_plots!")
    }
  }

  # Summary
  cli::cli_h2("Done")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN - No changes were made")
    cli::cli_alert_info("Run with {.code dry_run = FALSE} to apply changes")
  } else {
    cli::cli_alert_success("Migration completed successfully")
  }

  invisible(TRUE)
}

#' Check plot citations migration status
#'
#' Verifies whether `migrate_add_plot_citations()` has been applied.
#'
#' @param con Database connection to `plots_transects`
#' @return Invisible list with fields `fk_column_exists`, `migration_complete`
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' check_plot_citations_migration(con)
#' }
#'
#' @keywords internal
check_plot_citations_migration <- function(con) {

  cli::cli_h2("Checking plot citations migration status")

  result <- list(fk_column_exists = FALSE)

  result$fk_column_exists <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n FROM information_schema.columns
      WHERE table_name = 'data_liste_plots'
        AND column_name = 'id_citation'
    ")$n > 0
  }, error = function(e) FALSE)

  if (result$fk_column_exists) {
    cli::cli_alert_success("Column id_citation exists on data_liste_plots")
  } else {
    cli::cli_alert_danger("Column id_citation NOT found on data_liste_plots")
  }

  result$migration_complete <- result$fk_column_exists

  if (result$migration_complete) {
    cli::cli_alert_success("Plot citations migration is complete")
  } else {
    cli::cli_alert_warning("Plot citations migration is incomplete - run migrate_add_plot_citations(con, dry_run = FALSE)")
  }

  invisible(result)
}
