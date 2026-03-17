#' Migration: Add table_citations and id_citation FK to taxa_traits_measures
#'
#' Creates the `table_citations` lookup table in the `plots_transects` database
#' and adds an `id_citation` foreign key column to `taxa_traits_measures`.
#'
#' @details
#' The `table_citations` table stores structured bibliographic information for
#' the datasets and databases that contributed trait measurements. This is
#' distinct from the `references` flat-text column in `taxa_traits_measures`,
#' which records the original source of an individual measurement (e.g. a
#' herbarium label or field survey). A citation describes the compiled
#' dataset/database itself (e.g. TRY v6, BIEN, a curated species list) and
#' is what users should cite when using trait data.
#'
#' This migration:
#' 1. Creates `table_citations` with structured bibliographic fields
#' 2. Grants INSERT/UPDATE/SELECT on `table_citations` to PUBLIC
#' 3. Adds `id_citation` (FK to `table_citations`) to `taxa_traits_measures`
#'
#' @param con Database connection to `plots_transects` (must have admin privileges)
#' @param dry_run If TRUE, only print SQL without executing (default: TRUE)
#' @return Invisible TRUE on success
#'
#' @examples
#' \dontrun{
#' con <- call.mydb(user = "admin", password = "xxx")
#'
#' # Preview the migration
#' migrate_add_citations_table(con, dry_run = TRUE)
#'
#' # Run the migration
#' migrate_add_citations_table(con, dry_run = FALSE)
#' }
#'
#' @export
migrate_add_citations_table <- function(con, dry_run = TRUE) {

  cli::cli_h1("Migration: Add table_citations")

  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Invalid database connection")
  }

  # -------------------------------------------------------------------------
  # Step 1: Create table_citations
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 1: Create table_citations")

  sql_create_table <- "
    CREATE TABLE IF NOT EXISTS table_citations (
      id_citation   SERIAL PRIMARY KEY,
      citation_key  TEXT UNIQUE NOT NULL,
      authors       TEXT,
      year          INTEGER,
      title         TEXT,
      journal       TEXT,
      volume        TEXT,
      pages         TEXT,
      doi           TEXT,
      url           TEXT,
      dataset_name  TEXT,
      notes         TEXT,
      date_modif_d  SMALLINT,
      date_modif_m  SMALLINT,
      date_modif_y  SMALLINT
    );
  "

  if (dry_run) {
    cli::cli_alert_info("Would execute:{.code CREATE TABLE IF NOT EXISTS table_citations (...)}")
  } else {
    tryCatch({
      DBI::dbExecute(con, sql_create_table)
      cli::cli_alert_success("table_citations created (or already exists)")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to create table_citations: {e$message}")
      stop(e)
    })
  }

  # -------------------------------------------------------------------------
  # Step 2: Grant permissions to PUBLIC
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 2: Grant permissions on table_citations")

  sql_grant <- "GRANT SELECT, INSERT, UPDATE ON table_citations TO PUBLIC;"
  sql_grant_seq <- "GRANT USAGE, SELECT ON SEQUENCE table_citations_id_citation_seq TO PUBLIC;"

  if (dry_run) {
    cli::cli_alert_info("Would execute: {.code {trimws(sql_grant)}}")
    cli::cli_alert_info("Would execute: {.code {trimws(sql_grant_seq)}}")
  } else {
    tryCatch({
      DBI::dbExecute(con, sql_grant)
      DBI::dbExecute(con, sql_grant_seq)
      cli::cli_alert_success("Permissions granted on table_citations to PUBLIC")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to grant permissions: {e$message}")
      stop(e)
    })
  }

  # -------------------------------------------------------------------------
  # Step 3: Add id_citation FK to taxa_traits_measures
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 3: Add id_citation to taxa_traits_measures")

  sql_add_fk <- "
    ALTER TABLE taxa_traits_measures
      ADD COLUMN IF NOT EXISTS id_citation INTEGER
        REFERENCES table_citations(id_citation);
  "

  if (dry_run) {
    cli::cli_alert_info("Would execute: {.code ALTER TABLE taxa_traits_measures ADD COLUMN IF NOT EXISTS id_citation INTEGER REFERENCES table_citations(id_citation)}")
  } else {
    tryCatch({
      DBI::dbExecute(con, sql_add_fk)
      cli::cli_alert_success("Column id_citation added to taxa_traits_measures")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to add id_citation column: {e$message}")
      stop(e)
    })
  }

  # -------------------------------------------------------------------------
  # Step 4: Verify
  # -------------------------------------------------------------------------
  cli::cli_h2("Step 4: Verify migration")

  if (!dry_run) {
    # Check table exists
    tbl_exists <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM information_schema.tables
      WHERE table_name = 'table_citations'
    ")$n > 0

    if (tbl_exists) {
      cli::cli_alert_success("table_citations exists")
    } else {
      cli::cli_alert_danger("table_citations NOT found!")
    }

    # Check FK column exists
    col_exists <- DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n
      FROM information_schema.columns
      WHERE table_name = 'taxa_traits_measures'
        AND column_name = 'id_citation'
    ")$n > 0

    if (col_exists) {
      cli::cli_alert_success("Column id_citation exists on taxa_traits_measures")
    } else {
      cli::cli_alert_danger("Column id_citation NOT found on taxa_traits_measures!")
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


#' Check citations migration status
#'
#' Verifies whether the citations migration has been applied.
#'
#' @param con Database connection to `plots_transects`
#' @return Invisible list with fields `table_exists`, `fk_column_exists`,
#'   `migration_complete`
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' check_citations_migration(con)
#' }
#'
#' @export
check_citations_migration <- function(con) {

  cli::cli_h2("Checking citations migration status")

  result <- list(
    table_exists     = FALSE,
    fk_column_exists = FALSE
  )

  result$table_exists <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n FROM information_schema.tables
      WHERE table_name = 'table_citations'
    ")$n > 0
  }, error = function(e) FALSE)

  if (result$table_exists) {
    cli::cli_alert_success("table_citations exists")
  } else {
    cli::cli_alert_danger("table_citations does NOT exist")
  }

  result$fk_column_exists <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT COUNT(*) AS n FROM information_schema.columns
      WHERE table_name = 'taxa_traits_measures'
        AND column_name = 'id_citation'
    ")$n > 0
  }, error = function(e) FALSE)

  if (result$fk_column_exists) {
    cli::cli_alert_success("Column id_citation exists on taxa_traits_measures")
  } else {
    cli::cli_alert_danger("Column id_citation NOT found on taxa_traits_measures")
  }

  result$migration_complete <- result$table_exists && result$fk_column_exists

  if (result$migration_complete) {
    cli::cli_alert_success("Citations migration is complete")
  } else {
    cli::cli_alert_warning("Citations migration is incomplete - run migrate_add_citations_table(con, dry_run = FALSE)")
  }

  invisible(result)
}
