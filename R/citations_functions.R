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


# =============================================================================
# Phase 3 — CRUD functions
# =============================================================================

#' Query citations from table_citations
#'
#' Returns rows from `table_citations`, optionally filtered by ID, citation key,
#' dataset name, or a free-text pattern matched against `citation_key`, `authors`,
#' `title`, and `dataset_name`.
#'
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param ids Integer vector of `id_citation` values to retrieve.
#' @param keys Character vector of `citation_key` values to retrieve.
#' @param dataset_names Character vector of `dataset_name` values to filter on.
#' @param pattern Character string. Case-insensitive substring matched against
#'   `citation_key`, `authors`, `title`, and `dataset_name`.
#'
#' @return A data frame of matching rows, or all rows when no filter is supplied.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # All citations
#' query_citations(con)
#'
#' # By key
#' query_citations(con, keys = "TRY_v6")
#'
#' # Free-text search
#' query_citations(con, pattern = "TRY")
#' }
#'
#' @export
query_citations <- function(con = NULL,
                            ids           = NULL,
                            keys          = NULL,
                            dataset_names = NULL,
                            pattern       = NULL) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  # Build WHERE clauses
  clauses <- character(0)

  if (!is.null(ids)) {
    ids_sql <- paste(as.integer(ids), collapse = ", ")
    clauses <- c(clauses, paste0("id_citation IN (", ids_sql, ")"))
  }

  if (!is.null(keys)) {
    keys_sql <- paste0("'", gsub("'", "''", keys), "'", collapse = ", ")
    clauses <- c(clauses, paste0("citation_key IN (", keys_sql, ")"))
  }

  if (!is.null(dataset_names)) {
    ds_sql <- paste0("'", gsub("'", "''", dataset_names), "'", collapse = ", ")
    clauses <- c(clauses, paste0("dataset_name IN (", ds_sql, ")"))
  }

  if (!is.null(pattern)) {
    p <- gsub("'", "''", pattern)
    clauses <- c(clauses, paste0(
      "(citation_key ILIKE '%", p, "%'",
      " OR authors ILIKE '%", p, "%'",
      " OR title ILIKE '%", p, "%'",
      " OR dataset_name ILIKE '%", p, "%')"
    ))
  }

  sql <- if (length(clauses) > 0) {
    paste("SELECT * FROM table_citations WHERE", paste(clauses, collapse = " AND "))
  } else {
    "SELECT * FROM table_citations ORDER BY id_citation"
  }

  result <- func_try_fetch(con = actual_con, sql = sql)

  cli::cli_alert_info("{nrow(result)} citation(s) found")
  result
}


#' Add one or more citations to table_citations
#'
#' Inserts new rows into `table_citations`. Rows whose `citation_key` already
#' exists in the database are skipped with a warning.
#'
#' @param new_data Data frame with citation fields. The column `citation_key`
#'   is mandatory. Optional columns: `authors`, `year`, `title`, `journal`,
#'   `volume`, `pages`, `doi`, `url`, `dataset_name`, `notes`.
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param interactive Logical. If TRUE (default), shows a preview and asks for
#'   confirmation before inserting.
#'
#' @return Invisible data frame of actually inserted rows, or NULL if nothing
#'   was inserted.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' add_citation(
#'   data.frame(
#'     citation_key = "TRY_v6",
#'     authors      = "Kattge et al.",
#'     year         = 2020,
#'     title        = "TRY plant trait database - enhanced coverage and open access",
#'     journal      = "Global Change Biology",
#'     doi          = "10.1111/gcb.14904",
#'     dataset_name = "TRY"
#'   ),
#'   con = con
#' )
#' }
#'
#' @export
add_citation <- function(new_data, con = NULL, interactive = TRUE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  if (!"citation_key" %in% names(new_data)) {
    cli::cli_abort("new_data must contain a 'citation_key' column")
  }

  # Add modification date
  today <- Sys.Date()
  new_data$date_modif_d <- as.integer(format(today, "%d"))
  new_data$date_modif_m <- as.integer(format(today, "%m"))
  new_data$date_modif_y <- as.integer(format(today, "%Y"))

  # Check for existing keys
  existing <- tryCatch({
    DBI::dbGetQuery(actual_con,
      "SELECT citation_key FROM table_citations"
    )$citation_key
  }, error = function(e) character(0))

  duplicates <- new_data$citation_key[new_data$citation_key %in% existing]

  if (length(duplicates) > 0) {
    cli::cli_alert_warning(
      "Skipping {length(duplicates)} citation(s) with existing key(s): {paste(duplicates, collapse=', ')}"
    )
    new_data <- new_data[!new_data$citation_key %in% duplicates, ]
  }

  if (nrow(new_data) == 0) {
    cli::cli_alert_info("No new citations to insert")
    return(invisible(NULL))
  }

  # Keep only valid columns
  valid_cols <- c("citation_key", "authors", "year", "title", "journal",
                  "volume", "pages", "doi", "url", "dataset_name", "notes",
                  "date_modif_d", "date_modif_m", "date_modif_y")
  new_data <- new_data[, intersect(names(new_data), valid_cols), drop = FALSE]

  cli::cli_h3("Citations to insert:")
  print(new_data)

  do_insert <- if (interactive) {
    choose_prompt(message = paste0("Confirm inserting ", nrow(new_data), " citation(s)?"))
  } else {
    TRUE
  }

  if (do_insert) {
    tryCatch({
      DBI::dbWriteTable(actual_con, "table_citations", new_data,
                        append = TRUE, row.names = FALSE)
      cli::cli_alert_success("{nrow(new_data)} citation(s) inserted")
    }, error = function(e) {
      cli::cli_alert_danger("Insert failed: {e$message}")
      stop(e)
    })
  } else {
    cli::cli_alert_info("Insert cancelled")
    return(invisible(NULL))
  }

  invisible(new_data)
}


#' Update fields of an existing citation
#'
#' Updates one or more columns of a single row in `table_citations`, identified
#' by its `id_citation`.
#'
#' @param id_citation Integer. The `id_citation` of the row to update.
#' @param fields Named list of field-value pairs to update. Names must be valid
#'   column names of `table_citations` (excluding `id_citation`).
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param execute Logical. If FALSE (default), shows changes without applying
#'   them. Set to TRUE to apply.
#'
#' @return Invisible TRUE on success, or invisible FALSE if not executed.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Preview
#' update_citation(1, list(doi = "10.1111/gcb.14904", url = "https://..."), con)
#'
#' # Apply
#' update_citation(1, list(doi = "10.1111/gcb.14904"), con, execute = TRUE)
#' }
#'
#' @export
update_citation <- function(id_citation, fields, con = NULL, execute = FALSE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  if (!is.numeric(id_citation) || length(id_citation) != 1) {
    cli::cli_abort("id_citation must be a single integer")
  }

  valid_cols <- c("citation_key", "authors", "year", "title", "journal",
                  "volume", "pages", "doi", "url", "dataset_name", "notes",
                  "date_modif_d", "date_modif_m", "date_modif_y")

  invalid <- setdiff(names(fields), valid_cols)
  if (length(invalid) > 0) {
    cli::cli_abort("Invalid field(s): {paste(invalid, collapse=', ')}")
  }

  # Always update modification date
  today <- Sys.Date()
  fields$date_modif_d <- as.integer(format(today, "%d"))
  fields$date_modif_m <- as.integer(format(today, "%m"))
  fields$date_modif_y <- as.integer(format(today, "%Y"))

  # Fetch current values for comparison
  current <- tryCatch({
    DBI::dbGetQuery(actual_con,
      paste0("SELECT * FROM table_citations WHERE id_citation = ", as.integer(id_citation))
    )
  }, error = function(e) {
    cli::cli_abort("Could not fetch current record: {e$message}")
  })

  if (nrow(current) == 0) {
    cli::cli_abort("No citation found with id_citation = {id_citation}")
  }

  # Show comparison
  cli::cli_h3("Proposed changes for id_citation = {id_citation}:")
  for (nm in setdiff(names(fields), c("date_modif_d", "date_modif_m", "date_modif_y"))) {
    old_val <- if (nm %in% names(current)) current[[nm]][1] else NA
    cli::cli_alert_info("{.field {nm}}: {.val {old_val}} -> {.val {fields[[nm]]}}")
  }

  if (!execute) {
    cli::cli_alert_warning("DRY RUN - no changes applied (rerun with execute = TRUE)")
    return(invisible(FALSE))
  }

  # Build SET clause
  set_parts <- mapply(function(col, val) {
    if (is.na(val) || is.null(val)) {
      paste0(col, " = NULL")
    } else if (is.character(val)) {
      paste0(col, " = '", gsub("'", "''", val), "'")
    } else {
      paste0(col, " = ", val)
    }
  }, names(fields), fields, SIMPLIFY = TRUE)

  sql <- paste0(
    "UPDATE table_citations SET ",
    paste(set_parts, collapse = ", "),
    " WHERE id_citation = ", as.integer(id_citation)
  )

  tryCatch({
    DBI::dbExecute(actual_con, sql)
    cli::cli_alert_success("Citation {id_citation} updated")
  }, error = function(e) {
    cli::cli_alert_danger("Update failed: {e$message}")
    stop(e)
  })

  invisible(TRUE)
}


# =============================================================================
# Backfill helpers
# =============================================================================

#' Summarise existing references in taxa_traits_measures
#'
#' Returns a summary of distinct `references` values (and NULL) in
#' `taxa_traits_measures`, with their row counts and current `id_citation`
#' assignment. Use this to identify which groups of rows need a citation
#' backfilled.
#'
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#'
#' @return A data frame with columns `references`, `n_rows`,
#'   `id_citation_assigned` (TRUE/FALSE), `n_with_citation`, `n_without_citation`.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' survey_taxa_traits_references(con)
#' }
#'
#' @export
survey_taxa_traits_references <- function(con = NULL) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  result <- DBI::dbGetQuery(actual_con, "
    SELECT
      COALESCE(reference, '(NULL)') AS reference,
      COUNT(*)                      AS n_rows,
      COUNT(id_citation)            AS n_with_citation,
      COUNT(*) - COUNT(id_citation) AS n_without_citation
    FROM taxa_traits_measures
    GROUP BY reference
    ORDER BY n_rows DESC
  ")

  cli::cli_alert_info(
    "{nrow(result)} distinct reference value(s) found across {sum(result$n_rows)} row(s)"
  )
  cli::cli_alert_info(
    "{sum(result$n_without_citation)} row(s) still have no id_citation"
  )

  result
}


#' Backfill id_citation in taxa_traits_measures
#'
#' Updates `id_citation` for rows whose `reference` column matches a given
#' pattern (exact, partial, or NULL), assigning them to a citation from
#' `table_citations`. Supports dry-run preview before executing.
#'
#' @details
#' The `reference_pattern` argument is matched against the `reference` column
#' using `ILIKE` (case-insensitive partial match) unless `match_null = TRUE`,
#' in which case only rows where `reference IS NULL` are targeted.
#' Set `reference_pattern = NULL` to target **all** rows (use with care).
#'
#' @param citation_key Character. The `citation_key` of the citation to assign.
#'   Must already exist in `table_citations`.
#' @param reference_pattern Character or NULL. Pattern to match against the
#'   `reference` column (ILIKE). NULL targets all rows regardless of reference.
#' @param match_null Logical. If TRUE, targets rows where `reference IS NULL`
#'   (ignores `reference_pattern`). Default FALSE.
#' @param overwrite Logical. If TRUE, also update rows that already have an
#'   `id_citation`. If FALSE (default), only update rows where `id_citation IS NULL`.
#' @param con Database connection to `plots_transects`. If NULL, calls
#'   `call.mydb()`.
#' @param execute Logical. If FALSE (default), shows count of rows that would
#'   be updated without modifying the database.
#'
#' @return Invisible integer: number of rows updated (or that would be updated).
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Preview: how many rows with "TRY" in reference would get this citation?
#' backfill_taxa_traits_citations("TRY_2020", reference_pattern = "TRY", con = con)
#'
#' # Apply
#' backfill_taxa_traits_citations("TRY_2020", reference_pattern = "TRY",
#'                                con = con, execute = TRUE)
#'
#' # Target rows with NULL reference
#' backfill_taxa_traits_citations("CoForTraits_2019", match_null = TRUE,
#'                                con = con, execute = TRUE)
#'
#' # Target all rows (assign one citation to everything)
#' backfill_taxa_traits_citations("CoForTraits_2019", reference_pattern = NULL,
#'                                con = con, execute = TRUE)
#' }
#'
#' @export
backfill_taxa_traits_citations <- function(citation_key,
                                           reference_pattern = NULL,
                                           match_null        = FALSE,
                                           overwrite         = FALSE,
                                           con               = NULL,
                                           execute           = FALSE) {

  if (is.null(con)) con <- call.mydb()

  actual_con <- if (inherits(con, "Pool")) pool::poolCheckout(con) else con
  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
  }, add = TRUE)

  # Resolve citation_key -> id_citation
  cit <- DBI::dbGetQuery(actual_con,
    paste0("SELECT id_citation FROM table_citations WHERE citation_key = '",
           gsub("'", "''", citation_key), "'")
  )

  if (nrow(cit) == 0) {
    cli::cli_abort(
      "citation_key '{citation_key}' not found in table_citations. Run query_citations() to see available keys."
    )
  }

  id_cit <- cit$id_citation[1]

  # Build WHERE clause
  where_parts <- character(0)

  if (match_null) {
    where_parts <- c(where_parts, "reference IS NULL")
  } else if (!is.null(reference_pattern)) {
    p <- gsub("'", "''", reference_pattern)
    where_parts <- c(where_parts, paste0("reference ILIKE '%", p, "%'"))
  }
  # reference_pattern = NULL and match_null = FALSE -> no reference filter (all rows)

  if (!overwrite) {
    where_parts <- c(where_parts, "id_citation IS NULL")
  }

  where_sql <- if (length(where_parts) > 0) {
    paste("WHERE", paste(where_parts, collapse = " AND "))
  } else {
    ""
  }

  # Count rows that would be affected
  count_sql <- paste("SELECT COUNT(*) AS n FROM taxa_traits_measures", where_sql)
  n_rows <- DBI::dbGetQuery(actual_con, count_sql)$n

  cli::cli_alert_info(
    "{n_rows} row(s) would be updated with id_citation = {id_cit} ({citation_key})"
  )

  if (!execute) {
    cli::cli_alert_warning("DRY RUN - no changes applied (rerun with execute = TRUE)")
    return(invisible(n_rows))
  }

  if (n_rows == 0) {
    cli::cli_alert_info("Nothing to update")
    return(invisible(0L))
  }

  update_sql <- paste(
    "UPDATE taxa_traits_measures SET id_citation =", id_cit,
    where_sql
  )

  tryCatch({
    n_updated <- DBI::dbExecute(actual_con, update_sql)
    cli::cli_alert_success("{n_updated} row(s) updated with citation '{citation_key}'")
  }, error = function(e) {
    cli::cli_alert_danger("Update failed: {e$message}")
    stop(e)
  })

  invisible(n_rows)
}
