# Migration: Specimen Links Schema Updates
#
# This migration standardizes the specimen-individual linking system:
# 1. Creates linktypelist lookup table with priority values
# 2. Adds id_linktype FK column to data_link_specimens
# 3. Migrates existing type strings to FK references
# 4. Adds audit columns (created_by, created_at)
#
# Dependencies: DBI, dplyr, cli


#' Create linktypelist Lookup Table
#'
#' Creates the lookup table for specimen link types with priority values.
#' Higher priority = more authoritative link type.
#'
#' @param con Database connection (must have CREATE TABLE privileges)
#' @param dry_run If TRUE, only print SQL without executing
#'
#' @return TRUE if successful
#' @keywords internal
migration_create_linktypelist <- function(con = NULL, dry_run = FALSE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: Create linktypelist lookup table")

  # Check if table already exists
  existing_tables <- DBI::dbListTables(actual_con)
  if ("linktypelist" %in% existing_tables) {
    cli::cli_alert_warning("Table 'linktypelist' already exists. Skipping creation.")

    # Show existing content
    existing <- DBI::dbGetQuery(actual_con, "SELECT * FROM linktypelist ORDER BY priority DESC")
    cli::cli_alert_info("Existing link types:")
    for (i in seq_len(nrow(existing))) {
      cli::cli_li("{existing$linktype[i]} (priority: {existing$priority[i]})")
    }
    return(TRUE)
  }

  # SQL to create table
  sql_create_table <- "
    CREATE TABLE linktypelist (
      id_linktype SERIAL PRIMARY KEY,
      linktype VARCHAR(50) NOT NULL UNIQUE,
      description TEXT,
      priority INTEGER NOT NULL DEFAULT 0
    );
  "

  # SQL to insert default values
  sql_insert_defaults <- "
    INSERT INTO linktypelist (linktype, description, priority) VALUES
      ('type_individual', 'Specimen collected from this specific individual', 100),
      ('referenced_individual', 'Specimen represents same species but from different individual', 50);
  "

  if (dry_run) {
    cli::cli_h2("Dry run - SQL statements that would be executed:")
    cli::cli_code(sql_create_table)
    cli::cli_code(sql_insert_defaults)
    return(TRUE)
  }

  tryCatch({
    cli::cli_alert_info("Creating linktypelist table...")
    DBI::dbExecute(actual_con, sql_create_table)
    cli::cli_alert_success("Table created")

    cli::cli_alert_info("Inserting default link types...")
    DBI::dbExecute(actual_con, sql_insert_defaults)
    cli::cli_alert_success("Default values inserted")

    # Verify
    result <- DBI::dbGetQuery(actual_con, "SELECT * FROM linktypelist ORDER BY priority DESC")
    cli::cli_alert_info("Created {nrow(result)} link types:")
    for (i in seq_len(nrow(result))) {
      cli::cli_li("{result$linktype[i]} (priority: {result$priority[i]})")
    }

    cli::cli_alert_success("Migration Phase 1.1 complete: linktypelist table created")
    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Add id_linktype Column to data_link_specimens
#'
#' Adds the id_linktype FK column and migrates existing type strings to FK references.
#'
#' @param con Database connection
#' @param dry_run If TRUE, only print SQL without executing
#'
#' @return TRUE if successful
#' @keywords internal
migration_add_linktype_column <- function(con = NULL, dry_run = FALSE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: Add id_linktype column to data_link_specimens")

  # Check if linktypelist exists
  existing_tables <- DBI::dbListTables(actual_con)
  if (!"linktypelist" %in% existing_tables) {
    cli::cli_alert_danger("linktypelist table does not exist. Run migration_create_linktypelist() first.")
    stop("linktypelist table required")
  }

  # Check if column already exists
  existing_cols <- DBI::dbListFields(actual_con, "data_link_specimens")
  if ("id_linktype" %in% existing_cols) {
    cli::cli_alert_warning("Column 'id_linktype' already exists.")

    # Check migration status
    null_count <- DBI::dbGetQuery(actual_con,
      "SELECT COUNT(*) as n FROM data_link_specimens WHERE id_linktype IS NULL"
    )$n[1]

    if (null_count > 0) {
      cli::cli_alert_info("{null_count} rows still have NULL id_linktype. Running migration...")
    } else {
      cli::cli_alert_success("All rows already migrated. Nothing to do.")
      return(TRUE)
    }
  }

  # SQL statements
  sql_add_column <- "ALTER TABLE data_link_specimens ADD COLUMN IF NOT EXISTS id_linktype INTEGER;"

  sql_migrate_data <- "
    UPDATE data_link_specimens
    SET id_linktype = (
      SELECT id_linktype FROM linktypelist WHERE linktype = data_link_specimens.type
    )
    WHERE id_linktype IS NULL AND type IS NOT NULL;
  "

  sql_add_fk <- "
    ALTER TABLE data_link_specimens
    DROP CONSTRAINT IF EXISTS fk_linktype;

    ALTER TABLE data_link_specimens
    ADD CONSTRAINT fk_linktype FOREIGN KEY (id_linktype) REFERENCES linktypelist(id_linktype);
  "

  sql_add_index <- "CREATE INDEX IF NOT EXISTS idx_data_link_specimens_linktype ON data_link_specimens(id_linktype);"

  if (dry_run) {
    cli::cli_h2("Dry run - SQL statements that would be executed:")
    cli::cli_code(sql_add_column)
    cli::cli_code(sql_migrate_data)
    cli::cli_code(sql_add_fk)
    cli::cli_code(sql_add_index)

    # Show what would be migrated
    type_counts <- DBI::dbGetQuery(actual_con,
      "SELECT type, COUNT(*) as n FROM data_link_specimens GROUP BY type ORDER BY type"
    )
    cli::cli_alert_info("Current type distribution:")
    for (i in seq_len(nrow(type_counts))) {
      cli::cli_li("{type_counts$type[i]}: {type_counts$n[i]} rows")
    }
    return(TRUE)
  }

  tryCatch({
    # Add column if needed
    if (!"id_linktype" %in% existing_cols) {
      cli::cli_alert_info("Adding id_linktype column...")
      DBI::dbExecute(actual_con, sql_add_column)
      cli::cli_alert_success("Column added")
    }

    # Migrate existing data
    cli::cli_alert_info("Migrating existing type values to id_linktype...")
    n_migrated <- DBI::dbExecute(actual_con, sql_migrate_data)
    cli::cli_alert_success("Migrated {n_migrated} rows")

    # Check for unmapped types
    unmapped <- DBI::dbGetQuery(actual_con,
      "SELECT DISTINCT type FROM data_link_specimens WHERE id_linktype IS NULL AND type IS NOT NULL"
    )
    if (nrow(unmapped) > 0) {
      cli::cli_alert_warning("Found {nrow(unmapped)} unmapped type values:")
      for (t in unmapped$type) {
        cli::cli_li("{t}")
      }
      cli::cli_alert_info("You may need to add these to linktypelist manually")
    }

    # Add FK constraint
    cli::cli_alert_info("Adding foreign key constraint...")
    # Split into two statements for PostgreSQL
    DBI::dbExecute(actual_con, "ALTER TABLE data_link_specimens DROP CONSTRAINT IF EXISTS fk_linktype;")
    DBI::dbExecute(actual_con,
      "ALTER TABLE data_link_specimens ADD CONSTRAINT fk_linktype FOREIGN KEY (id_linktype) REFERENCES linktypelist(id_linktype);"
    )
    cli::cli_alert_success("Foreign key constraint added")

    # Add index
    cli::cli_alert_info("Creating index...")
    DBI::dbExecute(actual_con, sql_add_index)
    cli::cli_alert_success("Index created")

    cli::cli_alert_success("Migration Phase 1.2 complete: id_linktype column added and populated")
    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Add Audit Columns to data_link_specimens
#'
#' Adds created_by and created_at columns for audit trail.
#'
#' @param con Database connection
#' @param dry_run If TRUE, only print SQL without executing
#'
#' @return TRUE if successful
#' @keywords internal
migration_add_audit_columns <- function(con = NULL, dry_run = FALSE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  # Handle pool connections

  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Migration: Add audit columns to data_link_specimens")

  # Check existing columns
  existing_cols <- DBI::dbListFields(actual_con, "data_link_specimens")

  columns_to_add <- c()
  if (!"created_by" %in% existing_cols) {
    columns_to_add <- c(columns_to_add, "created_by")
  }
  if (!"created_at" %in% existing_cols) {
    columns_to_add <- c(columns_to_add, "created_at")
  }


  if (length(columns_to_add) == 0) {
    cli::cli_alert_success("All audit columns already exist. Nothing to do.")
    return(TRUE)
  }

  cli::cli_alert_info("Columns to add: {paste(columns_to_add, collapse = ', ')}")

  # SQL statements
  sql_add_created_by <- "ALTER TABLE data_link_specimens ADD COLUMN IF NOT EXISTS created_by TEXT DEFAULT current_user;"
  sql_add_created_at <- "ALTER TABLE data_link_specimens ADD COLUMN IF NOT EXISTS created_at TIMESTAMP DEFAULT NOW();"

  if (dry_run) {
    cli::cli_h2("Dry run - SQL statements that would be executed:")
    if ("created_by" %in% columns_to_add) cli::cli_code(sql_add_created_by)
    if ("created_at" %in% columns_to_add) cli::cli_code(sql_add_created_at)
    return(TRUE)
  }

  tryCatch({
    if ("created_by" %in% columns_to_add) {
      cli::cli_alert_info("Adding created_by column...")
      DBI::dbExecute(actual_con, sql_add_created_by)
      cli::cli_alert_success("created_by column added")
    }

    if ("created_at" %in% columns_to_add) {
      cli::cli_alert_info("Adding created_at column...")
      DBI::dbExecute(actual_con, sql_add_created_at)
      cli::cli_alert_success("created_at column added")
    }

    cli::cli_alert_success("Migration Phase 1.3 complete: audit columns added")
    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Migration failed: {e$message}")
    stop(e)
  })
}


#' Run Full Specimen Links Migration
#'
#' Runs all three phases of the specimen links migration:
#' 1. Create linktypelist lookup table
#' 2. Add id_linktype column and migrate data
#' 3. Add audit columns
#'
#' @param con Database connection
#' @param dry_run If TRUE, only show what would happen
#'
#' @return List with results from each phase
#' @keywords internal
run_specimen_links_migration <- function(con = NULL, dry_run = TRUE) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  cli::cli_h1("Full Specimen Links Migration")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN MODE - No changes will be made")
  }

  results <- list()

  # Phase 1.1: Create lookup table
  cli::cli_h2("Phase 1.1: Create linktypelist table")
  results$phase1_1 <- migration_create_linktypelist(con, dry_run = dry_run)

  # Phase 1.2: Add id_linktype column
  cli::cli_h2("Phase 1.2: Add id_linktype column")
  results$phase1_2 <- migration_add_linktype_column(con, dry_run = dry_run)

  # Phase 1.3: Add audit columns
  cli::cli_h2("Phase 1.3: Add audit columns")
  results$phase1_3 <- migration_add_audit_columns(con, dry_run = dry_run)

  cli::cli_h1("Migration Complete")

  return(results)
}


#' Verify Specimen Links Migration
#'
#' Checks the integrity of the specimen links migration.
#'
#' @param con Database connection
#'
#' @return Data frame with verification results
#' @export
verify_specimen_links_migration <- function(con = NULL) {
  if (is.null(con)) {
    con <- call.mydb()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  cli::cli_h1("Verifying specimen links migration")

  checks <- list()

  # Check 1: linktypelist exists
  existing_tables <- DBI::dbListTables(actual_con)
  checks$linktypelist_exists <- "linktypelist" %in% existing_tables

  if (checks$linktypelist_exists) {
    cli::cli_alert_success("linktypelist table exists")
    linktypes <- DBI::dbGetQuery(actual_con, "SELECT * FROM linktypelist ORDER BY priority DESC")
    checks$n_linktypes <- nrow(linktypes)
    cli::cli_alert_info("Link types: {paste(linktypes$linktype, collapse = ', ')}")
  } else {
    cli::cli_alert_danger("linktypelist table does NOT exist")
  }

  # Check 2: id_linktype column exists
  existing_cols <- DBI::dbListFields(actual_con, "data_link_specimens")
  checks$id_linktype_exists <- "id_linktype" %in% existing_cols

  if (checks$id_linktype_exists) {
    cli::cli_alert_success("id_linktype column exists")

    # Check migration completeness
    stats <- DBI::dbGetQuery(actual_con, "
      SELECT
        COUNT(*) as total_links,
        COUNT(id_linktype) as with_linktype,
        COUNT(*) - COUNT(id_linktype) as missing_linktype
      FROM data_link_specimens
    ")
    checks$total_links <- stats$total_links[1]
    checks$links_with_linktype <- stats$with_linktype[1]
    checks$links_missing_linktype <- stats$missing_linktype[1]

    cli::cli_alert_info("Total links: {checks$total_links}")
    cli::cli_alert_info("With id_linktype: {checks$links_with_linktype}")
    if (checks$links_missing_linktype > 0) {
      cli::cli_alert_warning("Missing id_linktype: {checks$links_missing_linktype}")
    } else {
      cli::cli_alert_success("All links have id_linktype set")
    }
  } else {
    cli::cli_alert_danger("id_linktype column does NOT exist")
  }

  # Check 3: Audit columns exist
  checks$created_by_exists <- "created_by" %in% existing_cols
  checks$created_at_exists <- "created_at" %in% existing_cols

  if (checks$created_by_exists && checks$created_at_exists) {
    cli::cli_alert_success("Audit columns (created_by, created_at) exist")
  } else {
    cli::cli_alert_warning("Missing audit columns")
  }

  # Check 4: Link type distribution
  if (checks$id_linktype_exists && checks$linktypelist_exists) {
    dist <- DBI::dbGetQuery(actual_con, "
      SELECT lt.linktype, COUNT(*) as n
      FROM data_link_specimens dls
      LEFT JOIN linktypelist lt ON dls.id_linktype = lt.id_linktype
      GROUP BY lt.linktype
      ORDER BY n DESC
    ")
    cli::cli_h3("Link type distribution:")
    for (i in seq_len(nrow(dist))) {
      cli::cli_li("{dist$linktype[i]}: {dist$n[i]}")
    }
  }

  # Summary
  checks$migration_complete <- all(c(
    checks$linktypelist_exists,
    checks$id_linktype_exists,
    checks$created_by_exists,
    checks$created_at_exists
  ))

  if (checks$migration_complete) {
    cli::cli_alert_success("Migration is complete!")
  } else {
    cli::cli_alert_warning("Migration is incomplete - run run_specimen_links_migration()")
  }

  return(as.data.frame(checks))
}
