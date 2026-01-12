# Migration Script: Convert table_idtax to Materialized View
#
# This script converts the regular table_idtax table to a PostgreSQL materialized
# view to allow designated users to refresh taxonomy mappings without requiring
# full admin permissions.
#
# IMPORTANT: Run this function as database admin/superuser
# IMPORTANT: This is a ONE-TIME migration script


#' Migrate table_idtax to Materialized View
#'
#' Converts table_idtax from a regular table to a PostgreSQL materialized view.
#' This is a ONE-TIME migration that should only be run by database admin.
#'
#' After migration:
#' - Users with data_manager_role can refresh the view
#' - Automatic staleness tracking via metadata table
#' - Optional automatic monthly refresh (if pg_cron available)
#'
#' @param con Main database connection (must have admin/superuser privileges)
#' @param con_taxa Taxa database connection (for initial data fetch)
#' @param data_manager_users Character vector of usernames to grant refresh permission.
#'   Default: c("dauby", "alex", "libalah")
#' @param setup_pg_cron Logical, attempt to set up automatic monthly refresh
#'   using pg_cron extension. Default FALSE. Requires pg_cron extension installed.
#' @param dry_run Logical, if TRUE only prints SQL commands without executing.
#'   Default FALSE.
#'
#' @return List with migration results and any errors
#'
#' @details
#' This function performs the following steps:
#' 1. Creates backup table (table_idtax_backup)
#' 2. Drops existing table_idtax
#' 3. Creates materialized view table_idtax
#' 4. Creates unique indexes
#' 5. Creates metadata tracking table
#' 6. Creates PostgreSQL functions for refresh and staleness checking
#' 7. Creates data_manager_role and grants permissions
#' 8. Grants role to specified users
#' 9. Optionally sets up pg_cron automatic refresh
#'
#' ROLLBACK: If migration fails, restore from backup using:
#'   rollback_table_idtax_migration(con)
#'
#' @examples
#' \dontrun{
#' # Connect as admin
#' con <- call.mydb()  # Use admin credentials
#' con_taxa <- call.mydb.taxa()
#'
#' # Dry run first (just show commands)
#' migrate_table_idtax_to_materialized_view(con, con_taxa, dry_run = TRUE)
#'
#' # Actual migration
#' result <- migrate_table_idtax_to_materialized_view(
#'   con,
#'   con_taxa,
#'   data_manager_users = c("dauby", "alex", "libalah")
#' )
#'
#' # With automatic refresh setup
#' result <- migrate_table_idtax_to_materialized_view(
#'   con,
#'   con_taxa,
#'   data_manager_users = c("dauby", "alex", "libalah"),
#'   setup_pg_cron = TRUE
#' )
#' }
#'
#' @export
migrate_table_idtax_to_materialized_view <- function(
    con,
    con_taxa = NULL,
    data_manager_users = c("dauby", "alex", "libalah"),
    setup_pg_cron = FALSE,
    dry_run = FALSE
) {

  if (is.null(con_taxa)) {
    con_taxa <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  actual_con_taxa <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
    if (inherits(con_taxa, "Pool") && !is.null(actual_con_taxa)) {
      pool::poolReturn(actual_con_taxa)
    }
  }, add = TRUE)

  results <- list(
    success = FALSE,
    steps_completed = character(),
    errors = character(),
    warnings = character()
  )

  cli::cli_h1("Migrating table_idtax to Materialized View")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN MODE - Commands will be printed but not executed")
  }

  # Helper function to execute SQL
  exec_sql <- function(sql, description, critical = TRUE) {
    cli::cli_alert_info(description)

    if (dry_run) {
      cli::cli_alert_info("SQL: {sql}")
      return(TRUE)
    }

    tryCatch({
      DBI::dbExecute(actual_con, sql)
      results$steps_completed <<- c(results$steps_completed, description)
      cli::cli_alert_success("✓ {description}")
      return(TRUE)
    }, error = function(e) {
      msg <- paste0(description, ": ", e$message)
      if (critical) {
        results$errors <<- c(results$errors, msg)
        cli::cli_alert_danger("✗ {msg}")
        stop(msg)
      } else {
        results$warnings <<- c(results$warnings, msg)
        cli::cli_alert_warning("⚠ {msg}")
        return(FALSE)
      }
    })
  }

  # Helper function to execute query
  exec_query <- function(sql, description) {
    if (dry_run) {
      cli::cli_alert_info("QUERY: {sql}")
      return(data.frame())
    }

    tryCatch({
      DBI::dbGetQuery(actual_con, sql)
    }, error = function(e) {
      msg <- paste0(description, ": ", e$message)
      results$warnings <<- c(results$warnings, msg)
      cli::cli_alert_warning("⚠ {msg}")
      return(data.frame())
    })
  }


  # =========================================================================
  # STEP 1: Create Backup
  # =========================================================================
  cli::cli_h2("Step 1: Create Backup")

  # Check if backup already exists
  backup_exists <- exec_query(
    "SELECT COUNT(*) as n FROM information_schema.tables WHERE table_name = 'table_idtax_backup';",
    "Check backup existence"
  )

  if (!dry_run && nrow(backup_exists) > 0 && backup_exists$n[1] > 0) {
    cli::cli_alert_warning("Backup table already exists. Dropping old backup...")
    exec_sql(
      "DROP TABLE IF EXISTS table_idtax_backup;",
      "Drop old backup",
      critical = FALSE
    )
  }

  exec_sql(
    "CREATE TABLE table_idtax_backup AS SELECT * FROM table_idtax;",
    "Create backup table"
  )

  # Verify backup
  if (!dry_run) {
    backup_count <- exec_query("SELECT COUNT(*) as n FROM table_idtax_backup;", "Count backup rows")
    original_count <- exec_query("SELECT COUNT(*) as n FROM table_idtax;", "Count original rows")

    if (nrow(backup_count) > 0 && nrow(original_count) > 0) {
      cli::cli_alert_success("Backup created: {backup_count$n[1]} rows (original: {original_count$n[1]})")

      if (backup_count$n[1] != original_count$n[1]) {
        stop("Backup verification failed: row count mismatch!")
      }
    }
  }


  # =========================================================================
  # STEP 2: Drop Existing Table and Create Materialized View
  # =========================================================================
  cli::cli_h2("Step 2: Create Materialized View")

  exec_sql(
    "DROP TABLE IF EXISTS table_idtax CASCADE;",
    "Drop existing table"
  )

  # Fetch fresh data from taxa database
  if (!dry_run) {
    cli::cli_alert_info("Fetching taxonomy data from taxa database...")
    id_taxa_table <- try_open_postgres_table(table = "table_taxa", con = actual_con_taxa) %>%
      dplyr::select(idtax_n, idtax_good_n) %>%
      dplyr::collect()

    cli::cli_alert_success("Fetched {nrow(id_taxa_table)} taxonomy records")

    # Create temporary table with fresh data
    DBI::dbWriteTable(actual_con, "table_idtax_temp", id_taxa_table, overwrite = TRUE)
  }

  # Create materialized view from temporary table (or backup if dry run)
  source_table <- if (dry_run) "table_idtax_backup" else "table_idtax_temp"

  exec_sql(
    sprintf("CREATE MATERIALIZED VIEW table_idtax AS
             SELECT idtax_n::INTEGER, idtax_good_n::INTEGER
             FROM %s;", source_table),
    "Create materialized view"
  )

  # Add comment
  exec_sql(
    "COMMENT ON MATERIALIZED VIEW table_idtax IS
     'Materialized view containing taxonomy ID mappings for synonym resolution.
     Refresh using: SELECT refresh_table_idtax();
     Last updated: check table_idtax_metadata';",
    "Add comment to materialized view",
    critical = FALSE
  )

  # Clean up temp table
  if (!dry_run) {
    exec_sql("DROP TABLE IF EXISTS table_idtax_temp;", "Clean up temporary table", critical = FALSE)
  }


  # =========================================================================
  # STEP 3: Create Indexes
  # =========================================================================
  cli::cli_h2("Step 3: Create Indexes")

  exec_sql(
    "CREATE UNIQUE INDEX idx_table_idtax_idtax_n ON table_idtax(idtax_n);",
    "Create unique index on idtax_n (required for CONCURRENTLY refresh)"
  )

  exec_sql(
    "CREATE INDEX idx_table_idtax_good_n ON table_idtax(idtax_good_n);",
    "Create index on idtax_good_n",
    critical = FALSE
  )


  # =========================================================================
  # STEP 4: Create Metadata Table
  # =========================================================================
  cli::cli_h2("Step 4: Create Metadata Table")

  exec_sql(
    "CREATE TABLE IF NOT EXISTS table_idtax_metadata (
       table_name VARCHAR(100) PRIMARY KEY,
       last_updated TIMESTAMP WITH TIME ZONE NOT NULL,
       updated_by VARCHAR(100),
       record_count INTEGER,
       source_info TEXT,
       notes TEXT
     );",
    "Create metadata table"
  )

  exec_sql(
    "COMMENT ON TABLE table_idtax_metadata IS
     'Tracks when table_idtax materialized view was last refreshed';",
    "Add comment to metadata table",
    critical = FALSE
  )

  # Insert initial metadata
  if (!dry_run) {
    record_count <- exec_query("SELECT COUNT(*) as n FROM table_idtax;", "Get record count")
    count_val <- if (nrow(record_count) > 0) as.integer(record_count$n[1]) else 0L

    exec_sql(
      sprintf("INSERT INTO table_idtax_metadata
               (table_name, last_updated, updated_by, record_count, source_info)
               VALUES ('table_idtax', CURRENT_TIMESTAMP, '%s', %d,
                       'Initial migration from table to materialized view')
               ON CONFLICT (table_name) DO UPDATE SET
                 last_updated = CURRENT_TIMESTAMP,
                 updated_by = '%s',
                 record_count = %d,
                 source_info = 'Initial migration from table to materialized view';",
              Sys.info()["user"], count_val, Sys.info()["user"], count_val),
      "Insert initial metadata"
    )
  }


  # =========================================================================
  # STEP 5: Create PostgreSQL Functions
  # =========================================================================
  cli::cli_h2("Step 5: Create PostgreSQL Functions")

  # Function: refresh_table_idtax()
  refresh_func_sql <- "
CREATE OR REPLACE FUNCTION refresh_table_idtax()
RETURNS TABLE(
    success BOOLEAN,
    message TEXT,
    record_count INTEGER,
    refresh_duration INTERVAL
)
SECURITY DEFINER
LANGUAGE plpgsql
AS $$
DECLARE
    start_time TIMESTAMP;
    end_time TIMESTAMP;
    row_count INTEGER;
BEGIN
    start_time := clock_timestamp();

    REFRESH MATERIALIZED VIEW CONCURRENTLY table_idtax;

    end_time := clock_timestamp();
    SELECT COUNT(*) INTO row_count FROM table_idtax;

    INSERT INTO table_idtax_metadata
        (table_name, last_updated, updated_by, record_count, source_info)
    VALUES
        ('table_idtax', CURRENT_TIMESTAMP, CURRENT_USER, row_count,
         'Refreshed via refresh_table_idtax() function')
    ON CONFLICT (table_name) DO UPDATE
    SET
        last_updated = CURRENT_TIMESTAMP,
        updated_by = CURRENT_USER,
        record_count = row_count,
        source_info = 'Refreshed via refresh_table_idtax() function';

    RETURN QUERY SELECT
        TRUE::BOOLEAN as success,
        format('Successfully refreshed table_idtax with %s records', row_count)::TEXT as message,
        row_count as record_count,
        (end_time - start_time) as refresh_duration;

EXCEPTION WHEN OTHERS THEN
    RETURN QUERY SELECT
        FALSE::BOOLEAN as success,
        format('Error refreshing table_idtax: %s', SQLERRM)::TEXT as message,
        NULL::INTEGER as record_count,
        NULL::INTERVAL as refresh_duration;
END;
$$;
"

  exec_sql(refresh_func_sql, "Create refresh_table_idtax() function")

  exec_sql(
    "COMMENT ON FUNCTION refresh_table_idtax() IS
     'Refreshes the table_idtax materialized view and updates metadata.
     Can be called by users granted EXECUTE permission.';",
    "Add comment to refresh function",
    critical = FALSE
  )


  # Function: check_table_idtax_staleness()
  staleness_func_sql <- "
CREATE OR REPLACE FUNCTION check_table_idtax_staleness(warn_days INTEGER DEFAULT 90)
RETURNS TABLE(
    is_stale BOOLEAN,
    days_old NUMERIC,
    last_updated TIMESTAMP WITH TIME ZONE,
    message TEXT
)
LANGUAGE plpgsql
AS $$
DECLARE
    meta_record RECORD;
    age_days NUMERIC;
BEGIN
    SELECT * INTO meta_record
    FROM table_idtax_metadata
    WHERE table_name = 'table_idtax';

    IF NOT FOUND THEN
        RETURN QUERY SELECT
            TRUE::BOOLEAN,
            NULL::NUMERIC,
            NULL::TIMESTAMP WITH TIME ZONE,
            'No metadata found for table_idtax'::TEXT;
        RETURN;
    END IF;

    age_days := EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - meta_record.last_updated)) / 86400;

    RETURN QUERY SELECT
        (age_days > warn_days)::BOOLEAN,
        ROUND(age_days, 1),
        meta_record.last_updated,
        CASE
            WHEN age_days > warn_days THEN
                format('WARNING: table_idtax is %.1f days old (threshold: %s days). Consider refreshing.',
                       age_days, warn_days)
            ELSE
                format('table_idtax is up to date (%.1f days old)', age_days)
        END::TEXT;
END;
$$;
"

  exec_sql(staleness_func_sql, "Create check_table_idtax_staleness() function")

  exec_sql(
    "COMMENT ON FUNCTION check_table_idtax_staleness(INTEGER) IS
     'Checks if table_idtax needs refreshing based on age threshold (default 90 days)';",
    "Add comment to staleness check function",
    critical = FALSE
  )


  # =========================================================================
  # STEP 6: Create Role and Grant Permissions
  # =========================================================================
  cli::cli_h2("Step 6: Create Role and Grant Permissions")

  # Create role
  exec_sql(
    "DO $$
     BEGIN
         IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'data_manager_role') THEN
             CREATE ROLE data_manager_role;
         END IF;
     END $$;",
    "Create data_manager_role (if not exists)",
    critical = FALSE
  )

  # Grant permissions
  exec_sql(
    "GRANT SELECT ON table_idtax TO public;",
    "Grant SELECT on table_idtax to public"
  )

  exec_sql(
    "GRANT SELECT ON table_idtax TO data_manager_role;",
    "Grant SELECT on table_idtax to data_manager_role"
  )

  exec_sql(
    "GRANT SELECT ON table_idtax_metadata TO public;",
    "Grant SELECT on metadata table to public"
  )

  exec_sql(
    "GRANT INSERT, UPDATE ON table_idtax_metadata TO data_manager_role;",
    "Grant INSERT, UPDATE on metadata table to data_manager_role"
  )

  exec_sql(
    "GRANT EXECUTE ON FUNCTION refresh_table_idtax() TO data_manager_role;",
    "Grant EXECUTE on refresh function to data_manager_role"
  )

  exec_sql(
    "GRANT EXECUTE ON FUNCTION check_table_idtax_staleness(INTEGER) TO public;",
    "Grant EXECUTE on staleness check function to public"
  )


  # =========================================================================
  # STEP 7: Grant Role to Specific Users
  # =========================================================================
  cli::cli_h2("Step 7: Grant Role to Users")

  for (username in data_manager_users) {
    # Check if user exists first
    user_exists <- exec_query(
      sprintf("SELECT COUNT(*) as n FROM pg_roles WHERE rolname = '%s';", username),
      sprintf("Check if user '%s' exists", username)
    )

    if (!dry_run && nrow(user_exists) > 0 && user_exists$n[1] == 0) {
      cli::cli_alert_warning("User '{username}' does not exist in database - skipping")
      results$warnings <- c(results$warnings, paste0("User not found: ", username))
      next
    }

    exec_sql(
      sprintf("GRANT data_manager_role TO %s;", username),
      sprintf("Grant data_manager_role to user '%s'", username),
      critical = FALSE
    )
  }


  # =========================================================================
  # STEP 8: Setup Automatic Refresh (Optional)
  # =========================================================================
  if (setup_pg_cron) {
    cli::cli_h2("Step 8: Setup Automatic Refresh (pg_cron)")

    # Check if pg_cron is installed
    pg_cron_exists <- exec_query(
      "SELECT COUNT(*) as n FROM pg_extension WHERE extname = 'pg_cron';",
      "Check if pg_cron extension is installed"
    )

    if (!dry_run && nrow(pg_cron_exists) > 0 && pg_cron_exists$n[1] > 0) {
      cli::cli_alert_success("pg_cron extension is installed")

      # Schedule monthly refresh (first day of month at 2 AM)
      exec_sql(
        "SELECT cron.schedule(
           'refresh-table-idtax',
           '0 2 1 * *',
           'SELECT refresh_table_idtax();'
         );",
        "Schedule monthly automatic refresh",
        critical = FALSE
      )

      cli::cli_alert_success("Scheduled automatic refresh: 1st of month at 2:00 AM")
    } else {
      cli::cli_alert_warning("pg_cron extension not installed - automatic refresh not scheduled")
      cli::cli_alert_info("To install pg_cron, database superuser must run: CREATE EXTENSION pg_cron;")
      results$warnings <- c(results$warnings, "pg_cron not available")
    }
  }


  # =========================================================================
  # STEP 9: Verification
  # =========================================================================
  if (!dry_run) {
    cli::cli_h2("Step 9: Verification")

    # Check materialized view exists
    mv_check <- exec_query(
      "SELECT schemaname, matviewname, matviewowner, ispopulated
       FROM pg_matviews
       WHERE matviewname = 'table_idtax';",
      "Check materialized view exists"
    )

    if (nrow(mv_check) > 0) {
      cli::cli_alert_success("Materialized view exists: {mv_check$matviewname[1]}")
      cli::cli_alert_info("Owner: {mv_check$matviewowner[1]}, Populated: {mv_check$ispopulated[1]}")
    }

    # Check record count
    count_check <- exec_query("SELECT COUNT(*) as n FROM table_idtax;", "Count records")
    if (nrow(count_check) > 0) {
      cli::cli_alert_success("Record count: {count_check$n[1]}")
    }

    # Check metadata
    meta_check <- exec_query(
      "SELECT * FROM table_idtax_metadata WHERE table_name = 'table_idtax';",
      "Check metadata"
    )
    if (nrow(meta_check) > 0) {
      cli::cli_alert_success("Metadata last updated: {meta_check$last_updated[1]}")
    }

    # Check staleness function
    staleness_check <- exec_query(
      "SELECT * FROM check_table_idtax_staleness(90);",
      "Test staleness check function"
    )
    if (nrow(staleness_check) > 0) {
      cli::cli_alert_success("Staleness check: {staleness_check$message[1]}")
    }

    # Check role members
    role_members <- exec_query(
      "SELECT r.rolname as role, m.rolname as member
       FROM pg_roles r
       JOIN pg_auth_members am ON r.oid = am.roleid
       JOIN pg_roles m ON am.member = m.oid
       WHERE r.rolname = 'data_manager_role';",
      "Check role members"
    )
    if (nrow(role_members) > 0) {
      cli::cli_alert_success("data_manager_role members: {paste(role_members$member, collapse=', ')}")
    }
  }


  # =========================================================================
  # Success Summary
  # =========================================================================
  cli::cli_h2("Migration Complete!")

  results$success <- TRUE

  cli::cli_alert_success("Successfully migrated table_idtax to materialized view")
  cli::cli_alert_info("Steps completed: {length(results$steps_completed)}")

  if (length(results$warnings) > 0) {
    cli::cli_alert_warning("Warnings encountered: {length(results$warnings)}")
    for (w in results$warnings) {
      cli::cli_alert_warning("  - {w}")
    }
  }

  if (!dry_run) {
    cli::cli_rule()
    cli::cli_alert_info("Next steps:")
    cli::cli_ul(c(
      "Update R package functions to use new approach",
      "Test with a data manager user account",
      "Notify users of the change",
      "Consider setting up automatic refresh if pg_cron is available"
    ))
    cli::cli_rule()
  }

  return(invisible(results))
}


#' Rollback table_idtax Migration
#'
#' Reverts the materialized view migration and restores table_idtax as a
#' regular table from the backup.
#'
#' @param con Database connection (must have admin privileges)
#'
#' @return Logical, TRUE if rollback successful
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()  # Admin credentials
#' rollback_table_idtax_migration(con)
#' }
#'
#' @export
rollback_table_idtax_migration <- function(con) {

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

  cli::cli_h1("Rolling Back table_idtax Migration")
  cli::cli_alert_warning("This will restore table_idtax to a regular table")

  tryCatch({
    # Drop materialized view
    cli::cli_alert_info("Dropping materialized view...")
    DBI::dbExecute(actual_con, "DROP MATERIALIZED VIEW IF EXISTS table_idtax CASCADE;")

    # Drop metadata table
    cli::cli_alert_info("Dropping metadata table...")
    DBI::dbExecute(actual_con, "DROP TABLE IF EXISTS table_idtax_metadata;")

    # Drop functions
    cli::cli_alert_info("Dropping functions...")
    DBI::dbExecute(actual_con, "DROP FUNCTION IF EXISTS refresh_table_idtax();")
    DBI::dbExecute(actual_con, "DROP FUNCTION IF EXISTS check_table_idtax_staleness(INTEGER);")

    # Restore from backup
    cli::cli_alert_info("Restoring from backup...")
    DBI::dbExecute(actual_con, "CREATE TABLE table_idtax AS SELECT * FROM table_idtax_backup;")

    # Create indexes
    cli::cli_alert_info("Creating indexes...")
    DBI::dbExecute(actual_con, "CREATE INDEX idx_table_idtax_idtax_n ON table_idtax(idtax_n);")
    DBI::dbExecute(actual_con, "CREATE INDEX idx_table_idtax_good_n ON table_idtax(idtax_good_n);")

    # Grant permissions
    cli::cli_alert_info("Granting permissions...")
    DBI::dbExecute(actual_con, "GRANT SELECT ON table_idtax TO public;")

    # Verify
    count <- DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM table_idtax;")
    cli::cli_alert_success("Rollback complete! Record count: {count$n[1]}")

    cli::cli_alert_info("Note: data_manager_role and user grants remain - clean up manually if desired")

    return(TRUE)

  }, error = function(e) {
    cli::cli_alert_danger("Rollback failed: {e$message}")
    return(FALSE)
  })
}


#' Test table_idtax Materialized View Setup
#'
#' Tests whether the materialized view migration was successful by checking
#' all components are in place.
#'
#' @param con Database connection
#'
#' @return List with test results
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' test_table_idtax_migration(con)
#' }
#'
#' @export
test_table_idtax_migration <- function(con) {

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

  cli::cli_h1("Testing table_idtax Materialized View Setup")

  results <- list(
    materialized_view_exists = FALSE,
    metadata_table_exists = FALSE,
    refresh_function_exists = FALSE,
    staleness_function_exists = FALSE,
    role_exists = FALSE,
    can_refresh = FALSE,
    record_count = 0
  )

  # Test 1: Materialized view exists
  mv_check <- tryCatch({
    DBI::dbGetQuery(
      actual_con,
      "SELECT COUNT(*) as n FROM pg_matviews WHERE matviewname = 'table_idtax';"
    )
  }, error = function(e) data.frame(n = 0))

  results$materialized_view_exists <- (nrow(mv_check) > 0 && mv_check$n[1] > 0)
  if (results$materialized_view_exists) {
    cli::cli_alert_success("✓ Materialized view exists")
    # Get record count
    count <- DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM table_idtax;")
    results$record_count <- count$n[1]
    cli::cli_alert_info("  Record count: {results$record_count}")
  } else {
    cli::cli_alert_danger("✗ Materialized view NOT found")
  }

  # Test 2: Metadata table exists
  meta_check <- tryCatch({
    DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM table_idtax_metadata;")
  }, error = function(e) data.frame(n = 0))

  results$metadata_table_exists <- (nrow(meta_check) > 0)
  if (results$metadata_table_exists) {
    cli::cli_alert_success("✓ Metadata table exists")
  } else {
    cli::cli_alert_danger("✗ Metadata table NOT found")
  }

  # Test 3: refresh_table_idtax function exists
  refresh_check <- tryCatch({
    DBI::dbGetQuery(
      actual_con,
      "SELECT COUNT(*) as n FROM pg_proc WHERE proname = 'refresh_table_idtax';"
    )
  }, error = function(e) data.frame(n = 0))

  results$refresh_function_exists <- (nrow(refresh_check) > 0 && refresh_check$n[1] > 0)
  if (results$refresh_function_exists) {
    cli::cli_alert_success("✓ refresh_table_idtax() function exists")
  } else {
    cli::cli_alert_danger("✗ refresh_table_idtax() function NOT found")
  }

  # Test 4: staleness check function exists
  staleness_check <- tryCatch({
    DBI::dbGetQuery(
      actual_con,
      "SELECT COUNT(*) as n FROM pg_proc WHERE proname = 'check_table_idtax_staleness';"
    )
  }, error = function(e) data.frame(n = 0))

  results$staleness_function_exists <- (nrow(staleness_check) > 0 && staleness_check$n[1] > 0)
  if (results$staleness_function_exists) {
    cli::cli_alert_success("✓ check_table_idtax_staleness() function exists")

    # Test the function
    staleness <- tryCatch({
      DBI::dbGetQuery(actual_con, "SELECT * FROM check_table_idtax_staleness(90);")
    }, error = function(e) data.frame())

    if (nrow(staleness) > 0) {
      cli::cli_alert_info("  {staleness$message[1]}")
    }
  } else {
    cli::cli_alert_danger("✗ check_table_idtax_staleness() function NOT found")
  }

  # Test 5: data_manager_role exists
  role_check <- tryCatch({
    DBI::dbGetQuery(
      actual_con,
      "SELECT COUNT(*) as n FROM pg_roles WHERE rolname = 'data_manager_role';"
    )
  }, error = function(e) data.frame(n = 0))

  results$role_exists <- (nrow(role_check) > 0 && role_check$n[1] > 0)
  if (results$role_exists) {
    cli::cli_alert_success("✓ data_manager_role exists")

    # List members
    members <- tryCatch({
      DBI::dbGetQuery(
        actual_con,
        "SELECT r.rolname as role, m.rolname as member
         FROM pg_roles r
         JOIN pg_auth_members am ON r.oid = am.roleid
         JOIN pg_roles m ON am.member = m.oid
         WHERE r.rolname = 'data_manager_role';"
      )
    }, error = function(e) data.frame())

    if (nrow(members) > 0) {
      cli::cli_alert_info("  Members: {paste(members$member, collapse=', ')}")
    }
  } else {
    cli::cli_alert_danger("✗ data_manager_role NOT found")
  }

  # Test 6: Can current user refresh?
  can_refresh <- tryCatch({
    DBI::dbGetQuery(actual_con, "SELECT * FROM refresh_table_idtax();")
    TRUE
  }, error = function(e) {
    if (grepl("permission denied", e$message, ignore.case = TRUE)) {
      cli::cli_alert_warning("⚠ Current user cannot refresh (no permission)")
      FALSE
    } else {
      cli::cli_alert_warning("⚠ Could not test refresh: {e$message}")
      FALSE
    }
  })

  results$can_refresh <- can_refresh
  if (can_refresh) {
    cli::cli_alert_success("✓ Current user CAN refresh materialized view")
  }

  # Summary
  cli::cli_rule()
  all_pass <- all(
    results$materialized_view_exists,
    results$metadata_table_exists,
    results$refresh_function_exists,
    results$staleness_function_exists,
    results$role_exists
  )

  if (all_pass) {
    cli::cli_alert_success("All tests passed! Migration successful.")
  } else {
    cli::cli_alert_warning("Some tests failed. Review results above.")
  }

  return(invisible(results))
}
