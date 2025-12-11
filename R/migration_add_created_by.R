#' Migration: Add created_by Column for Row-Level Security
#'
#' This migration adds a `created_by` column to `data_liste_plots` table
#' that enables automatic access for plot creators. After this migration,
#' users will automatically have SELECT, UPDATE, DELETE access to plots
#' they create.
#'
#' @details
#' This migration:
#' 1. Adds `created_by` column with DEFAULT current_user
#' 2. Backfills existing plots to a specified admin user
#' 3. Creates global RLS policies for creator access
#' 4. Keeps INSERT open for all users
#'
#' After migration, access works as follows:
#' - INSERT: Anyone can insert (policy: WITH CHECK true)
#' - SELECT/UPDATE/DELETE own plots: Via created_by = current_user
#' - SELECT/UPDATE/DELETE others' plots: Via explicit grants (define_user_policy)
#'
#' @param con Database connection (must have admin privileges)
#' @param backfill_user Username to assign as creator for existing plots (default: "dauby")
#' @param dry_run If TRUE, only print SQL without executing (default: FALSE)
#' @return Invisible TRUE on success
#'
#' @examples
#' \dontrun{
#' # Connect as admin
#' con <- call.mydb(user = "admin", password = "xxx")
#'
#' # Preview the migration
#' migrate_add_created_by(con, dry_run = TRUE)
#'
#' # Run the migration
#' migrate_add_created_by(con, backfill_user = "dauby")
#' }
#'
#' @export
migrate_add_created_by <- function(con, backfill_user = "dauby", dry_run = FALSE) {

  cli::cli_h1("Migration: Add created_by Column for RLS")

  # Verify connection
  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Invalid database connection")
  }

  # Check if user has admin privileges (can create policies)
  cli::cli_alert_info("Checking admin privileges...")

  # Step 1: Add created_by column
  cli::cli_h2("Step 1: Add created_by column")

  sql_add_column <- "
    ALTER TABLE data_liste_plots
    ADD COLUMN IF NOT EXISTS created_by TEXT DEFAULT current_user;
  "

  if (dry_run) {
    cli::cli_alert_info("Would execute: {.code {trimws(sql_add_column)}}")
  } else {
    tryCatch({
      DBI::dbExecute(con, sql_add_column)
      cli::cli_alert_success("Column 'created_by' added (or already exists)")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to add column: {e$message}")
      stop(e)
    })
  }

  # Step 2: Backfill existing rows
  cli::cli_h2("Step 2: Backfill existing plots")

  # Count rows to update
  if (!dry_run) {
    n_null <- DBI::dbGetQuery(con,
      "SELECT COUNT(*) as n FROM data_liste_plots WHERE created_by IS NULL"
    )$n
    cli::cli_alert_info("Found {n_null} plots with NULL created_by")
  }

  sql_backfill <- sprintf("
    UPDATE data_liste_plots
    SET created_by = '%s'
    WHERE created_by IS NULL;
  ", backfill_user)

  if (dry_run) {
    cli::cli_alert_info("Would execute: {.code UPDATE data_liste_plots SET created_by = '{backfill_user}' WHERE created_by IS NULL}")
  } else {
    tryCatch({
      n_updated <- DBI::dbExecute(con, sql_backfill)
      cli::cli_alert_success("Updated {n_updated} plots with created_by = '{backfill_user}'")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to backfill: {e$message}")
      stop(e)
    })
  }

  # Step 3: Create global creator access policies
  cli::cli_h2("Step 3: Create creator access policies")

  policies <- list(
    list(
      name = "creator_access_select",
      cmd = "SELECT",
      clause = "USING (created_by = current_user)"
    ),
    list(
      name = "creator_access_update",
      cmd = "UPDATE",
      clause = "USING (created_by = current_user)"
    ),
    list(
      name = "creator_access_delete",
      cmd = "DELETE",
      clause = "USING (created_by = current_user)"
    ),
    list(
      name = "insert_open",
      cmd = "INSERT",
      clause = "WITH CHECK (true)"
    )
  )

  for (policy in policies) {
    sql_drop <- sprintf("DROP POLICY IF EXISTS %s ON data_liste_plots;", policy$name)
    sql_create <- sprintf("
      CREATE POLICY %s ON data_liste_plots
      FOR %s
      TO PUBLIC
      %s;
    ", policy$name, policy$cmd, policy$clause)

    if (dry_run) {
      cli::cli_alert_info("Would create policy: {.strong {policy$name}} FOR {policy$cmd}")
    } else {
      tryCatch({
        DBI::dbExecute(con, sql_drop)
        DBI::dbExecute(con, sql_create)
        cli::cli_alert_success("Policy '{policy$name}' created for {policy$cmd}")
      }, error = function(e) {
        cli::cli_alert_danger("Failed to create policy '{policy$name}': {e$message}")
        stop(e)
      })
    }
  }

  # Step 4: Ensure RLS is enabled
  cli::cli_h2("Step 4: Enable Row Level Security")

  sql_enable_rls <- "ALTER TABLE data_liste_plots ENABLE ROW LEVEL SECURITY;"

  if (dry_run) {
    cli::cli_alert_info("Would execute: {.code ALTER TABLE data_liste_plots ENABLE ROW LEVEL SECURITY}")
  } else {
    tryCatch({
      DBI::dbExecute(con, sql_enable_rls)
      cli::cli_alert_success("Row Level Security enabled on data_liste_plots")
    }, error = function(e) {
      # May already be enabled
      cli::cli_alert_warning("RLS enable warning (may already be enabled): {e$message}")
    })
  }

  # Step 5: Verify setup
  cli::cli_h2("Step 5: Verify migration")

  if (!dry_run) {
    # Check column exists
    columns <- DBI::dbGetQuery(con, "
      SELECT column_name, data_type, column_default
      FROM information_schema.columns
      WHERE table_name = 'data_liste_plots' AND column_name = 'created_by'
    ")

    if (nrow(columns) > 0) {
      cli::cli_alert_success("Column 'created_by' exists with default: {columns$column_default}")
    } else {
      cli::cli_alert_danger("Column 'created_by' NOT found!")
    }

    # List all policies
    policies_df <- DBI::dbGetQuery(con, "
      SELECT policyname, cmd, roles::text, qual, with_check
      FROM pg_policies
      WHERE tablename = 'data_liste_plots'
      ORDER BY policyname
    ")

    cli::cli_h3("Current policies on data_liste_plots:")
    if (nrow(policies_df) > 0) {
      for (i in seq_len(nrow(policies_df))) {
        p <- policies_df[i, ]
        cli::cli_alert_info("{.strong {p$policyname}} [{p$cmd}] -> {p$roles}")
      }
    } else {
      cli::cli_alert_warning("No policies found!")
    }

    # Check distribution of created_by
    creators <- DBI::dbGetQuery(con, "
      SELECT created_by, COUNT(*) as n_plots
      FROM data_liste_plots
      GROUP BY created_by
      ORDER BY n_plots DESC
      LIMIT 10
    ")

    cli::cli_h3("Plot ownership distribution (top 10):")
    for (i in seq_len(nrow(creators))) {
      c <- creators[i, ]
      cli::cli_alert_info("{.field {c$created_by}}: {c$n_plots} plots")
    }
  }

  # Summary
  cli::cli_h2("Migration Complete")

  if (dry_run) {
    cli::cli_alert_warning("DRY RUN - No changes were made")
    cli::cli_alert_info("Run with dry_run = FALSE to apply changes")
  } else {
    cli::cli_alert_success("Migration completed successfully!")
    cli::cli_alert_info("Users can now automatically access plots they create")
    cli::cli_alert_info("Use define_user_policy() to grant access to other users' plots")
  }

  invisible(TRUE)
}


#' Check created_by Migration Status
#'
#' Verifies whether the created_by migration has been applied.
#'
#' @param con Database connection
#' @return List with migration status details
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' check_created_by_migration(con)
#' }
#'
#' @export
check_created_by_migration <- function(con) {

 cli::cli_h2("Checking created_by migration status")

  result <- list(
    column_exists = FALSE,
    policies_exist = FALSE,
    rls_enabled = FALSE
  )

  # Check column
  columns <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT column_name
      FROM information_schema.columns
      WHERE table_name = 'data_liste_plots' AND column_name = 'created_by'
    ")
  }, error = function(e) data.frame())

  result$column_exists <- nrow(columns) > 0

  if (result$column_exists) {
    cli::cli_alert_success("Column 'created_by' exists")
  } else {
    cli::cli_alert_danger("Column 'created_by' does NOT exist")
  }

  # Check policies
  policies <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT policyname
      FROM pg_policies
      WHERE tablename = 'data_liste_plots'
        AND policyname LIKE 'creator_access%'
    ")
  }, error = function(e) data.frame())

  result$policies_exist <- nrow(policies) >= 3

  if (result$policies_exist) {
    cli::cli_alert_success("Creator access policies exist ({nrow(policies)} found)")
  } else {
    cli::cli_alert_danger("Creator access policies NOT found")
  }

  # Check RLS enabled
  rls_status <- tryCatch({
    DBI::dbGetQuery(con, "
      SELECT relrowsecurity
      FROM pg_class
      WHERE relname = 'data_liste_plots'
    ")
  }, error = function(e) data.frame(relrowsecurity = FALSE))

  result$rls_enabled <- isTRUE(rls_status$relrowsecurity[1])

  if (result$rls_enabled) {
    cli::cli_alert_success("Row Level Security is enabled")
  } else {
    cli::cli_alert_warning("Row Level Security is NOT enabled")
  }

  # Summary
  result$migration_complete <- all(unlist(result))

  if (result$migration_complete) {
    cli::cli_alert_success("Migration is complete and active")
  } else {
    cli::cli_alert_warning("Migration is incomplete - run migrate_add_created_by()")
  }

  invisible(result)
}
