#' Setup permissions for users to insert plots
#'
#' @description
#' Grants necessary table-level permissions for users to insert plots into
#' data_liste_plots. Row-level security policies control which plots users
#' can see/modify after insertion.
#'
#' **For database administrators only.**
#'
#' There are two approaches:
#' 1. **Grant to specific users** - Use `grant_plot_insert_permissions()`
#' 2. **Grant to a role** - Grant to a role and assign users to that role
#'
#' @param con Database connection (must have GRANT privilege)
#' @param user Character. Username or role name to grant permissions to
#' @param grant_to_public Logical. If TRUE, grants to PUBLIC (all users).
#'   Default FALSE for security. Use with caution.
#'
#' @details
#' This function grants table-level privileges:
#' - **SELECT**: Read plots (RLS controls which rows)
#' - **INSERT**: Create new plots (RLS auto-sets created_by)
#' - **UPDATE**: Modify plots (RLS controls which rows)
#' - **DELETE**: Delete plots (RLS controls which rows)
#'
#' After granting table privileges, Row-Level Security (RLS) policies control:
#' - Users can always see/modify plots they created (via created_by column)
#' - Admins can grant access to other users' plots via `define_user_policy()`
#'
#' @return TRUE if successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Grant to specific user
#' grant_plot_insert_permissions(con, "john.doe")
#'
#' # Grant to a role (then assign users to role)
#' grant_plot_insert_permissions(con, "data_contributors")
#' # Then in PostgreSQL: GRANT data_contributors TO john_doe;
#'
#' # Grant to all users (use with caution!)
#' grant_plot_insert_permissions(con, grant_to_public = TRUE)
#' }
#'
#' @seealso
#' - `define_user_policy()` - Grant access to specific plots
#' - `diagnose_plot_permissions()` - Diagnose permission issues
#'
#' @export
grant_plot_insert_permissions <- function(con, user = NULL, grant_to_public = FALSE) {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  if (is.null(user) && !grant_to_public) {
    stop("Must specify either 'user' or set 'grant_to_public = TRUE'", call. = FALSE)
  }

  if (grant_to_public) {
    cli::cli_alert_warning("Granting to PUBLIC - all users will be able to insert plots")
    target <- "PUBLIC"
  } else {
    target <- user
  }

  cli::cli_h2("Granting Plot Insert Permissions")

  # Tables related to plots that users need access to
  all_tables <- c(
    "data_liste_plots",
    "data_liste_sub_plots",
    "data_individuals",
    "data_ind_measures",
    "data_ind_measures_feat",
    "data_traits_measures",
    "data_trait_measures"
  )

  # Check which tables actually exist
  existing_tables <- c()
  for (table in all_tables) {
    exists <- tryCatch({
      DBI::dbExistsTable(con, table)
    }, error = function(e) FALSE)

    if (exists) {
      existing_tables <- c(existing_tables, table)
    } else {
      cli::cli_alert_info("Table '{table}' not found - skipping")
    }
  }

  if (length(existing_tables) == 0) {
    cli::cli_alert_danger("No target tables found in database!")
    return(invisible(FALSE))
  }

  success <- TRUE

  for (table in existing_tables) {
    tryCatch({
      # Grant basic CRUD operations
      sql <- glue::glue("
        GRANT SELECT, INSERT, UPDATE, DELETE
        ON {DBI::dbQuoteIdentifier(con, table)}
        TO {if (grant_to_public) target else DBI::dbQuoteIdentifier(con, target)};
      ")

      DBI::dbExecute(con, sql)
      cli::cli_alert_success("Granted permissions on '{table}'")

    }, error = function(e) {
      cli::cli_alert_danger("Failed on '{table}': {e$message}")
      success <<- FALSE
    })
  }

  # Discover and grant on sequences automatically
  cli::cli_alert_info("Discovering sequences for granted tables...")

  sequences <- tryCatch({
    # Find all sequences owned by the tables we just granted on
    seq_query <- sprintf("
      SELECT
        s.relname as sequence_name
      FROM pg_class s
      JOIN pg_depend d ON d.objid = s.oid
      JOIN pg_class t ON d.refobjid = t.oid
      WHERE s.relkind = 'S'
        AND t.relname IN (%s)
    ", paste0("'", existing_tables, "'", collapse = ", "))

    result <- DBI::dbGetQuery(con, seq_query)
    result$sequence_name
  }, error = function(e) {
    cli::cli_alert_warning("Could not discover sequences: {e$message}")
    character(0)
  })

  if (length(sequences) > 0) {
    cli::cli_alert_info("Found {length(sequences)} sequence(s) to grant")

    for (seq in sequences) {
      tryCatch({
        sql <- glue::glue("
          GRANT USAGE, SELECT ON SEQUENCE {DBI::dbQuoteIdentifier(con, seq)}
          TO {if (grant_to_public) target else DBI::dbQuoteIdentifier(con, target)};
        ")

        DBI::dbExecute(con, sql)
        cli::cli_alert_success("Granted sequence access on '{seq}'")

      }, error = function(e) {
        cli::cli_alert_warning("Failed to grant on sequence '{seq}': {e$message}")
        success <<- FALSE
      })
    }
  } else {
    cli::cli_alert_info("No sequences found or none needed")
  }

  if (success) {
    cli::cli_alert_success("✅ Permissions granted successfully!")
    cli::cli_alert_info("Users can now insert plots. RLS policies control which plots they can see/modify.")
  } else {
    cli::cli_alert_danger("❌ Some permissions failed to grant")
  }

  invisible(success)
}


#' Diagnose plot insertion permissions
#'
#' @description
#' Diagnostic tool to check if a user has the necessary permissions to
#' insert plots into data_liste_plots and related tables.
#'
#' @param con Database connection
#' @param verbose Logical. Print detailed information? Default TRUE.
#'
#' @return List with diagnostic information
#'
#' @export
diagnose_plot_permissions <- function(con, verbose = TRUE) {

  results <- list(
    current_user = NULL,
    table_permissions = list(),
    rls_enabled = list(),
    can_insert_plots = FALSE
  )

  if (verbose) cli::cli_h2("Diagnosing Plot Insert Permissions")

  # Get current user
  tryCatch({
    results$current_user <- DBI::dbGetQuery(con, "SELECT current_user")$current_user
    if (verbose) cli::cli_alert_info("Current user: {results$current_user}")
  }, error = function(e) {
    if (verbose) cli::cli_alert_danger("Cannot get current user: {e$message}")
  })

  # Check table-level permissions
  tables <- c("data_liste_plots", "data_liste_sub_plots", "data_individuals")

  if (verbose) cli::cli_h3("Table-Level Permissions")

  for (table in tables) {
    tryCatch({
      perms <- DBI::dbGetQuery(con, sprintf("
        SELECT
          has_table_privilege(current_user, '%s', 'SELECT') as can_select,
          has_table_privilege(current_user, '%s', 'INSERT') as can_insert,
          has_table_privilege(current_user, '%s', 'UPDATE') as can_update,
          has_table_privilege(current_user, '%s', 'DELETE') as can_delete
      ", table, table, table, table))

      results$table_permissions[[table]] <- perms

      if (verbose) {
        cli::cli_alert_info("Table: {table}")
        status <- c()
        if (perms$can_select) status <- c(status, "SELECT")
        if (perms$can_insert) status <- c(status, "INSERT")
        if (perms$can_update) status <- c(status, "UPDATE")
        if (perms$can_delete) status <- c(status, "DELETE")

        if (length(status) > 0) {
          cli::cli_alert_success("  Permissions: {paste(status, collapse = ', ')}")
        } else {
          cli::cli_alert_danger("  No permissions!")
        }

        if (!perms$can_insert) {
          cli::cli_alert_warning("  ⚠️  Cannot INSERT - admin must run: grant_plot_insert_permissions(con, '{results$current_user}')")
        }
      }

    }, error = function(e) {
      if (verbose) cli::cli_alert_danger("Error checking '{table}': {e$message}")
    })
  }

  # Check RLS policies
  if (verbose) cli::cli_h3("Row-Level Security (RLS)")

  for (table in tables) {
    tryCatch({
      rls_info <- DBI::dbGetQuery(con, sprintf("
        SELECT
          relrowsecurity as rls_enabled,
          relforcerowsecurity as rls_forced
        FROM pg_class
        WHERE relname = '%s'
      ", table))

      results$rls_enabled[[table]] <- rls_info

      if (verbose && nrow(rls_info) > 0) {
        if (rls_info$rls_enabled) {
          cli::cli_alert_info("  {table}: RLS enabled ✓")
        } else {
          cli::cli_alert_warning("  {table}: RLS not enabled")
        }
      }

    }, error = function(e) {
      if (verbose) cli::cli_alert_info("  Cannot check RLS for '{table}'")
    })
  }

  # Overall assessment
  if (verbose) {
    cli::cli_h3("Summary")

    can_insert <- results$table_permissions$data_liste_plots$can_insert
    results$can_insert_plots <- can_insert

    if (can_insert) {
      cli::cli_alert_success("✅ User CAN insert plots")
      cli::cli_alert_info("RLS policies will control which plots are visible after insertion")
    } else {
      cli::cli_alert_danger("❌ User CANNOT insert plots")
      cli::cli_alert_info("Admin must run: grant_plot_insert_permissions(con, '{results$current_user}')")
    }
  }

  invisible(results)
}


#' Grant permissions on ALL tables and sequences
#'
#' @description
#' Grants SELECT, INSERT, UPDATE, DELETE on ALL existing tables and
#' USAGE, SELECT on ALL existing sequences in the public schema.
#'
#' **For database administrators only.**
#'
#' This is more permissive than `grant_plot_insert_permissions()` but
#' avoids missing table errors. Note: Does NOT affect future tables
#' (need ALTER DEFAULT PRIVILEGES for that).
#'
#' @param con Database connection (must have GRANT privilege)
#' @param user Character. Username or role to grant permissions to.
#'   If NULL, must set grant_to_public = TRUE.
#' @param grant_to_public Logical. If TRUE, grants to PUBLIC (all users).
#'   Default FALSE for security.
#' @param schema Character. Schema name. Default "public".
#'
#' @return TRUE if successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Grant to all users
#' grant_all_table_permissions(con, grant_to_public = TRUE)
#'
#' # Grant to specific user
#' grant_all_table_permissions(con, "john.doe")
#' }
#'
#' @export
grant_all_table_permissions <- function(con, user = NULL, grant_to_public = FALSE,
                                        schema = "public") {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  if (is.null(user) && !grant_to_public) {
    stop("Must specify either 'user' or set 'grant_to_public = TRUE'", call. = FALSE)
  }

  if (grant_to_public) {
    cli::cli_alert_warning("Granting to PUBLIC - all users will have access to ALL tables")
    target <- "PUBLIC"
  } else {
    target <- user
  }

  cli::cli_h2("Granting ALL Table Permissions")

  success <- TRUE

  # Grant on all tables
  tryCatch({
    sql_tables <- glue::glue("
      GRANT SELECT, INSERT, UPDATE, DELETE
      ON ALL TABLES IN SCHEMA {DBI::dbQuoteIdentifier(con, schema)}
      TO {if (grant_to_public) target else DBI::dbQuoteIdentifier(con, target)};
    ")

    DBI::dbExecute(con, sql_tables)
    cli::cli_alert_success("Granted SELECT, INSERT, UPDATE, DELETE on ALL tables in '{schema}'")

  }, error = function(e) {
    cli::cli_alert_danger("Failed to grant on tables: {e$message}")
    success <- FALSE
  })

  # Grant on all sequences
  tryCatch({
    sql_sequences <- glue::glue("
      GRANT USAGE, SELECT
      ON ALL SEQUENCES IN SCHEMA {DBI::dbQuoteIdentifier(con, schema)}
      TO {if (grant_to_public) target else DBI::dbQuoteIdentifier(con, target)};
    ")

    DBI::dbExecute(con, sql_sequences)
    cli::cli_alert_success("Granted USAGE, SELECT on ALL sequences in '{schema}'")

  }, error = function(e) {
    cli::cli_alert_danger("Failed to grant on sequences: {e$message}")
    success <- FALSE
  })

  if (success) {
    cli::cli_alert_success("✅ Successfully granted permissions on all existing tables and sequences")
    cli::cli_alert_info("Note: This does NOT affect future tables. Use ALTER DEFAULT PRIVILEGES for that.")
  }

  invisible(success)
}


#' Setup complete import wizard permissions
#'
#' @description
#' One-command setup to grant all necessary permissions for users to use
#' the import wizard. This includes:
#' - Secure function for adding people (table_colnam)
#' - Table permissions for inserting plots
#' - Table permissions for lookup tables
#'
#' **For database administrators only.**
#'
#' @param con Database connection (must have superuser or GRANT privileges)
#' @param user Character. Username or role to grant permissions to.
#'   If NULL and grant_to_public = FALSE, only sets up secure functions.
#' @param grant_to_public Logical. If TRUE, grants to all users (PUBLIC).
#'   Default FALSE.
#' @param grant_all_tables Logical. If TRUE, uses `grant_all_table_permissions()`
#'   instead of specific tables. Default TRUE to avoid missing table errors.
#'
#' @return TRUE if all setups successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Setup for specific user (grants on all tables)
#' setup_import_wizard_permissions(con, "john.doe")
#'
#' # Setup for all users (grants on all tables)
#' setup_import_wizard_permissions(con, grant_to_public = TRUE)
#'
#' # Setup with specific tables only
#' setup_import_wizard_permissions(con, grant_to_public = TRUE, grant_all_tables = FALSE)
#' }
#'
#' @export
setup_import_wizard_permissions <- function(con, user = NULL, grant_to_public = FALSE,
                                            grant_all_tables = TRUE) {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  cli::cli_h1("Setting Up Import Wizard Permissions")

  results <- list(
    add_person_function = FALSE,
    plot_permissions = FALSE,
    lookup_permissions = FALSE
  )

  # 1. Setup secure add_person function
  cli::cli_h2("Step 1: Secure Function for Adding People")
  results$add_person_function <- setup_add_person_function(con)
  if (!results$add_person_function) {
    cli::cli_alert_warning("Failed to setup add_person function")
  }

  # 2. Grant plot insert permissions
  if (!is.null(user) || grant_to_public) {
    cli::cli_h2("Step 2: Table Permissions")

    if (grant_all_tables) {
      cli::cli_alert_info("Using grant on ALL tables (recommended)")
      results$plot_permissions <- grant_all_table_permissions(con, user, grant_to_public)
    } else {
      cli::cli_alert_info("Using grant on specific tables only")
      results$plot_permissions <- grant_plot_insert_permissions(con, user, grant_to_public)
    }

    if (!results$plot_permissions) {
      cli::cli_alert_warning("Some table permissions failed")
    }

    # 3. Grant lookup table permissions
    cli::cli_h2("Step 3: Lookup Table Permissions")
    if (!is.null(user) && !grant_to_public) {
      results$lookup_permissions <- grant_lookup_table_permissions(con, user)
      if (!results$lookup_permissions) {
        cli::cli_alert_warning("Some lookup table permissions failed")
      }
    } else {
      results$lookup_permissions <- TRUE  # Not needed for PUBLIC
    }
  } else {
    results$plot_permissions <- TRUE  # Not requested
    results$lookup_permissions <- TRUE  # Not requested
  }

  # Check if critical permissions were granted
  critical_success <- results$add_person_function && results$plot_permissions

  cli::cli_h2("Summary")

  if (critical_success) {
    cli::cli_alert_success("✅ Import wizard permissions setup complete!")

    if (!is.null(user)) {
      cli::cli_alert_info("User '{user}' can now:")
    } else if (grant_to_public) {
      cli::cli_alert_info("All users can now:")
    } else {
      cli::cli_alert_info("All users can now:")
    }

    cli::cli_ul(c(
      "Add new people to the database",
      "Insert new plots and related data",
      if (results$lookup_permissions) "Update lookup tables" else NULL
    ))

    if (!results$lookup_permissions) {
      cli::cli_alert_info("Note: Lookup table permissions had issues but import wizard should work")
    }
  } else {
    cli::cli_alert_danger("❌ Critical permission setups failed")

    if (!results$add_person_function) {
      cli::cli_alert_warning("Cannot add people - secure function not created")
    }
    if (!results$plot_permissions) {
      cli::cli_alert_warning("Cannot insert plots - permissions not granted")
    }
  }

  invisible(critical_success)
}
