# User Management Functions -----------------------------------------------

#' Create user registry table
#'
#' @description
#' Creates the `user_registry` table in the main database to store user
#' metadata (email, institution, etc.). This table is used for user tracking
#' and communication, not for authentication.
#'
#' **For database administrators only.**
#'
#' @param con Connection to main database.
#'
#' @return TRUE if successful, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' create_user_registry(con)
#' }
#'
#' @export
create_user_registry <- function(con) {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  # Check if table already exists
  if (DBI::dbExistsTable(con, "user_registry")) {
    cli::cli_alert_info("Table 'user_registry' already exists")
    return(invisible(TRUE))
  }

  sql_create <- "
    CREATE TABLE user_registry (
      username TEXT PRIMARY KEY,
      email TEXT,
      first_name TEXT,
      last_name TEXT,
      institution TEXT,
      created_at TIMESTAMP DEFAULT NOW(),
      is_active BOOLEAN DEFAULT TRUE,
      notes TEXT
    )
  "

  sql_comment <- "COMMENT ON TABLE user_registry IS 'User metadata registry for tracking and communication'"

  tryCatch({
    DBI::dbExecute(con, sql_create)
    DBI::dbExecute(con, sql_comment)
    cli::cli_alert_success("Created 'user_registry' table")

    # Grant full access to the table creator, SELECT to all users
    DBI::dbExecute(con, "GRANT ALL ON user_registry TO CURRENT_USER")
    DBI::dbExecute(con, "GRANT SELECT ON user_registry TO PUBLIC")
    cli::cli_alert_info("Granted ALL on user_registry to current user, SELECT to all users")

    invisible(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create user_registry: {e$message}")
    invisible(FALSE)
  })
}


#' Register a user in the registry
#'
#' @description
#' Adds or updates a user's metadata in the `user_registry` table.
#' The user must already exist as a PostgreSQL role (created via OVH portal).
#'
#' @param con Connection to main database.
#' @param username Character. PostgreSQL username (must match the role name).
#' @param email Character. User's email address.
#' @param first_name Character. User's first name (optional).
#' @param last_name Character. User's last name (optional).
#' @param institution Character. User's institution (optional).
#' @param notes Character. Additional notes (optional).
#'
#' @return TRUE if successful, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' register_user(con,
#'   username = "jdupont",
#'   email = "j.dupont@institution.org",
#'   first_name = "Jean",
#'   last_name = "Dupont",
#'   institution = "IRD"
#' )
#' }
#'
#' @export
register_user <- function(con, username, email = NULL,
                          first_name = NULL, last_name = NULL,
                          institution = NULL, notes = NULL) {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  # Convert NULL to NA_character_ for SQL substitution (glue_sql requires length-1 values)
  email       <- if (is.null(email))       NA_character_ else email
  first_name  <- if (is.null(first_name))  NA_character_ else first_name
  last_name   <- if (is.null(last_name))   NA_character_ else last_name
  institution <- if (is.null(institution)) NA_character_ else institution
  notes       <- if (is.null(notes))       NA_character_ else notes

  tryCatch({
    # Verify the user exists as a PostgreSQL role
    role_exists <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT 1 FROM pg_roles WHERE rolname = {username} AND rolcanlogin = TRUE",
      .con = con
    ))

    if (nrow(role_exists) == 0) {
      cli::cli_alert_danger("User '{username}' does not exist as a PostgreSQL role. Create it on OVH first.")
      return(invisible(FALSE))
    }

    # Ensure registry table exists
    if (!DBI::dbExistsTable(con, "user_registry")) {
      cli::cli_alert_info("Creating user_registry table...")
      create_user_registry(con)
    }

    # Check if user already registered
    existing <- DBI::dbGetQuery(con, glue::glue_sql(
      "SELECT username FROM user_registry WHERE username = {username}",
      .con = con
    ))
    if (nrow(existing) > 0) {
      # Update existing entry
      updates <- c()
      params <- list()

      if (!is.na(email)) updates <- c(updates, "email = {email}")
      if (!is.na(first_name)) updates <- c(updates, "first_name = {first_name}")
      if (!is.na(last_name)) updates <- c(updates, "last_name = {last_name}")
      if (!is.na(institution)) updates <- c(updates, "institution = {institution}")
      if (!is.na(notes)) updates <- c(updates, "notes = {notes}")

      if (length(updates) > 0) {
        sql <- glue::glue_sql(
          "UPDATE user_registry SET ",
          paste(updates, collapse = ", "),
          " WHERE username = {username}",
          .con = con
        )
        DBI::dbExecute(con, sql)
        cli::cli_alert_success("Updated registry for user '{username}'")
      } else {
        cli::cli_alert_info("User '{username}' already registered, no updates provided")
      }
    } else {
      # Insert new entry
      sql <- glue::glue_sql(
        "INSERT INTO user_registry (username, email, first_name, last_name, institution, notes)
         VALUES ({username}, {email}, {first_name}, {last_name}, {institution}, {notes})",
        .con = con
      )
      DBI::dbExecute(con, sql)
      cli::cli_alert_success("Registered user '{username}' in registry")
    }

    invisible(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to register user: {e$message}")
    invisible(FALSE)
  })
}


#' Setup user permissions on both databases
#'
#' @description
#' One-command setup to grant a user appropriate permissions on the main database
#' (`plots_transects`) and/or the taxa database (`rainbio`). Also registers the
#' user in the `user_registry` table.
#'
#' The user must already exist as a PostgreSQL role (created via OVH portal).
#'
#' **For database administrators only.**
#'
#' @param con_main Connection to main database.
#' @param con_taxa Connection to taxa database (optional).
#' @param username Character. PostgreSQL username.
#' @param email Character. User's email address (optional but recommended).
#' @param first_name Character. User's first name (optional).
#' @param last_name Character. User's last name (optional).
#' @param institution Character. User's institution (optional).
#' @param main_db_access Character. Access level for plots_transects:
#'   `"read_only"`, `"read_write"`, or `"none"`. Default `"read_write"`.
#' @param taxa_db_access Character. Access level for rainbio:
#'   `"read_only"`, `"read_write"`, or `"none"`. Default `"read_only"`.
#' @param plot_ids Integer vector. Plot IDs to grant access to (optional).
#'   If provided, RLS policies will be set for these plots.
#' @param notes Character. Additional notes about the user (optional).
#'
#' @return A list with setup results.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' con_taxa <- call.mydb.taxa()
#'
#' # Full setup for a new user
#' setup_user_permissions(
#'   con_main = con,
#'   con_taxa = con_taxa,
#'   username = "jdupont",
#'   email = "j.dupont@institution.org",
#'   first_name = "Jean",
#'   last_name = "Dupont",
#'   institution = "IRD",
#'   main_db_access = "read_write",
#'   taxa_db_access = "read_only",
#'   plot_ids = c(10, 15, 22)
#' )
#' }
#'
#' @export
setup_user_permissions <- function(con_main, con_taxa = NULL,
                                   username,
                                   email = NULL,
                                   first_name = NULL,
                                   last_name = NULL,
                                   institution = NULL,
                                   main_db_access = c("read_write", "read_only", "none"),
                                   taxa_db_access = c("read_only", "read_write", "none"),
                                   plot_ids = NULL,
                                   notes = NULL) {

  main_db_access <- match.arg(main_db_access)
  taxa_db_access <- match.arg(taxa_db_access)

  if (!test_connection(con_main)) {
    stop("Invalid main database connection", call. = FALSE)
  }

  # Verify user exists as PostgreSQL role
  role_exists <- DBI::dbGetQuery(con_main, glue::glue_sql(
    "SELECT 1 FROM pg_roles WHERE rolname = {username} AND rolcanlogin = TRUE",
    .con = con_main
  ))

  if (nrow(role_exists) == 0) {
    stop(glue::glue("User '{username}' does not exist as a PostgreSQL role. Create it on OVH first."),
         call. = FALSE)
  }

  results <- list(
    registry = FALSE,
    main_db = FALSE,
    taxa_db = FALSE,
    rls_policies = FALSE
  )

  cli::cli_h1("Setting Up User: {username}")

  # Step 1: Register in user registry
  cli::cli_h2("Step 1: User Registry")
  results$registry <- register_user(con_main, username, email,
                                    first_name, last_name,
                                    institution, notes)

  # Step 2: Main database permissions
  cli::cli_h2("Step 2: Main Database Permissions ({main_db_access})")

  if (main_db_access != "none") {
    results$main_db <- tryCatch({
      if (main_db_access == "read_only") {
        # Grant SELECT on all tables
        sql <- glue::glue_sql(
          "GRANT SELECT ON ALL TABLES IN SCHEMA public TO {`username`}",
          .con = con_main
        )
        DBI::dbExecute(con_main, sql)

        sql_seq <- glue::glue_sql(
          "GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO {`username`}",
          .con = con_main
        )
        DBI::dbExecute(con_main, sql_seq)

        cli::cli_alert_success("Granted read-only access on main database")
      } else {
        # Use existing function for read_write
        grant_all_table_permissions(con_main, username)
      }
      TRUE
    }, error = function(e) {
      cli::cli_alert_danger("Failed to grant main DB permissions: {e$message}")
      FALSE
    })
  } else {
    cli::cli_alert_info("Skipping main database (access = 'none')")
    results$main_db <- TRUE
  }

  # Step 3: Taxa database permissions
  cli::cli_h2("Step 3: Taxa Database Permissions ({taxa_db_access})")

  if (taxa_db_access != "none" && !is.null(con_taxa)) {
    if (!test_connection(con_taxa)) {
      cli::cli_alert_warning("Invalid taxa database connection - skipping")
    } else {
      results$taxa_db <- tryCatch({
        if (taxa_db_access == "read_only") {
          sql <- glue::glue_sql(
            "GRANT SELECT ON ALL TABLES IN SCHEMA public TO {`username`}",
            .con = con_taxa
          )
          DBI::dbExecute(con_taxa, sql)

          sql_seq <- glue::glue_sql(
            "GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO {`username`}",
            .con = con_taxa
          )
          DBI::dbExecute(con_taxa, sql_seq)

          cli::cli_alert_success("Granted read-only access on taxa database")
        } else {
          grant_all_table_permissions(con_taxa, username)
        }
        TRUE
      }, error = function(e) {
        cli::cli_alert_danger("Failed to grant taxa DB permissions: {e$message}")
        FALSE
      })
    }
  } else if (is.null(con_taxa) && taxa_db_access != "none") {
    cli::cli_alert_warning("No taxa database connection provided - skipping")
  } else {
    cli::cli_alert_info("Skipping taxa database (access = 'none')")
    results$taxa_db <- TRUE
  }

  # Step 4: RLS policies for specific plots
  cli::cli_h2("Step 4: Row-Level Security Policies")

  if (!is.null(plot_ids) && length(plot_ids) > 0) {
    results$rls_policies <- tryCatch({
      ops <- if (main_db_access == "read_only") "SELECT" else c("SELECT", "INSERT", "UPDATE")
      define_user_policy(con_main, username, plot_ids, operations = ops)
      cli::cli_alert_success("Set RLS policies for {length(plot_ids)} plots")
      TRUE
    }, error = function(e) {
      cli::cli_alert_danger("Failed to set RLS policies: {e$message}")
      FALSE
    })
  } else {
    cli::cli_alert_info("No plot IDs specified - skipping RLS policies")
    cli::cli_alert_info("Use define_user_policy() later to grant access to specific plots")
    results$rls_policies <- TRUE
  }

  # Summary
  cli::cli_h2("Summary")
  all_ok <- all(unlist(results))

  if (all_ok) {
    cli::cli_alert_success("User '{username}' fully set up!")
  } else {
    failed <- names(results)[!unlist(results)]
    cli::cli_alert_warning("Setup completed with issues in: {paste(failed, collapse = ', ')}")
  }

  invisible(results)
}


#' Get all registered users
#'
#' @description
#' Retrieves all users from the `user_registry` table, optionally
#' filtered by active status.
#'
#' @param con Connection to main database.
#' @param active_only Logical. If TRUE (default), only return active users.
#'
#' @return A data.frame with user information.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' get_registered_users(con)
#' get_registered_users(con, active_only = FALSE)
#' }
#'
#' @export
get_registered_users <- function(con, active_only = TRUE) {

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  if (!DBI::dbExistsTable(con, "user_registry")) {
    cli::cli_alert_warning("user_registry table does not exist. Run create_user_registry() first.")
    return(data.frame())
  }

  if (active_only) {
    sql <- "SELECT * FROM user_registry WHERE is_active = TRUE ORDER BY username"
  } else {
    sql <- "SELECT * FROM user_registry ORDER BY username"
  }

  DBI::dbGetQuery(con, sql)
}


#' Get email addresses of all registered users
#'
#' @description
#' Retrieves email addresses for all active registered users.
#' Useful for sending bulk communications.
#'
#' @param con Connection to main database.
#' @param as_string Logical. If TRUE, returns emails as a semicolon-separated
#'   string (for pasting into email clients). If FALSE (default), returns a
#'   character vector.
#'
#' @return Character vector of emails, or a single string if `as_string = TRUE`.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Get as vector
#' emails <- get_user_emails(con)
#'
#' # Get as string for email client
#' get_user_emails(con, as_string = TRUE)
#' }
#'
#' @export
get_user_emails <- function(con, as_string = FALSE) {

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  if (!DBI::dbExistsTable(con, "user_registry")) {
    cli::cli_alert_warning("user_registry table does not exist. Run create_user_registry() first.")
    return(character(0))
  }

  result <- DBI::dbGetQuery(con,
    "SELECT username, email FROM user_registry WHERE is_active = TRUE AND email IS NOT NULL ORDER BY username"
  )

  if (nrow(result) == 0) {
    cli::cli_alert_info("No users with email addresses found")
    return(character(0))
  }

  cli::cli_alert_info("Found {nrow(result)} users with email addresses")

  emails <- result$email

  if (as_string) {
    email_str <- paste(emails, collapse = "; ")
    cli::cli_text(email_str)
    return(invisible(email_str))
  }

  emails
}


#' Deactivate a user
#'
#' @description
#' Marks a user as inactive in the registry and revokes their database
#' permissions. Does NOT drop the PostgreSQL role (that must be done on OVH).
#'
#' @param con_main Connection to main database.
#' @param con_taxa Connection to taxa database (optional).
#' @param username Character. Username to deactivate.
#' @param revoke_permissions Logical. If TRUE (default), revokes all table
#'   permissions and drops RLS policies.
#'
#' @return TRUE if successful, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' con_taxa <- call.mydb.taxa()
#' deactivate_user(con, con_taxa, "jdupont")
#' }
#'
#' @export
deactivate_user <- function(con_main, con_taxa = NULL, username,
                            revoke_permissions = TRUE) {

  if (!test_connection(con_main)) {
    stop("Invalid database connection", call. = FALSE)
  }

  cli::cli_h2("Deactivating user: {username}")

  success <- TRUE

  # Step 1: Mark as inactive in registry
  if (DBI::dbExistsTable(con_main, "user_registry")) {
    tryCatch({
      DBI::dbExecute(con_main, glue::glue_sql(
        "UPDATE user_registry SET is_active = FALSE WHERE username = {username}",
        .con = con_main
      ))
      cli::cli_alert_success("Marked '{username}' as inactive in registry")
    }, error = function(e) {
      cli::cli_alert_warning("Could not update registry: {e$message}")
    })
  }

  if (!revoke_permissions) {
    return(invisible(success))
  }

  # Step 2: Drop RLS policies on main database
  tryCatch({
    policies <- list_user_policies(con_main, user = username)
    if (nrow(policies) > 0) {
      for (i in seq_len(nrow(policies))) {
        drop_sql <- glue::glue_sql(
          "DROP POLICY IF EXISTS {`policies$policyname[i]`} ON {`policies$tablename[i]`}",
          .con = con_main
        )
        DBI::dbExecute(con_main, drop_sql)
      }
      cli::cli_alert_success("Dropped {nrow(policies)} RLS policies")
    } else {
      cli::cli_alert_info("No RLS policies found for '{username}'")
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not drop RLS policies: {e$message}")
    success <<- FALSE
  })

  # Step 3: Revoke permissions on main database
  tryCatch({
    DBI::dbExecute(con_main, glue::glue_sql(
      "REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM {`username`}",
      .con = con_main
    ))
    DBI::dbExecute(con_main, glue::glue_sql(
      "REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public FROM {`username`}",
      .con = con_main
    ))
    cli::cli_alert_success("Revoked all permissions on main database")
  }, error = function(e) {
    cli::cli_alert_warning("Could not revoke main DB permissions: {e$message}")
    success <<- FALSE
  })

  # Step 4: Revoke permissions on taxa database
  if (!is.null(con_taxa) && test_connection(con_taxa)) {
    tryCatch({
      DBI::dbExecute(con_taxa, glue::glue_sql(
        "REVOKE ALL PRIVILEGES ON ALL TABLES IN SCHEMA public FROM {`username`}",
        .con = con_taxa
      ))
      DBI::dbExecute(con_taxa, glue::glue_sql(
        "REVOKE ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public FROM {`username`}",
        .con = con_taxa
      ))
      cli::cli_alert_success("Revoked all permissions on taxa database")
    }, error = function(e) {
      cli::cli_alert_warning("Could not revoke taxa DB permissions: {e$message}")
      success <<- FALSE
    })
  }

  if (success) {
    cli::cli_alert_success("User '{username}' deactivated successfully")
    cli::cli_alert_info("Note: The PostgreSQL role still exists. Remove it from OVH portal if needed.")
  } else {
    cli::cli_alert_warning("User '{username}' deactivated with some issues")
  }

  invisible(success)
}


#' Reactivate a user
#'
#' @description
#' Marks a previously deactivated user as active in the registry.
#' Does NOT restore permissions — use `setup_user_permissions()` to re-grant access.
#'
#' @param con Connection to main database.
#' @param username Character. Username to reactivate.
#'
#' @return TRUE if successful, FALSE otherwise.
#'
#' @export
reactivate_user <- function(con, username) {

  if (!test_connection(con)) {
    stop("Invalid database connection", call. = FALSE)
  }

  if (!DBI::dbExistsTable(con, "user_registry")) {
    cli::cli_alert_warning("user_registry table does not exist")
    return(invisible(FALSE))
  }

  tryCatch({
    result <- DBI::dbExecute(con, glue::glue_sql(
      "UPDATE user_registry SET is_active = TRUE WHERE username = {username}",
      .con = con
    ))

    if (result == 0) {
      cli::cli_alert_warning("User '{username}' not found in registry")
      return(invisible(FALSE))
    }

    cli::cli_alert_success("Reactivated user '{username}' in registry")
    cli::cli_alert_info("Use setup_user_permissions() to re-grant database access")
    invisible(TRUE)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to reactivate user: {e$message}")
    invisible(FALSE)
  })
}
