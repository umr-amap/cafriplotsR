#' Grant permissions for adding people to table_colnam
#'
#' @description
#' Creates a secure PostgreSQL function that allows users to add new people
#' to table_colnam without granting direct INSERT permissions on the table.
#' This uses SECURITY DEFINER to execute with elevated privileges.
#'
#' **For database administrators only.**
#'
#' @param con Database connection (must have superuser or table owner privileges)
#'
#' @details
#' This function creates:
#' 1. A PostgreSQL function `add_person()` that validates and inserts new people
#' 2. Grants EXECUTE permission on this function to PUBLIC (all users)
#' 3. The function runs with SECURITY DEFINER (owner's privileges)
#'
#' Users can then add people by calling:
#' ```sql
#' SELECT add_person('FirstName', 'LastName', 'Nationality', 'Institute', 'Contact');
#' ```
#'
#' The function validates that:
#' - First name and last name are provided and non-empty
#' - The person doesn't already exist (based on full name)
#'
#' @return TRUE if successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' # As database administrator:
#' con <- call.mydb()
#' setup_add_person_function(con)
#'
#' # Users can then add people:
#' DBI::dbGetQuery(con, "
#'   SELECT add_person('John', 'Doe', 'USA', 'University', 'john.doe@example.com')
#' ")
#' }
#'
#' @export
setup_add_person_function <- function(con) {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  cli::cli_alert_info("Creating secure add_person() function...")

  tryCatch({
    # Create the function with SECURITY DEFINER
    DBI::dbExecute(con, "
      CREATE OR REPLACE FUNCTION add_person(
        p_first_name TEXT,
        p_last_name TEXT,
        p_nationality TEXT DEFAULT NULL,
        p_institute TEXT DEFAULT NULL,
        p_contact TEXT DEFAULT NULL
      )
      RETURNS INTEGER
      SECURITY DEFINER
      LANGUAGE plpgsql
      AS $$
      DECLARE
        v_full_name TEXT;
        v_new_id INTEGER;
        v_existing_id INTEGER;
      BEGIN
        -- Validate inputs
        IF p_first_name IS NULL OR TRIM(p_first_name) = '' THEN
          RAISE EXCEPTION 'First name is required';
        END IF;

        IF p_last_name IS NULL OR TRIM(p_last_name) = '' THEN
          RAISE EXCEPTION 'Last name is required';
        END IF;

        -- Build full name
        v_full_name := TRIM(p_first_name) || ' ' || TRIM(p_last_name);

        -- Check if person already exists
        SELECT id_table_colnam INTO v_existing_id
        FROM table_colnam
        WHERE colnam = v_full_name
        LIMIT 1;

        IF v_existing_id IS NOT NULL THEN
          RAISE NOTICE 'Person already exists with ID: %', v_existing_id;
          RETURN v_existing_id;
        END IF;

        -- Insert new person
        INSERT INTO table_colnam (surname, family_name, colnam, nationality, institute, contact)
        VALUES (
          TRIM(p_first_name),
          TRIM(p_last_name),
          v_full_name,
          NULLIF(TRIM(p_nationality), ''),
          NULLIF(TRIM(p_institute), ''),
          NULLIF(TRIM(p_contact), '')
        )
        RETURNING id_table_colnam INTO v_new_id;

        RAISE NOTICE 'Successfully added person: % (ID: %)', v_full_name, v_new_id;
        RETURN v_new_id;
      END;
      $$;
    ")

    cli::cli_alert_success("Created add_person() function")

    # Grant execute permission to all users
    DBI::dbExecute(con, "
      GRANT EXECUTE ON FUNCTION add_person(TEXT, TEXT, TEXT, TEXT, TEXT) TO PUBLIC;
    ")

    cli::cli_alert_success("Granted EXECUTE permission to all users")

    cli::cli_alert_success(paste0(
      "Setup complete! Users can now add people using:\n",
      "  add_person_to_db(con, 'FirstName', 'LastName', ...)"
    ))

    return(invisible(TRUE))

  }, error = function(e) {
    cli::cli_alert_danger("Failed to setup add_person function: {e$message}")

    if (grepl("permission denied", e$message, ignore.case = TRUE)) {
      cli::cli_alert_info("You need superuser or table owner privileges to create SECURITY DEFINER functions")
    }

    return(invisible(FALSE))
  })
}


#' Add a person to table_colnam using secure function
#'
#' @description
#' Adds a new person to table_colnam using the secure add_person() PostgreSQL function.
#' This works even if the user doesn't have direct INSERT permission on table_colnam.
#'
#' **Requires**: Database administrator must first run `setup_add_person_function()`
#'
#' @param con Database connection
#' @param first_name Character. Person's first name (required)
#' @param last_name Character. Person's last name (required)
#' @param nationality Character. Person's nationality (optional)
#' @param institute Character. Person's institute/organization (optional)
#' @param contact Character. Contact email or phone (optional)
#'
#' @return Integer. The ID of the newly created (or existing) person
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Add a new person
#' id <- add_person_to_db(con, "John", "Doe",
#'                        nationality = "USA",
#'                        institute = "University XYZ")
#'
#' # Person already exists - returns existing ID
#' id2 <- add_person_to_db(con, "John", "Doe")  # Returns same ID
#' }
#'
#' @export
add_person_to_db <- function(con, first_name, last_name,
                              nationality = NULL,
                              institute = NULL,
                              contact = NULL) {

  # Validate inputs
  if (is.null(first_name) || nchar(trimws(first_name)) == 0) {
    stop("First name is required", call. = FALSE)
  }

  if (is.null(last_name) || nchar(trimws(last_name)) == 0) {
    stop("Last name is required", call. = FALSE)
  }

  # Build query
  query <- sprintf("
    SELECT add_person(%s, %s, %s, %s, %s) as id
  ",
    DBI::dbQuoteLiteral(con, trimws(first_name)),
    DBI::dbQuoteLiteral(con, trimws(last_name)),
    if (!is.null(nationality)) DBI::dbQuoteLiteral(con, trimws(nationality)) else "NULL",
    if (!is.null(institute)) DBI::dbQuoteLiteral(con, trimws(institute)) else "NULL",
    if (!is.null(contact)) DBI::dbQuoteLiteral(con, trimws(contact)) else "NULL"
  )

  tryCatch({
    result <- DBI::dbGetQuery(con, query)
    new_id <- result$id

    cli::cli_alert_success("Person added/found: {first_name} {last_name} (ID: {new_id})")

    return(new_id)

  }, error = function(e) {
    if (grepl("function add_person.*does not exist", e$message, ignore.case = TRUE)) {
      stop(
        "The add_person() function doesn't exist in the database.\n",
        "Database administrator must run: setup_add_person_function(con)",
        call. = FALSE
      )
    } else {
      stop(e$message, call. = FALSE)
    }
  })
}


#' Check if secure add_person function exists
#'
#' @description
#' Checks whether the add_person() PostgreSQL function has been created
#' by a database administrator.
#'
#' @param con Database connection
#'
#' @return Logical. TRUE if function exists, FALSE otherwise
#'
#' @keywords internal
check_add_person_function_exists <- function(con) {
  tryCatch({
    result <- DBI::dbGetQuery(con, "
      SELECT EXISTS (
        SELECT 1
        FROM pg_proc p
        JOIN pg_namespace n ON p.pronamespace = n.oid
        WHERE n.nspname = 'public'
        AND p.proname = 'add_person'
      ) as exists
    ")
    return(result$exists)
  }, error = function(e) {
    return(FALSE)
  })
}


#' Grant table-level permissions for lookup tables
#'
#' @description
#' Grants INSERT, UPDATE, SELECT permissions on common lookup tables.
#' Alternative to SECURITY DEFINER functions - gives direct table access.
#'
#' **For database administrators only.**
#'
#' @param con Database connection (must have GRANT privilege)
#' @param user Character. Username to grant permissions to
#' @param tables Character vector. Tables to grant permissions on.
#'   Default: c("table_colnam", "table_countries", "methodslist")
#' @param operations Character vector. Operations to grant.
#'   Default: c("SELECT", "INSERT", "UPDATE")
#'
#' @return TRUE if successful, FALSE otherwise
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Grant permissions to a specific user
#' grant_lookup_table_permissions(con, "john.doe")
#'
#' # Grant only to specific table
#' grant_lookup_table_permissions(con, "john.doe", tables = "table_colnam")
#'
#' # Grant full access including DELETE
#' grant_lookup_table_permissions(con, "admin_user",
#'                                operations = c("SELECT", "INSERT", "UPDATE", "DELETE"))
#' }
#'
#' @export
grant_lookup_table_permissions <- function(con, user,
                                           tables = c("table_colnam", "table_countries",
                                                      "methodslist", "table_citations"),
                                           operations = c("SELECT", "INSERT", "UPDATE")) {

  if (!test_connection(con)) {
    cli::cli_alert_danger("Invalid database connection")
    return(invisible(FALSE))
  }

  if (is.null(user) || nchar(user) == 0) {
    stop("User must be specified", call. = FALSE)
  }

  success <- TRUE

  for (table in tables) {
    tryCatch({
      ops <- paste(operations, collapse = ", ")
      sql <- glue::glue("
        GRANT {ops} ON {DBI::dbQuoteIdentifier(con, table)}
        TO {DBI::dbQuoteIdentifier(con, user)};
      ")

      DBI::dbExecute(con, sql)
      cli::cli_alert_success("Granted {ops} on '{table}' to '{user}'")

    }, error = function(e) {
      cli::cli_alert_danger("Failed to grant on '{table}': {e$message}")
      success <<- FALSE
    })
  }

  return(invisible(success))
}
