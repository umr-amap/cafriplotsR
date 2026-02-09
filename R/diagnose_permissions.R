#' Diagnose add_person function and permissions
#'
#' @description
#' Diagnostic tool to check if the secure add_person() function is properly
#' set up and accessible by the current user.
#'
#' @param con Database connection
#' @param verbose Logical. Print detailed information? Default TRUE.
#'
#' @return List with diagnostic information
#'
#' @export
diagnose_add_person_setup <- function(con, verbose = TRUE) {

  results <- list(
    function_exists = FALSE,
    can_execute = FALSE,
    can_direct_insert = FALSE,
    function_owner = NULL,
    current_user = NULL,
    error = NULL
  )

  if (verbose) cli::cli_h2("Diagnosing add_person() Setup")

  # Get current user
  tryCatch({
    results$current_user <- DBI::dbGetQuery(con, "SELECT current_user")$current_user
    if (verbose) cli::cli_alert_info("Current user: {results$current_user}")
  }, error = function(e) {
    results$error <- paste("Cannot get current user:", e$message)
  })

  # Check if function exists
  tryCatch({
    func_info <- DBI::dbGetQuery(con, "
      SELECT
        p.proname,
        pg_catalog.pg_get_userbyid(p.proowner) as owner,
        p.prosecdef as is_security_definer,
        has_function_privilege(current_user, p.oid, 'EXECUTE') as can_execute
      FROM pg_proc p
      JOIN pg_namespace n ON p.pronamespace = n.oid
      WHERE n.nspname = 'public'
      AND p.proname = 'add_person'
    ")

    if (nrow(func_info) > 0) {
      results$function_exists <- TRUE
      results$function_owner <- func_info$owner
      results$can_execute <- func_info$can_execute

      if (verbose) {
        cli::cli_alert_success("Function exists!")
        cli::cli_alert_info("Function owner: {func_info$owner}")
        cli::cli_alert_info("Security definer: {func_info$is_security_definer}")
        cli::cli_alert_info("Can execute: {func_info$can_execute}")
      }
    } else {
      results$function_exists <- FALSE
      if (verbose) {
        cli::cli_alert_danger("Function does NOT exist")
        cli::cli_alert_info("Admin must run: setup_add_person_function(con)")
      }
    }
  }, error = function(e) {
    results$error <- paste("Error checking function:", e$message)
    if (verbose) cli::cli_alert_danger("Error checking function: {e$message}")
  })

  # Check direct INSERT permission
  tryCatch({
    has_insert <- DBI::dbGetQuery(con, "
      SELECT has_table_privilege(current_user, 'table_colnam', 'INSERT') as has_insert
    ")$has_insert

    results$can_direct_insert <- has_insert

    if (verbose) {
      if (has_insert) {
        cli::cli_alert_success("User has direct INSERT permission on table_colnam")
      } else {
        cli::cli_alert_warning("User does NOT have direct INSERT permission on table_colnam")
      }
    }
  }, error = function(e) {
    if (verbose) cli::cli_alert_danger("Error checking INSERT permission: {e$message}")
  })

  # Test the function
  if (results$function_exists && results$can_execute) {
    if (verbose) cli::cli_h3("Testing function execution")

    tryCatch({
      test_result <- DBI::dbGetQuery(con, "
        SELECT add_person('TEST', 'USER_DELETE_ME', NULL, NULL, NULL) as id
      ")

      if (verbose) {
        cli::cli_alert_success("Function executes successfully!")
        cli::cli_alert_info("Test person ID: {test_result$id}")
        cli::cli_alert_warning("Remember to delete test person: DELETE FROM table_colnam WHERE id_table_colnam = {test_result$id}")
      }

      results$test_success <- TRUE
      results$test_id <- test_result$id

    }, error = function(e) {
      results$test_success <- FALSE
      results$test_error <- e$message
      if (verbose) cli::cli_alert_danger("Function execution failed: {e$message}")
    })
  }

  # Summary
  if (verbose) {
    cli::cli_h3("Summary")

    if (results$function_exists && results$can_execute) {
      cli::cli_alert_success("✅ Setup is correct - user can add people via secure function")
    } else if (results$can_direct_insert) {
      cli::cli_alert_warning("⚠️  User has direct INSERT but secure function not set up")
    } else {
      cli::cli_alert_danger("❌ User cannot add people - needs admin to run setup_add_person_function()")
    }
  }

  invisible(results)
}
