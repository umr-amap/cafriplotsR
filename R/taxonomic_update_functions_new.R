# Updated Taxonomic Functions for Materialized View Approach
#
# These functions replace/augment existing functions in taxonomic_update_functions.R
# to work with table_idtax as a PostgreSQL materialized view.
#
# Migration: After running the SQL migration script, replace the old functions
# with these new versions.


#' Check table_idtax Staleness
#'
#' Checks how old the table_idtax materialized view is and whether it needs
#' refreshing based on a configurable threshold.
#'
#' @param con Database connection. If NULL, calls call.mydb()
#' @param warn_days Integer, number of days after which to warn (default 90)
#' @param silent Logical, if TRUE suppresses messages (default FALSE)
#'
#' @return List with elements:
#'   - is_stale: Logical, TRUE if older than warn_days
#'   - days_old: Numeric, age in days
#'   - last_updated: POSIXct, timestamp of last update
#'   - message: Character, status message
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' status <- check_table_idtax_staleness(con)
#' if (status$is_stale) {
#'   update_taxa_link_table(con)
#' }
#' }
#'
#' @export
check_table_idtax_staleness <- function(con = NULL, warn_days = 90, silent = FALSE) {

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

  result <- tryCatch({
    # Call PostgreSQL function
    status <- DBI::dbGetQuery(
      actual_con,
      sprintf("SELECT * FROM check_table_idtax_staleness(%d);", warn_days)
    )

    if (nrow(status) == 0) {
      return(list(
        is_stale = TRUE,
        days_old = NA,
        last_updated = NA,
        message = "Could not check staleness (no metadata found)"
      ))
    }

    # Convert to list
    result <- list(
      is_stale = status$is_stale[1],
      days_old = status$days_old[1],
      last_updated = status$last_updated[1],
      message = status$message[1]
    )

    # Display message if not silent
    if (!silent) {
      if (result$is_stale) {
        cli::cli_alert_warning(result$message)
        cli::cli_alert_info("Run update_taxa_link_table() to refresh.")
      } else {
        cli::cli_alert_success(result$message)
      }
    }

    result

  }, error = function(e) {
    # Fallback if PostgreSQL function doesn't exist (pre-migration)
    metadata <- tryCatch({
      DBI::dbGetQuery(
        actual_con,
        "SELECT * FROM table_idtax_metadata WHERE table_name = 'table_idtax';"
      )
    }, error = function(e2) NULL)

    if (is.null(metadata) || nrow(metadata) == 0) {
      if (!silent) {
        cli::cli_alert_warning("Cannot check table_idtax age (metadata table not found)")
        cli::cli_alert_info("Consider running the materialized view migration")
      }
      return(list(
        is_stale = TRUE,
        days_old = NA,
        last_updated = NA,
        message = "No metadata available"
      ))
    }

    # Calculate age manually
    days_old <- as.numeric(difftime(Sys.time(), metadata$last_updated[1], units = "days"))
    is_stale <- days_old > warn_days

    msg <- if (is_stale) {
      sprintf("WARNING: table_idtax is %.1f days old (threshold: %d days)", days_old, warn_days)
    } else {
      sprintf("table_idtax is up to date (%.1f days old)", days_old)
    }

    if (!silent) {
      if (is_stale) {
        cli::cli_alert_warning(msg)
      } else {
        cli::cli_alert_success(msg)
      }
    }

    list(
      is_stale = is_stale,
      days_old = round(days_old, 1),
      last_updated = metadata$last_updated[1],
      message = msg
    )
  })

  return(result)
}


#' Update table_idtax (Materialized View Version)
#'
#' Refreshes the table_idtax materialized view with latest synonym information
#' from the taxa database. This version works with the materialized view approach
#' and can be run by non-admin users with appropriate permissions.
#'
#' The function first tries to use the PostgreSQL refresh_table_idtax() function
#' (materialized view approach). If that fails, it falls back to the legacy
#' method of updating via dbWriteTable (requires admin permissions).
#'
#' @param con Database connection to main DB. If NULL, calls call.mydb()
#' @param con_taxa Database connection to taxa DB. If NULL, calls call.mydb.taxa()
#' @param force Logical, if TRUE forces refresh even if recently updated (default FALSE)
#' @param warn_days Integer, age threshold in days (default 90)
#'
#' @return List with elements:
#'   - success: Logical, TRUE if refresh succeeded
#'   - method: Character, "materialized_view" or "legacy"
#'   - message: Character, status message
#'   - record_count: Integer, number of records after refresh
#'   - duration: Numeric, refresh duration in seconds (if available)
#'
#' @examples
#' \dontrun{
#' # Simple refresh
#' update_taxa_link_table()
#'
#' # Force refresh even if recently updated
#' update_taxa_link_table(force = TRUE)
#'
#' # With explicit connections
#' con <- call.mydb()
#' result <- update_taxa_link_table(con = con)
#' }
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
update_taxa_link_table <- function(con = NULL, con_taxa = NULL, force = FALSE, warn_days = 90) {

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

  # Check if refresh is needed (unless forced)
  if (!force) {
    staleness <- check_table_idtax_staleness(con = actual_con, warn_days = warn_days, silent = TRUE)

    if (!is.na(staleness$is_stale) && !staleness$is_stale) {
      cli::cli_alert_info("table_idtax was updated {staleness$days_old} days ago (within threshold)")
      cli::cli_alert_info("Use force = TRUE to refresh anyway")

      return(list(
        success = TRUE,
        method = "skipped",
        message = "Refresh not needed",
        record_count = NA,
        duration = 0
      ))
    }
  }

  # Try materialized view approach first
  cli::cli_alert_info("Attempting to refresh table_idtax materialized view...")

  result <- tryCatch({
    # Call PostgreSQL refresh function
    refresh_result <- DBI::dbGetQuery(actual_con, "SELECT * FROM refresh_table_idtax();")

    if (nrow(refresh_result) > 0 && refresh_result$success[1]) {
      cli::cli_alert_success(refresh_result$message[1])

      if (!is.na(refresh_result$refresh_duration[1])) {
        duration_sec <- as.numeric(refresh_result$refresh_duration[1], units = "secs")
        cli::cli_alert_info("Refresh completed in {round(duration_sec, 2)} seconds")
      }

      return(list(
        success = TRUE,
        method = "materialized_view",
        message = refresh_result$message[1],
        record_count = refresh_result$record_count[1],
        duration = as.numeric(refresh_result$refresh_duration[1], units = "secs")
      ))
    } else {
      cli::cli_alert_danger(refresh_result$message[1])
      stop("Materialized view refresh failed")
    }

  }, error = function(e) {
    # Check if it's a permission error
    if (grepl("permission denied|does not exist", e$message, ignore.case = TRUE)) {
      cli::cli_alert_warning("Materialized view refresh not available: {e$message}")
      cli::cli_alert_info("Falling back to legacy update method...")

      # Fall back to legacy method
      legacy_update_taxa_link_table(con = actual_con, con_taxa = con_taxa)

    } else {
      stop(e)
    }
  })

  return(result)
}


#' Legacy table_idtax Update Method
#'
#' Original method for updating table_idtax using dbWriteTable.
#' Requires admin/write permissions. Used as fallback when materialized
#' view approach is not available.
#'
#' @param con Main database connection
#' @param con_taxa Taxa database connection
#'
#' @return List with success status
#' @keywords internal
legacy_update_taxa_link_table <- function(con = NULL, con_taxa = NULL) {

  if (is.null(con_taxa)) {
    con_taxa <- call.mydb.taxa()
  }

  if (is.null(con)) {
    con <- call.mydb()
  }

  # Handle pool connections for taxa
  actual_con_taxa <- if (inherits(con_taxa, "Pool")) {
    pool::poolCheckout(con_taxa)
  } else {
    con_taxa
  }

  # Handle pool connections for main
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con_taxa, "Pool") && !is.null(actual_con_taxa)) {
      pool::poolReturn(actual_con_taxa)
    }
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  start_time <- Sys.time()

  cli::cli_alert_info("Fetching taxonomy data from taxa database...")

  id_taxa_table <- try_open_postgres_table(table = "table_taxa", con = actual_con_taxa) %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::collect()

  cli::cli_alert_info("Writing {nrow(id_taxa_table)} records to table_idtax...")

  # For materialized view, we can't use dbWriteTable
  # Need to use REFRESH or recreate the view
  view_exists <- tryCatch({
    result <- DBI::dbGetQuery(
      actual_con,
      "SELECT COUNT(*) as n FROM pg_matviews WHERE matviewname = 'table_idtax';"
    )
    result$n[1] > 0
  }, error = function(e) FALSE)

  if (view_exists) {
    cli::cli_alert_danger("table_idtax is a materialized view - cannot update with dbWriteTable")
    cli::cli_alert_info("You need REFRESH permission or admin access")
    cli::cli_alert_info("Contact your database administrator to:")
    cli::cli_alert_info("  1. Grant you REFRESH permission, or")
    cli::cli_alert_info("  2. Run the refresh on your behalf")

    stop("Insufficient permissions to update table_idtax")

  } else {
    # Regular table - use dbWriteTable
    DBI::dbWriteTable(
      actual_con,
      name = "table_idtax",
      value = id_taxa_table,
      append = FALSE,
      overwrite = TRUE
    )

    # Update metadata if table exists
    tryCatch({
      metadata <- data.frame(
        table_name = "table_idtax",
        last_updated = Sys.time(),
        updated_by = Sys.info()["user"],
        record_count = nrow(id_taxa_table),
        source_info = "Updated via legacy dbWriteTable method",
        notes = NA_character_,
        stringsAsFactors = FALSE
      )

      DBI::dbWriteTable(
        actual_con,
        name = "table_idtax_metadata",
        value = metadata,
        append = FALSE,
        overwrite = TRUE
      )
    }, error = function(e) {
      # Metadata table might not exist - ignore
    })

    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

    cli::cli_alert_success("table_idtax updated ({nrow(id_taxa_table)} records)")
    cli::cli_alert_info("Update completed in {round(duration, 2)} seconds")

    return(list(
      success = TRUE,
      method = "legacy",
      message = "Updated via dbWriteTable",
      record_count = nrow(id_taxa_table),
      duration = duration
    ))
  }
}


#' Get table_idtax Metadata
#'
#' Returns metadata about the table_idtax materialized view including
#' last update time, record count, and who updated it.
#'
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return Tibble with metadata, or NULL if not available
#'
#' @examples
#' \dontrun{
#' get_table_idtax_metadata()
#' }
#'
#' @export
get_table_idtax_metadata <- function(con = NULL) {

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

  metadata <- tryCatch({
    DBI::dbGetQuery(
      actual_con,
      "SELECT * FROM table_idtax_metadata WHERE table_name = 'table_idtax';"
    ) %>%
      tibble::as_tibble()
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch metadata: {e$message}")
    NULL
  })

  return(metadata)
}
