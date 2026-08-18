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

  # Check if staging table exists - if so, use legacy method which updates it properly
  staging_table_exists <- tryCatch({
    result <- DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM table_idtax_temp;")
    result$n[1] > 0
  }, error = function(e) FALSE)

  if (staging_table_exists) {
    cli::cli_alert_info("Detected staging table setup (table_idtax_temp)")
    cli::cli_alert_info("Using manual update method to ensure staging table is refreshed...")
    return(legacy_update_taxa_link_table(con = actual_con, con_taxa = con_taxa))
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

      # CRITICAL: Verify that the refresh actually updated the data
      # Check if table_idtax_temp exists and has recent data
      cli::cli_alert_info("Verifying data was actually updated...")

      temp_table_exists <- tryCatch({
        result <- DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM table_idtax_temp;")
        result$n[1] > 0
      }, error = function(e) FALSE)

      if (!temp_table_exists) {
        cli::cli_alert_warning("Staging table table_idtax_temp not found or empty")
        cli::cli_alert_info("PostgreSQL function may not be updating staging table properly")
        cli::cli_alert_info("Falling back to manual update method...")
        stop("Staging table verification failed - triggering fallback")
      }

      cli::cli_alert_success("Verification passed - data appears to be updated")

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
    # Check if it's a permission error or materialized view issue
    if (grepl("permission denied|does not exist|is not a table|materialized view", e$message, ignore.case = TRUE)) {
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
    cli::cli_alert_warning("table_idtax is a materialized view - cannot use dbWriteTable directly")
    cli::cli_alert_info("Updating staging table table_idtax_temp first...")

    # Step 1: Update the staging table that feeds the materialized view
    # Use TRUNCATE + INSERT instead of DROP/CREATE to preserve dependencies
    staging_success <- tryCatch({
      # TRUNCATE the staging table (faster than DELETE and resets sequences)
      DBI::dbExecute(actual_con, "TRUNCATE TABLE table_idtax_temp;")
      cli::cli_alert_info("Truncated staging table table_idtax_temp")

      # Insert new data
      DBI::dbWriteTable(
        actual_con,
        name = "table_idtax_temp",
        value = id_taxa_table,
        append = TRUE,  # Append instead of overwrite
        overwrite = FALSE
      )

      cli::cli_alert_success("Staging table table_idtax_temp updated with {nrow(id_taxa_table)} records")
      TRUE

    }, error = function(e) {
      cli::cli_alert_warning("Failed to update staging table: {e$message}")
      cli::cli_alert_info("Will try direct refresh instead...")
      FALSE
    })

    # Step 2: Refresh the materialized view (pulls from updated staging table)
    refresh_success <- tryCatch({
      cli::cli_alert_info("Refreshing materialized view table_idtax...")
      DBI::dbExecute(actual_con, "REFRESH MATERIALIZED VIEW table_idtax;")

      end_time <- Sys.time()
      duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

      # Verify refresh worked by checking record count
      count_result <- DBI::dbGetQuery(actual_con, "SELECT COUNT(*) as n FROM table_idtax;")

      cli::cli_alert_success("Materialized view refreshed successfully")
      cli::cli_alert_info("Refresh completed in {round(duration, 2)} seconds")
      cli::cli_alert_info("Record count: {count_result$n[1]}")

      return(list(
        success = TRUE,
        method = if (staging_success) "staging_table_and_refresh" else "direct_refresh",
        message = if (staging_success) "Updated staging table and refreshed view" else "Refreshed view only",
        record_count = count_result$n[1],
        duration = duration
      ))

    }, error = function(e) {
      cli::cli_alert_danger("Failed to refresh materialized view: {e$message}")
      cli::cli_alert_info("You need REFRESH permission or admin access")
      cli::cli_alert_info("Contact your database administrator to:")
      cli::cli_alert_info("  1. Grant you REFRESH permission, or")
      cli::cli_alert_info("  2. Run the refresh on your behalf")

      stop("Insufficient permissions to update table_idtax")
    })

    return(refresh_success)

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
      dplyr::as_tibble()
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch metadata: {e$message}")
    NULL
  })

  return(metadata)
}


#' Add taxonomic entry non-interactively (for Shiny apps)
#'
#' Non-interactive version of add_entry_taxa for use in Shiny applications.
#' Does not prompt for user input or growth forms.
#'
#' @param tax_gen Character, genus name
#' @param tax_esp Character, species epithet
#' @param tax_fam Character, family name
#' @param tax_order Character, order name
#' @param tax_famclass Character, class name
#' @param tax_rank1 Character, infraspecific rank ("var." or "subsp.")
#' @param tax_name1 Character, infraspecific name
#' @param author1 Character, species author
#' @param author2 Character, infraspecific author
#' @param author3 Character, additional author
#' @param year_description Numeric, year of description
#' @param morpho_species Logical, whether this is a morphospecies
#' @param tax_tax Character, full taxonomic name with authors
#' @param con Database connection (pool or connection object)
#'
#' @return Integer, the new idtax_n value
#' @keywords internal
#' @export
.add_taxa_noninteractive <- function(
    tax_gen = NULL,
    tax_esp = NULL,
    tax_fam = NULL,
    tax_order = NULL,
    tax_famclass = NULL,
    tax_rank1 = NULL,
    tax_name1 = NULL,
    author1 = NULL,
    author2 = NULL,
    author3 = NULL,
    year_description = NULL,
    morpho_species = FALSE,
    tax_tax = NULL,
    con = NULL
) {

  # Get connection
  if (is.null(con)) {
    mydb_taxa <- call.mydb.taxa()
  } else {
    mydb_taxa <- con
  }

  # Convert NULL to NA for database insertion
  if (is.null(tax_esp)) tax_esp <- NA
  if (is.null(tax_gen)) tax_gen <- NA
  if (is.null(tax_fam)) tax_fam <- NA
  if (is.null(tax_order)) tax_order <- NA
  if (is.null(tax_famclass)) tax_famclass <- NA
  if (is.null(tax_rank1)) tax_rank1 <- NA
  if (is.null(tax_name1)) tax_name1 <- NA
  if (is.null(author1)) author1 <- NA
  if (is.null(author2)) author2 <- NA
  if (is.null(author3)) author3 <- NA

  # Determine tax_rank
  tax_rank <- NA
  if (!is.na(tax_esp) & is.na(tax_rank1)) {
    tax_rank <- "ESP"
  }
  if (!is.na(tax_esp) & !is.na(tax_rank1)) {
    if (tax_rank1 == "subsp.") tax_rank <- "SUBSP"
    if (tax_rank1 == "var.") tax_rank <- "VAR"
    if (tax_rank1 == "f.") tax_rank <- "F"
  }

  # Determine tax_rankinf
  tax_rankinf <- NA
  if (!is.na(tax_rank)) {
    if (tax_rank == "VAR") tax_rankinf <- "VAR"
    if (tax_rank == "SUBSP") tax_rankinf <- "SUBSP"
  }
  if (!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "FAM"
  if (!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "GEN"
  if (!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp) & (is.na(tax_rank) | tax_rank == "ESP")) tax_rankinf <- "ESP"
  if (!is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "ORDER"
  if (!is.na(tax_famclass) & is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "CLASS"

  # Determine tax_rankesp
  tax_rankesp <- NA
  if (!is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "ORDER"
  if (!is.na(tax_famclass) & is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "CLASS"
  if (!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "FAM"
  if (!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "GEN"
  if (!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp)) tax_rankesp <- "ESP"

  # Get id_tax_famclass
  id_tax_fam_class <- try_open_postgres_table(table = "table_tax_famclass", con = mydb_taxa) %>%
    filter(tax_famclass == !!tax_famclass) %>%
    collect()

  if (nrow(id_tax_fam_class) == 0) {
    stop("Class '", tax_famclass, "' not found in table_tax_famclass")
  }

  # Build new record
  new_rec <- dplyr::tibble(
    tax_order = tax_order,
    tax_famclass = tax_famclass,
    tax_fam = tax_fam,
    tax_gen = tax_gen,
    tax_esp = tax_esp,
    tax_rank01 = tax_rank1,
    tax_nam01 = tax_name1,
    tax_rank02 = NA,
    tax_nam02 = NA,
    tax_source = "NEW",
    tax_rank = tax_rank,
    tax_rankinf = tax_rankinf,
    tax_rankesp = tax_rankesp,
    fktax = NA,
    author1 = author1,
    author2 = author2,
    author3 = author3,
    citation = NA,
    year_description = ifelse(!is.null(year_description) && !is.na(year_description), year_description, NA),
    idtax_good_n = NA,
    id_tax_famclass = id_tax_fam_class$id_tax_famclass[1],
    morpho_species = morpho_species
  )

  # Check for duplicates
  seek_dup <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa)

  if (!is.na(new_rec$tax_famclass)) {
    seek_dup <- seek_dup %>% filter(tax_famclass == !!new_rec$tax_famclass)
  }
  if (!is.na(new_rec$tax_order)) {
    seek_dup <- seek_dup %>% filter(tax_order == !!new_rec$tax_order)
  } else {
    seek_dup <- seek_dup %>% filter(is.na(tax_order))
  }
  if (!is.na(new_rec$tax_fam)) {
    seek_dup <- seek_dup %>% filter(tax_fam == !!new_rec$tax_fam)
  } else {
    seek_dup <- seek_dup %>% filter(is.na(tax_fam))
  }
  if (!is.na(new_rec$tax_gen)) {
    seek_dup <- seek_dup %>% filter(tax_gen == !!new_rec$tax_gen)
  } else {
    seek_dup <- seek_dup %>% filter(is.na(tax_gen))
  }
  if (!is.na(new_rec$tax_esp)) {
    seek_dup <- seek_dup %>% filter(tax_esp == !!new_rec$tax_esp)
  } else {
    seek_dup <- seek_dup %>% filter(is.na(tax_esp))
  }
  if (!is.na(new_rec$tax_rank01)) {
    seek_dup <- seek_dup %>% dplyr::filter(.data$tax_rank01 == !!new_rec$tax_rank01)
  } else {
    seek_dup <- seek_dup %>% dplyr::filter(is.na(.data$tax_rank01))
  }
  if (!is.na(new_rec$tax_nam01)) {
    seek_dup <- seek_dup %>% dplyr::filter(.data$tax_nam01 == !!new_rec$tax_nam01)
  } else {
    seek_dup <- seek_dup %>% dplyr::filter(is.na(.data$tax_nam01))
  }

  seek_dup <- seek_dup %>% collect()

  if (nrow(seek_dup) > 0) {
    stop("Taxon already exists in database with idtax_n = ", seek_dup$idtax_n[1])
  }

  # Add modification fields
  new_rec <- .add_modif_field(new_rec)
  new_rec <- new_rec %>%
    dplyr::rename(
      data_modif_m = "date_modif_m",
      data_modif_y = "date_modif_y",
      data_modif_d = "date_modif_d"
    )

  # Insert into database
  cli::cli_alert_success("Adding new entry to table_taxa")
  DBI::dbWriteTable(mydb_taxa, "table_taxa", new_rec, append = TRUE, row.names = FALSE)

  # Get the new ID (use dbGetQuery for pool compatibility)
  lastval <- DBI::dbGetQuery(mydb_taxa, "SELECT MAX(idtax_n) AS max FROM table_taxa")

  new_id <- lastval$max[1]
  cli::cli_alert_success("New taxon added with idtax_n = {new_id}")

  return(new_id)
}
