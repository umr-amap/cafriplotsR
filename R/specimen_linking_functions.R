# Specimen Linking Functions
#
# This file contains functions for linking individual trees to herbarium specimens.
# These functions manage the relationships between field observations (individuals)
# and herbarium collections (specimens) in the database.
#
# Main functions:
# - .add_link_specimens(): Add links between individuals and herbarium specimens
# - query_all_specimen_links(): Query links with optional specimen and linktype info
# - get_linktypes(): Get available link types from lookup table
# - get_ref_specimen_ind(): Find reference specimen for individuals by collector
#
# Link Types:
# - type_individual: Specimen collected from this specific individual (priority 100)
# - referenced_individual: Specimen represents same species but from different individual (priority 50)
#
# Dependencies: DBI, dplyr, cli, tibble


#' Get Link Types from Lookup Table
#'
#' Returns the available link types from the linktypelist lookup table.
#'
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return Tibble with id_linktype, linktype, description, priority columns
#' @export
get_linktypes <- function(con = NULL) {
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

  tryCatch({
    result <- dplyr::tbl(actual_con, "linktypelist") %>%
      dplyr::collect() %>%
      dplyr::arrange(dplyr::desc(priority))

    return(result)
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch link types: {e$message}")
    # Return default values if table doesn't exist yet
    return(tibble::tibble(
      id_linktype = c(1L, 2L),
      linktype = c("type_individual", "referenced_individual"),
      description = c("Specimen collected from this specific individual",
                      "Specimen represents same species but from different individual"),
      priority = c(100L, 50L)
    ))
  })
}


#' Validate Specimen Link Before Adding
#'
#' Internal function to validate that all foreign key references exist.
#'
#' @param id_specimen Integer specimen ID
#' @param id_n Integer individual ID
#' @param id_linktype Integer link type ID
#' @param con Database connection
#'
#' @return List with valid (logical) and errors (character vector)
#' @keywords internal
.validate_specimen_link <- function(id_specimen, id_n, id_linktype, con) {
  errors <- character()

  # Validate id_specimen exists in specimens table
  specimen_exists <- dplyr::tbl(con, "specimens") %>%
    dplyr::filter(id_specimen == !!id_specimen) %>%
    dplyr::count() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  if (specimen_exists == 0) {
    errors <- c(errors, paste0("Specimen ID ", id_specimen, " not found in specimens table"))
  }

  # Validate id_n exists in data_individuals table
  individual_exists <- dplyr::tbl(con, "data_individuals") %>%
    dplyr::filter(id_n == !!id_n) %>%
    dplyr::count() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  if (individual_exists == 0) {
    errors <- c(errors, paste0("Individual ID ", id_n, " not found in data_individuals table"))
  }

  # Validate id_linktype exists in linktypelist (if not NULL)
  if (!is.null(id_linktype) && !is.na(id_linktype)) {
    linktype_exists <- tryCatch({
      dplyr::tbl(con, "linktypelist") %>%
        dplyr::filter(id_linktype == !!id_linktype) %>%
        dplyr::count() %>%
        dplyr::collect() %>%
        dplyr::pull(n)
    }, error = function(e) 1)  # Skip check if table doesn't exist

    if (linktype_exists == 0) {
      errors <- c(errors, paste0("Link type ID ", id_linktype, " not found in linktypelist table"))
    }
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}


#' Add Link Between Specimen and Individual
#'
#' Creates links between herbarium specimens and individual trees in the database.
#' Includes validation to ensure foreign key references exist.
#'
#' @param new_data Tibble with columns: id_specimen, id_n, and either id_linktype or type
#' @param col_names_select Character vector of column names in new_data to use.
#'   If NULL, uses all columns of new_data.
#' @param col_names_corresp Character vector of target column names.
#'   Default: c("id_specimen", "id_n", "id_linktype")
#' @param launch_adding_data Logical. If TRUE, links are actually added to database.
#'   Default FALSE for safety.
#' @param validate Logical. If TRUE, validates FK references before adding.
#'   Default TRUE.
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return Invisibly returns the data that was (or would be) added
#'
#' @details
#' Link types:
#' - type_individual (id_linktype=1): Specimen collected from this specific individual
#' - referenced_individual (id_linktype=2): Specimen represents same species but from different individual
#'
#' The function:
#' 1. Renames columns to standard names
#' 2. Checks for duplicate links (same id_specimen + id_n already in database)
#' 3. Validates FK references if validate=TRUE
#' 4. Sets audit columns (created_by, created_at)
#' 5. Adds links if launch_adding_data=TRUE
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
.add_link_specimens <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = c("id_specimen", "id_n", "id_linktype"),
                                launch_adding_data = FALSE,
                                validate = TRUE,
                                con = NULL) {

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

  if (is.null(col_names_select)) {
    col_names_select <- names(new_data)
  }

  # Rename columns to standard names
  new_data_renamed <- .rename_data(
    dataset = new_data,
    col_old = col_names_select,
    col_new = col_names_corresp
  )

  # Handle backward compatibility: convert 'type' string to 'id_linktype'
  if ("type" %in% names(new_data_renamed) && !"id_linktype" %in% names(new_data_renamed)) {
    cli::cli_alert_info("Converting 'type' column to 'id_linktype' using linktypelist lookup")
    linktypes <- get_linktypes(actual_con)

    new_data_renamed <- new_data_renamed %>%
      dplyr::left_join(
        linktypes %>% dplyr::select(dplyr::all_of(c("linktype", "id_linktype"))),
        by = c("type" = "linktype")
      )

    # Check for unmapped types
    unmapped <- new_data_renamed %>%
      dplyr::filter(is.na(.data$id_linktype) & !is.na(.data$type)) %>%
      dplyr::distinct(.data$type) %>%
      dplyr::pull("type")

    if (length(unmapped) > 0) {
      cli::cli_alert_warning("Unmapped type values: {paste(unmapped, collapse = ', ')}")
    }
  }

  # Ensure required columns exist
  if (!"id_n" %in% names(new_data_renamed)) {
    new_data_renamed <- new_data_renamed %>%
      dplyr::mutate(id_n = NA_integer_)
  }

  if (!"id_liste_plots" %in% names(new_data_renamed)) {
    new_data_renamed <- new_data_renamed %>%
      dplyr::mutate(id_liste_plots = NA_integer_)
  }

  if (!"id_linktype" %in% names(new_data_renamed)) {
    new_data_renamed <- new_data_renamed %>%
      dplyr::mutate(id_linktype = NA_integer_)
  }

  cli::cli_alert_info("Preparing to add {nrow(new_data_renamed)} links for {dplyr::n_distinct(new_data_renamed$id_specimen)} different specimens")

  # Check for duplicates with existing links
  existing_links <- dplyr::tbl(actual_con, "data_link_specimens") %>%
    dplyr::select("id_n", "id_specimen") %>%
    dplyr::collect()

  check_dup <- new_data_renamed %>%
    dplyr::inner_join(existing_links, by = c("id_n", "id_specimen"))

  if (nrow(check_dup) > 0) {
    cli::cli_alert_warning("{nrow(check_dup)} links already exist in database - excluding duplicates")
    new_data_renamed <- new_data_renamed %>%
      dplyr::anti_join(existing_links, by = c("id_n", "id_specimen"))
    cli::cli_alert_info("Links to add after excluding duplicates: {nrow(new_data_renamed)}")
  }

  if (nrow(new_data_renamed) == 0) {
    cli::cli_alert_warning("No new links to add - all already exist in database")
    return(invisible(new_data_renamed))
  }

  # Validate FK references - BATCH VALIDATION
  if (validate && nrow(new_data_renamed) > 0) {
    cli::cli_alert_info("Validating foreign key references for {nrow(new_data_renamed)} links...")

    # Get all unique IDs to validate
    all_specimen_ids <- unique(new_data_renamed$id_specimen)
    all_individual_ids <- unique(new_data_renamed$id_n)
    all_linktype_ids <- unique(new_data_renamed$id_linktype[!is.na(new_data_renamed$id_linktype)])

    cli::cli_alert_info("  Checking {length(all_specimen_ids)} specimens, {length(all_individual_ids)} individuals, {length(all_linktype_ids)} link types")

    # Batch query for specimens
    existing_specimens <- dplyr::tbl(actual_con, "specimens") %>%
      dplyr::filter(.data$id_specimen %in% !!all_specimen_ids) %>%
      dplyr::select("id_specimen") %>%
      dplyr::collect() %>%
      dplyr::pull("id_specimen")

    # Batch query for individuals
    existing_individuals <- dplyr::tbl(actual_con, "data_individuals") %>%
      dplyr::filter(.data$id_n %in% !!all_individual_ids) %>%
      dplyr::select("id_n") %>%
      dplyr::collect() %>%
      dplyr::pull("id_n")

    # Batch query for link types (skip if table doesn't exist)
    existing_linktypes <- tryCatch({
      if (length(all_linktype_ids) > 0) {
        dplyr::tbl(actual_con, "linktypelist") %>%
          dplyr::filter(.data$id_linktype %in% !!all_linktype_ids) %>%
          dplyr::select("id_linktype") %>%
          dplyr::collect() %>%
          dplyr::pull("id_linktype")
      } else {
        integer(0)
      }
    }, error = function(e) {
      cli::cli_alert_info("  Skipping linktype validation (table may not exist)")
      all_linktype_ids  # Assume all are valid if table doesn't exist
    })

    # Check for missing references
    missing_specimens <- setdiff(all_specimen_ids, existing_specimens)
    missing_individuals <- setdiff(all_individual_ids, existing_individuals)
    missing_linktypes <- setdiff(all_linktype_ids, existing_linktypes)

    validation_errors <- character()

    if (length(missing_specimens) > 0) {
      validation_errors <- c(validation_errors,
        paste0("Missing specimen IDs: ", paste(missing_specimens, collapse = ", ")))
    }

    if (length(missing_individuals) > 0) {
      validation_errors <- c(validation_errors,
        paste0("Missing individual IDs: ", paste(missing_individuals, collapse = ", ")))
    }

    if (length(missing_linktypes) > 0) {
      validation_errors <- c(validation_errors,
        paste0("Missing linktype IDs: ", paste(missing_linktypes, collapse = ", ")))
    }

    if (length(validation_errors) > 0) {
      cli::cli_alert_danger("Validation failed:")
      for (err in validation_errors) {
        cli::cli_li(err)
      }

      if (launch_adding_data) {
        cli::cli_alert_danger("Aborting due to validation errors. Fix errors and retry.")
        return(invisible(new_data_renamed))
      }
    } else {
      cli::cli_alert_success("All {nrow(new_data_renamed)} links passed validation")
    }
  }

  # Prepare data for insertion
  # Select columns that exist in database table
  db_columns <- c("id_specimen", "id_n", "id_linktype", "id_liste_plots", "type")
  data_to_add <- new_data_renamed %>%
    dplyr::select(dplyr::any_of(db_columns))

  # Add audit columns
  data_to_add <- data_to_add %>%
    dplyr::mutate(
      created_by = Sys.info()["user"],
      created_at = Sys.time()
    )

  cli::cli_h3("Data to add:")
  print(data_to_add %>% dplyr::select(-dplyr::all_of("created_at")))

  if (launch_adding_data) {
    tryCatch({
      DBI::dbWriteTable(actual_con, "data_link_specimens",
                        data_to_add, append = TRUE, row.names = FALSE)
      cli::cli_alert_success("Added {nrow(data_to_add)} links to data_link_specimens table")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to add links: {e$message}")
      stop(e)
    })
  } else {
    cli::cli_alert_warning("Dry run mode - set launch_adding_data=TRUE to actually add links")
  }

  return(invisible(data_to_add))
}


#' Query All Specimen Links for Individuals
#'
#' Returns ALL specimen links for specified individuals, including link type
#' information and specimen details. Unlike merge_individuals_taxa() which
#' returns only the primary link, this returns all links.
#'
#' @param id_ind Integer vector of individual IDs. If NULL, returns all links.
#' @param id_specimen Integer vector of specimen IDs. If NULL, ignored.
#' @param include_specimen_info Logical. If TRUE, joins specimen details.
#' @param include_linktype_info Logical. If TRUE, joins link type details.
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return Tibble with link information
#' @export
query_all_specimen_links <- function(id_ind = NULL,
                                     id_specimen = NULL,
                                     include_specimen_info = TRUE,
                                     include_linktype_info = TRUE,
                                     con = NULL) {

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

  # Start with links table
  links <- dplyr::tbl(actual_con, "data_link_specimens")

  # Filter by individual IDs
  if (!is.null(id_ind)) {
    links <- links %>%
      dplyr::filter(id_n %in% !!id_ind)
  }

  # Filter by specimen IDs
  if (!is.null(id_specimen)) {
    links <- links %>%
      dplyr::filter(id_specimen %in% !!id_specimen)
  }

  # Join link type info
  if (include_linktype_info) {
    links <- tryCatch({
      links %>%
        dplyr::left_join(
          dplyr::tbl(actual_con, "linktypelist") %>%
            dplyr::select(id_linktype, linktype, priority),
          by = "id_linktype"
        )
    }, error = function(e) {
      # Table might not exist yet
      links
    })
  }

  # Collect before joining specimens (different table structure)
  links <- links %>% dplyr::collect()

  # Join specimen info
  if (include_specimen_info && nrow(links) > 0) {
    specimen_info <- dplyr::tbl(actual_con, "specimens") %>%
      dplyr::select(id_specimen, idtax_n, id_colnam, colnbr, suffix,
                    detby, detd, detm, dety) %>%
      dplyr::collect()

    # Get collector names
    collector_names <- dplyr::tbl(actual_con, "table_colnam") %>%
      dplyr::select(id_table_colnam, colnam) %>%
      dplyr::collect()

    specimen_info <- specimen_info %>%
      dplyr::left_join(collector_names, by = c("id_colnam" = "id_table_colnam")) %>%
      dplyr::rename(collector_name = colnam)

    links <- links %>%
      dplyr::left_join(specimen_info, by = "id_specimen")
  }

  return(tibble::as_tibble(links))
}


#' Get Primary Specimen for Individuals
#'
#' Returns the primary (highest priority) specimen link for each individual.
#' Uses link type priority: type_individual (100) > referenced_individual (50).
#' If same priority, uses most recent determination date.
#'
#' @param id_ind Integer vector of individual IDs
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return Tibble with one row per individual (primary specimen only)
#' @export
get_primary_specimen_link <- function(id_ind = NULL, con = NULL) {

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

  # Get all links with priority
  links <- dplyr::tbl(actual_con, "data_link_specimens")

  if (!is.null(id_ind)) {
    links <- links %>%
      dplyr::filter(id_n %in% !!id_ind)
  }

  # Join link type for priority
  links <- tryCatch({
    links %>%
      dplyr::left_join(
        dplyr::tbl(actual_con, "linktypelist") %>%
          dplyr::select(id_linktype, linktype, priority),
        by = "id_linktype"
      )
  }, error = function(e) {
    links %>% dplyr::mutate(priority = 0L, linktype = NA_character_)
  })

  # Join specimen for determination date
  links <- links %>%
    dplyr::left_join(
      dplyr::tbl(actual_con, "specimens") %>%
        dplyr::select(id_specimen, dety, detm, detd),
      by = "id_specimen"
    ) %>%
    dplyr::collect()

  # Handle NULL/NA priority
  links <- links %>%
    dplyr::mutate(priority = dplyr::coalesce(priority, 0L))

  # Create determination date for sorting
  links <- links %>%
    dplyr::mutate(
      det_date = lubridate::make_date(
        dplyr::coalesce(dety, 1900L),
        dplyr::coalesce(detm, 1L),
        dplyr::coalesce(detd, 1L)
      )
    )

  # Sort by priority (desc), then determination date (desc), take first per individual
  primary <- links %>%
    dplyr::arrange(id_n, dplyr::desc(priority), dplyr::desc(det_date)) %>%
    dplyr::group_by(id_n) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-det_date)

  return(tibble::as_tibble(primary))
}


#' Find Unlinked Individuals with Herbarium Information
#'
#' Extract individuals that have herbarium specimen references in their
#' data but no formal links in data_link_specimens table.
#'
#' @param collector Character. Optional collector name to filter by.
#' @param ids Integer vector. Optional individual IDs to check.
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return List with:
#'   - all_herb_not_linked: Individuals with herbarium info but no links
#'   - all_linked_individuals: IDs of individuals that ARE linked
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
get_ref_specimen_ind <- function(collector = NULL, ids = NULL, con = NULL) {

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

  if (!is.null(collector)) {
    collector <- .link_colnam(
      data_stand = tibble::tibble(colnam = collector),
      column_searched = "colnam",
      column_name = "colnam",
      id_field = "id_colnam",
      id_table_name = "id_table_colnam",
      db_connection = actual_con,
      table_name = "table_colnam"
    )
  }

  # Get all individuals that already have links
  all_id_individuals_links <- dplyr::tbl(actual_con, "data_link_specimens") %>%
    dplyr::select(id_n) %>%
    dplyr::distinct(id_n) %>%
    dplyr::collect()

  # Get individuals with herbarium info but NOT linked
  all_herb_individuals <- dplyr::tbl(actual_con, "data_individuals") %>%
    dplyr::select(id_n, id_specimen, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type) %>%
    dplyr::filter(!id_n %in% !!all_id_individuals_links$id_n)

  if (!is.null(ids)) {
    all_herb_individuals <- all_herb_individuals %>%
      dplyr::filter(id_n %in% !!ids)
  }

  all_herb_individuals <- all_herb_individuals %>%
    dplyr::collect()

  # Filter to those with herbarium information
  all_herb_not_linked <- all_herb_individuals %>%
    dplyr::filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char))

  # Extract specimen number from herbarium_nbe_char
  all_herb_not_linked <- all_herb_not_linked %>%
    dplyr::filter(!is.na(herbarium_nbe_char)) %>%
    dplyr::mutate(
      herbarium_nbe_char = stringr::str_replace(herbarium_nbe_char, "-", " "),
      herbarium_nbe_char = stringr::str_replace_all(herbarium_nbe_char, "[.]", " "),
      nbrs = readr::parse_number(herbarium_nbe_char),
      nbrs = ifelse(nbrs < 1, nbrs * -1, nbrs)
    ) %>%
    dplyr::arrange(dplyr::desc(nbrs))

  # Extract collector name
  all_herb_not_linked <- all_herb_not_linked %>%
    dplyr::mutate(
      coll = stringr::str_replace(herbarium_nbe_char, as.character(nbrs), ""),
      coll = stringr::str_trim(coll)
    )

  # Link collector to table_colnam
  all_herb_not_linked <- .link_colnam(
    data_stand = all_herb_not_linked,
    column_searched = "coll",
    column_name = "colnam",
    id_field = "id_colnam",
    id_table_name = "id_table_colnam",
    db_connection = actual_con,
    table_name = "table_colnam"
  )

  # Add plot information
  all_herb_not_linked <- all_herb_not_linked %>%
    dplyr::left_join(
      dplyr::tbl(actual_con, "data_individuals") %>%
        dplyr::select(id_n, id_table_liste_plots_n, idtax_n) %>%
        dplyr::collect(),
      by = "id_n"
    ) %>%
    dplyr::left_join(
      dplyr::tbl(actual_con, "data_liste_plots") %>%
        dplyr::select(id_liste_plots, plot_name, team_leader) %>%
        dplyr::collect(),
      by = c("id_table_liste_plots_n" = "id_liste_plots")
    )

  # Get all linked individuals for reference
  all_linked_individuals <- dplyr::tbl(actual_con, "data_link_specimens") %>%
    dplyr::distinct(id_n) %>%
    dplyr::collect()

  return(list(
    all_herb_not_linked = all_herb_not_linked,
    all_linked_individuals = all_linked_individuals
  ))
}


#' Query Unmatched Specimens (Internal)
#'
#' Find specimens that have taxonomic discrepancies or are not properly linked.
#'
#' @param con Database connection. If NULL, calls call.mydb()
#'
#' @return List with problematic specimens
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
.query_unmatched_specimens <- function(con = NULL) {

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

  all_herbarium_individuals <- dplyr::tbl(actual_con, "data_individuals") %>%
    dplyr::select(herbarium_nbe_char, herbarium_code_char, herbarium_nbe_type,
                  id_diconame_n, id_specimen, id_n) %>%
    dplyr::filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char) | !is.na(herbarium_nbe_type)) %>%
    dplyr::collect()

  # Specimens with more than one id_diconame
  all_herbarium_individuals_not_linked_diff_tax <- all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1)

  # Get names for those specimens
  all_herbarium_individuals_not_linked_diff_tax <- all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::left_join(
      dplyr::tbl(actual_con, "diconame") %>%
        dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
        dplyr::collect(),
      by = c("id_diconame_n" = "id_n")
    ) %>%
    dplyr::filter(herbarium_nbe_char %in% all_herbarium_individuals_not_linked_diff_tax$herbarium_nbe_char) %>%
    dplyr::arrange(herbarium_nbe_char)

  # Specimens with more than one genus
  herb_specimen_diff_gen <- all_herbarium_individuals_not_linked_diff_tax %>%
    dplyr::distinct(herbarium_nbe_char, tax_gen) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n > 1)

  # Individuals concerned by multi-genus specimens
  data_individuals_concerned <- dplyr::tbl(actual_con, "data_individuals") %>%
    dplyr::filter(herbarium_nbe_char %in% !!herb_specimen_diff_gen$herbarium_nbe_char) %>%
    dplyr::collect() %>%
    dplyr::select(dbh, code_individu, tag, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type, id_diconame_n) %>%
    dplyr::left_join(
      dplyr::tbl(actual_con, "diconame") %>%
        dplyr::select(id_n, full_name_no_auth, tax_gen, tax_esp, tax_fam) %>%
        dplyr::collect(),
      by = c("id_diconame_n" = "id_n")
    ) %>%
    dplyr::arrange(herbarium_nbe_char)

  # Extract specimens not linked, excluding problematic ones
  all_herbarium_individuals_not_linked <- all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::filter(!herbarium_nbe_char %in% all_herbarium_individuals_not_linked_diff_tax$herbarium_nbe_char)

  # Extract collector and number
  regexp <- "[[:digit:]]+"
  num_extracted <- stringr::str_extract(all_herbarium_individuals_not_linked$herbarium_nbe_char, regexp)

  df <- tibble::tibble(
    full = all_herbarium_individuals_not_linked$herbarium_nbe_char,
    num = num_extracted
  )

  coll_extracted <- apply(df, MARGIN = 1, FUN = function(x) gsub(x[2], "", x[1]))
  coll_extracted <- trimws(coll_extracted)

  all_herbarium_individuals_not_linked <- all_herbarium_individuals_not_linked %>%
    tibble::add_column(col_name = coll_extracted) %>%
    tibble::add_column(colnbr = num_extracted)

  all_herbarium_individuals_not_linked <- .link_colnam(
    data_stand = all_herbarium_individuals_not_linked,
    column_searched = "col_name",
    column_name = "colnam",
    id_field = "id_colnam",
    id_table_name = "id_table_colnam",
    db_connection = actual_con,
    table_name = "table_colnam"
  )

  return(list(
    all_herbarium_individuals_not_linked_diff_tax = all_herbarium_individuals_not_linked_diff_tax,
    data_individuals_not_linked_diff_tax_concerned = data_individuals_concerned,
    all_herbarium_individuals_not_linked = all_herbarium_individuals_not_linked
  ))
}
