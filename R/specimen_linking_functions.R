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
# - reference_plot: Specimen collected within a plot, individual unknown (priority 10)
#
# A link type declares its scope in linktypelist.scope:
# - 'individual' links fill id_n and leave id_liste_plots NULL
# - 'plot' links fill id_liste_plots and leave id_n NULL
#
# Priority orders the links of one individual when the code must pick the one
# that governs its determination. Plot links carry no id_n, so they never enter
# that sort; their priority only keeps them out of the way in the linking UI.
#
# Dependencies: DBI, dplyr, cli, tibble


#' Get Link Types from Lookup Table
#'
#' Returns the available link types from the linktypelist lookup table.
#'
#' @param con Database connection. If NULL, calls call.mydb()
#' @param scope Character. Restrict to link types of this scope: `"individual"`
#'   for types that link a specimen to a tree, `"plot"` for types that link it
#'   to a plot only. `NULL` (the default) returns every type.
#'
#' @return Tibble with id_linktype, linktype, description, priority, scope columns
#'
#' @details
#' `scope` was added by the `reference_plot_linktype` migration. On a database
#' where that has not run the column is absent, and it is reconstructed here
#' from the link type names so that callers can rely on it either way.
#'
#' @export
get_linktypes <- function(con = NULL, scope = NULL) {
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
    dplyr::tbl(actual_con, "linktypelist") %>%
      dplyr::collect() %>%
      dplyr::arrange(dplyr::desc(priority))
  }, error = function(e) {
    cli::cli_alert_warning("Could not fetch link types: {e$message}")
    # Return default values if table doesn't exist yet
    dplyr::tibble(
      id_linktype = c(1L, 2L, 3L),
      linktype = c("type_individual", "referenced_individual", "reference_plot"),
      description = c("Specimen collected from this specific individual",
                      "Specimen represents same species but from different individual",
                      "Specimen collected within this plot; the individual tree is not identified"),
      priority = c(100L, 50L, 10L),
      scope = c("individual", "individual", "plot")
    )
  })

  result <- .ensure_linktype_scope(result)

  if (!is.null(scope)) {
    result <- result %>% dplyr::filter(.data$scope %in% !!scope)
  }

  result
}


#' Ensure a Link Type Table Carries a scope Column
#'
#' `linktypelist.scope` is added by the `reference_plot_linktype` migration.
#' Before it has run the column is absent, so it is reconstructed from the link
#' type names: everything is individual-level except `reference_plot`. This
#' lets the rest of the package treat scope as always present.
#'
#' @param linktypes Tibble as returned by the linktypelist query
#'
#' @return The same tibble, with a `scope` column
#' @keywords internal
.ensure_linktype_scope <- function(linktypes) {
  if ("scope" %in% names(linktypes)) {
    return(linktypes)
  }

  plot_level_types <- c("reference_plot")

  linktypes %>%
    dplyr::mutate(
      scope = ifelse(.data$linktype %in% plot_level_types, "plot", "individual")
    )
}


#' Validate Specimen Link Before Adding
#'
#' Internal function to validate that all foreign key references exist.
#'
#' @param id_specimen Integer specimen ID
#' @param id_n Integer individual ID. `NA` for a plot-level link.
#' @param id_linktype Integer link type ID
#' @param con Database connection
#' @param id_liste_plots Integer plot ID. `NA` for an individual-level link.
#'
#' @return List with valid (logical) and errors (character vector)
#' @keywords internal
.validate_specimen_link <- function(id_specimen, id_n, id_linktype, con,
                                    id_liste_plots = NA_integer_) {
  errors <- character()

  has_individual <- !is.null(id_n) && !is.na(id_n)
  has_plot <- !is.null(id_liste_plots) && !is.na(id_liste_plots)

  # Validate id_specimen exists in specimens table
  specimen_exists <- dplyr::tbl(con, "specimens") %>%
    dplyr::filter(id_specimen == !!id_specimen) %>%
    dplyr::count() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  if (specimen_exists == 0) {
    errors <- c(errors, paste0("Specimen ID ", id_specimen, " not found in specimens table"))
  }

  # A link has to attach the specimen to something
  if (!has_individual && !has_plot) {
    errors <- c(errors, "Link has neither an individual (id_n) nor a plot (id_liste_plots)")
  }

  # Validate id_n exists in data_individuals table
  if (has_individual) {
    individual_exists <- dplyr::tbl(con, "data_individuals") %>%
      dplyr::filter(id_n == !!id_n) %>%
      dplyr::count() %>%
      dplyr::collect() %>%
      dplyr::pull(n)

    if (individual_exists == 0) {
      errors <- c(errors, paste0("Individual ID ", id_n, " not found in data_individuals table"))
    }
  }

  # Validate id_liste_plots exists in data_liste_plots table
  if (has_plot) {
    plot_exists <- dplyr::tbl(con, "data_liste_plots") %>%
      dplyr::filter(id_liste_plots == !!id_liste_plots) %>%
      dplyr::count() %>%
      dplyr::collect() %>%
      dplyr::pull(n)

    if (plot_exists == 0) {
      errors <- c(errors, paste0("Plot ID ", id_liste_plots, " not found in data_liste_plots table"))
    }
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


#' Check Links Against the Scope of Their Link Type
#'
#' An `individual` link type fills `id_n` and leaves `id_liste_plots` NULL; a
#' `plot` type does the reverse. This reports rows that do neither.
#'
#' @param links Tibble with id_linktype, id_n and id_liste_plots columns
#' @param con Database connection
#'
#' @return Character vector of error messages, empty if every link is coherent
#' @keywords internal
.check_link_scope <- function(links, con) {
  if (nrow(links) == 0 || !"id_linktype" %in% names(links)) {
    return(character())
  }

  linktypes <- tryCatch(
    get_linktypes(con),
    error = function(e) NULL
  )
  if (is.null(linktypes) || nrow(linktypes) == 0) {
    return(character())
  }

  # A link whose type is unknown cannot be checked against a scope.
  annotated <- links %>%
    dplyr::filter(!is.na(.data$id_linktype)) %>%
    dplyr::left_join(
      linktypes %>% dplyr::select(dplyr::all_of(c("id_linktype", "linktype", "scope"))),
      by = "id_linktype"
    )

  errors <- character()

  bad_individual <- annotated %>%
    dplyr::filter(.data$scope == "individual" & is.na(.data$id_n))
  if (nrow(bad_individual) > 0) {
    errors <- c(errors, paste0(
      nrow(bad_individual), " link(s) of an individual-level type (",
      paste(unique(bad_individual$linktype), collapse = ", "), ") have no id_n"
    ))
  }

  bad_plot <- annotated %>%
    dplyr::filter(.data$scope == "plot" & is.na(.data$id_liste_plots))
  if (nrow(bad_plot) > 0) {
    errors <- c(errors, paste0(
      nrow(bad_plot), " link(s) of a plot-level type (",
      paste(unique(bad_plot$linktype), collapse = ", "), ") have no id_liste_plots"
    ))
  }

  crossed <- annotated %>%
    dplyr::filter(.data$scope == "plot" & !is.na(.data$id_n))
  if (nrow(crossed) > 0) {
    errors <- c(errors, paste0(
      nrow(crossed), " link(s) of a plot-level type also carry an id_n - ",
      "use an individual-level type instead"
    ))
  }

  errors
}


#' Add Link Between Specimen and Individual
#'
#' Creates links between herbarium specimens and individual trees in the database.
#' Includes validation to ensure foreign key references exist.
#'
#' @param new_data Tibble with column id_specimen, either id_linktype or type,
#'   and - depending on the scope of that link type - id_n (individual-level)
#'   or id_liste_plots (plot-level).
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
#' Link types, and the column each one fills:
#' - type_individual: specimen collected from this specific individual (id_n)
#' - referenced_individual: specimen of the same species from a different
#'   individual (id_n)
#' - reference_plot: specimen collected within a plot, the individual tree
#'   unknown (id_liste_plots)
#'
#' Which of the two a type expects is `linktypelist.scope`, checked here.
#'
#' The function:
#' 1. Renames columns to standard names
#' 2. Checks for duplicate links (same specimen, individual, type and plot)
#' 3. Validates FK references and link scope if validate=TRUE
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

  # Keep the legacy free-text `type` column in step with id_linktype. It is
  # redundant, but the 74 pre-existing plot links are keyed on it and the
  # reference_plot migration reads it, so letting the two drift is worse than
  # writing both.
  if (!"type" %in% names(new_data_renamed)) {
    linktype_names <- tryCatch(
      get_linktypes(actual_con) %>%
        dplyr::select(dplyr::all_of(c("id_linktype", "linktype"))) %>%
        dplyr::rename(type = "linktype"),
      error = function(e) NULL
    )

    new_data_renamed <- if (is.null(linktype_names)) {
      new_data_renamed %>% dplyr::mutate(type = NA_character_)
    } else {
      new_data_renamed %>%
        dplyr::left_join(linktype_names, by = "id_linktype")
    }
  }

  cli::cli_alert_info("Preparing to add {nrow(new_data_renamed)} links for {dplyr::n_distinct(new_data_renamed$id_specimen)} different specimens")

  # Check for duplicates with existing links. id_liste_plots belongs in the key:
  # a plot-level link has a NULL id_n, and dplyr matches NA to NA by default, so
  # without it two links to different plots would look like the same link.
  dup_key <- c("id_n", "id_specimen", "id_linktype", "id_liste_plots")

  existing_links <- dplyr::tbl(actual_con, "data_link_specimens") %>%
    dplyr::select(dplyr::all_of(dup_key)) %>%
    dplyr::collect()

  check_dup <- new_data_renamed %>%
    dplyr::inner_join(existing_links, by = dup_key)

  if (nrow(check_dup) > 0) {
    cli::cli_alert_warning("{nrow(check_dup)} links already exist in database - excluding duplicates")
    new_data_renamed <- new_data_renamed %>%
      dplyr::anti_join(existing_links, by = dup_key)
    cli::cli_alert_info("Links to add after excluding duplicates: {nrow(new_data_renamed)}")
  }

  if (nrow(new_data_renamed) == 0) {
    cli::cli_alert_warning("No new links to add - all already exist in database")
    return(invisible(new_data_renamed))
  }

  # Validate FK references - BATCH VALIDATION
  if (validate && nrow(new_data_renamed) > 0) {
    cli::cli_alert_info("Validating foreign key references for {nrow(new_data_renamed)} links...")

    # Get all unique IDs to validate. NAs are dropped rather than looked up: a
    # plot-level link has no individual and an individual-level link no plot,
    # and setdiff() would otherwise report the NA itself as a missing ID.
    all_specimen_ids <- unique(new_data_renamed$id_specimen)
    all_individual_ids <- unique(new_data_renamed$id_n[!is.na(new_data_renamed$id_n)])
    all_plot_ids <- unique(new_data_renamed$id_liste_plots[!is.na(new_data_renamed$id_liste_plots)])
    all_linktype_ids <- unique(new_data_renamed$id_linktype[!is.na(new_data_renamed$id_linktype)])

    cli::cli_alert_info("  Checking {length(all_specimen_ids)} specimens, {length(all_individual_ids)} individuals, {length(all_plot_ids)} plots, {length(all_linktype_ids)} link types")

    # Batch query for specimens
    existing_specimens <- dplyr::tbl(actual_con, "specimens") %>%
      dplyr::filter(.data$id_specimen %in% !!all_specimen_ids) %>%
      dplyr::select("id_specimen") %>%
      dplyr::collect() %>%
      dplyr::pull("id_specimen")

    # Batch query for individuals
    existing_individuals <- if (length(all_individual_ids) > 0) {
      dplyr::tbl(actual_con, "data_individuals") %>%
        dplyr::filter(.data$id_n %in% !!all_individual_ids) %>%
        dplyr::select("id_n") %>%
        dplyr::collect() %>%
        dplyr::pull("id_n")
    } else {
      integer(0)
    }

    # Batch query for plots (plot-level links, e.g. reference_plot)
    existing_plots <- if (length(all_plot_ids) > 0) {
      dplyr::tbl(actual_con, "data_liste_plots") %>%
        dplyr::filter(.data$id_liste_plots %in% !!all_plot_ids) %>%
        dplyr::select("id_liste_plots") %>%
        dplyr::collect() %>%
        dplyr::pull("id_liste_plots")
    } else {
      integer(0)
    }

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
    missing_plots <- setdiff(all_plot_ids, existing_plots)
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

    if (length(missing_plots) > 0) {
      validation_errors <- c(validation_errors,
        paste0("Missing plot IDs: ", paste(missing_plots, collapse = ", ")))
    }

    if (length(missing_linktypes) > 0) {
      validation_errors <- c(validation_errors,
        paste0("Missing linktype IDs: ", paste(missing_linktypes, collapse = ", ")))
    }

    # Every link must attach the specimen to something
    orphan_links <- new_data_renamed %>%
      dplyr::filter(is.na(.data$id_n) & is.na(.data$id_liste_plots))

    if (nrow(orphan_links) > 0) {
      validation_errors <- c(validation_errors,
        paste0(nrow(orphan_links),
               " link(s) have neither an individual (id_n) nor a plot (id_liste_plots)"))
    }

    # ... and must attach it to the end its link type declares
    scope_errors <- .check_link_scope(new_data_renamed, actual_con)
    validation_errors <- c(validation_errors, scope_errors)

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

  return(dplyr::as_tibble(links))
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

  # Get all links with priority. Plot-level links (reference_plot) have no
  # id_n and cannot be the primary specimen of any individual; without this
  # they would group into a spurious NA row when id_ind is NULL.
  links <- dplyr::tbl(actual_con, "data_link_specimens") %>%
    dplyr::filter(!is.na(.data$id_n))

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

  return(dplyr::as_tibble(primary))
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
      data_stand = dplyr::tibble(colnam = collector),
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

  df <- dplyr::tibble(
    full = all_herbarium_individuals_not_linked$herbarium_nbe_char,
    num = num_extracted
  )

  coll_extracted <- apply(df, MARGIN = 1, FUN = function(x) gsub(x[2], "", x[1]))
  coll_extracted <- trimws(coll_extracted)

  all_herbarium_individuals_not_linked <- all_herbarium_individuals_not_linked %>%
    dplyr::mutate(col_name = coll_extracted, colnbr = num_extracted)

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
