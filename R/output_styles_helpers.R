#' Apply output style to query_plots results
#'
#' @description
#' Restructures query_plots() flat table output into a list of organized tables
#' based on the selected output style.
#'
#' @param data Data frame or list returned by query_plots()
#' @param style Character: output style name
#' @param extract_individuals Logical: were individuals extracted?
#' @param show_multiple_census Logical: were multiple censuses shown?
#'
#' @return List of data frames organized by output style
#'
#' @keywords internal
#' @noRd
.apply_output_style <- function(data, style, extract_individuals, show_multiple_census = FALSE) {

  # Resolve `style` -> (style_name, style_config). Supports:
  #   * character scalar matching a built-in name
  #   * a plot_output_style object (custom, built with output_style())
  if (inherits(style, "plot_output_style")) {
    style_name   <- attr(style, "style_name") %||% "<custom>"
    style_config <- unclass(style)
  } else if (is.character(style) && length(style) == 1L) {
    style_name   <- style
    style_config <- .plot_output_styles[[style_name]]
    if (is.null(style_config)) {
      cli::cli_alert_warning("Unknown style '{style_name}', using 'standard'")
      style_name   <- "standard"
      style_config <- .plot_output_styles[[style_name]]
    }
  } else {
    cli::cli_alert_warning("Invalid `style` input, using 'standard'")
    style_name   <- "standard"
    style_config <- .plot_output_styles[[style_name]]
  }

  # Adjust style based on show_multiple_census for permanent plots — only when
  # the user requested the built-in "permanent_plot" by name. Custom styles
  # built with output_style() are respected as-is.
  if (identical(style_name, "permanent_plot") && show_multiple_census) {
    style_name   <- "permanent_plot_multi_census"
    style_config <- .plot_output_styles[[style_name]]
    cli::cli_alert_info("Using 'permanent_plot_multi_census' style for multiple census data")
  }

  style <- style_name  # keep `style` name used by downstream attribute setters

  # Handle data structure (could be data frame or list from current query_plots)
  if (is.list(data) && !is.data.frame(data)) {
    # Already a list (has $extract, $census_features, etc.)
    main_data <- data$extract
    meta_data <- data$meta_data
    census_features <- data$census_features
    hd_source <- data$hd_source
    coordinates <- data$coordinates
    coordinates_sf <- data$coordinates_sf
  } else {
    # Simple data frame
    main_data <- data
    meta_data <- NULL
    census_features <- NULL
    hd_source <- NULL
    coordinates <- NULL
    coordinates_sf <- NULL
  }

  # Initialize result list
  result <- list()

  # 1. Extract metadata table
  result$metadata <- .extract_metadata_table(
    data = main_data,
    meta_data = meta_data,
    style_config = style_config,
    extract_individuals = extract_individuals
  )

  # 2. Extract individuals table (if individuals were extracted)
  if (extract_individuals) {
    result$individuals <- .extract_individuals_table(data = main_data, style_config, show_multiple_census)
  }

  # 3. Extract additional tables based on style
  if ("censuses" %in% style_config$additional_tables && !is.null(census_features)) {
    result$censuses <- .extract_census_table(census_features, main_data)
  }

  if ("height_diameter" %in% style_config$additional_tables && extract_individuals) {
    hd_pairs <- .extract_height_diameter_pairs(
      data               = main_data,
      show_multiple_census = show_multiple_census,
      hd_source          = hd_source
    )
    if (!is.null(hd_pairs) && nrow(hd_pairs) > 0) {
      result$height_diameter <- hd_pairs
    }
  }

  # 4. Add coordinates if available (from extract_coordinates = TRUE)
  if (!is.null(coordinates)) {
    result$coordinates <- coordinates
  }

  if (!is.null(coordinates_sf)) {
    result$coordinates_sf <- coordinates_sf
  }

  # Add class and attributes
  class(result) <- c("plot_query_list", "list")
  attr(result, "style") <- style
  attr(result, "style_description") <- style_config$description

  return(result)
}


#' Extract metadata table from query results
#'
#' @description
#' Extracts plot-level metadata from query results. Always renames id_liste_plots
#' to plot_id regardless of output style, so chaining queries is always consistent:
#' query_plots(id_plot = metadata$metadata$plot_id).
#'
#' @keywords internal
#' @noRd
.extract_metadata_table <- function(data, meta_data = NULL, style_config, extract_individuals) {

  # If meta_data table is provided (from res_list$meta_data), use it as the source
  # This table was created before individual extraction and has all plot-level columns
  source_data <- if (!is.null(meta_data) && is.data.frame(meta_data)) {
    meta_data
  } else {
    data
  }

  # Get columns to keep
  meta_cols <- style_config$metadata_columns

  # Handle "all" case
  if (length(meta_cols) == 1 && meta_cols == "all") {
    if (extract_individuals && is.null(meta_data)) {
      # Only need to filter out individual columns if using data (not meta_data)
      result_meta <- source_data %>%
        dplyr::select(-dplyr::matches("^(dbh|height|pom|tag|id_n)")) %>%
        dplyr::distinct(.data$plot_name, .keep_all = TRUE)
    } else {
      # meta_data already has only plot-level columns
      result_meta <- source_data %>%
        dplyr::distinct(.data$plot_name, .keep_all = TRUE)
    }
    return(result_meta)
  }

  # Get available columns from source data
  available_cols <- names(source_data)

  # Always keep id_liste_plots (similar to id_n for individuals)
  keep_cols <- "id_liste_plots"

  # Add specified columns that exist
  specified_cols <- setdiff(meta_cols, "id_liste_plots")
  keep_cols <- c(keep_cols, intersect(specified_cols, available_cols))

  # Handle common features
  if (!is.null(style_config$keep_common_features) && style_config$keep_common_features) {
    feat_cols <- grep("^feat_", available_cols, value = TRUE)
    # Keep features present in >10% of rows (not all NA)
    if (length(feat_cols) > 0) {
      common_feats <- feat_cols[sapply(source_data[feat_cols], function(x) mean(!is.na(x)) > 0.1)]
      keep_cols <- c(keep_cols, common_feats)
    }
  }

  # Apply keep patterns - add columns matching specified patterns
  if (!is.null(style_config$keep_patterns) && length(style_config$keep_patterns) > 0) {
    for (pattern in style_config$keep_patterns) {
      pattern_cols <- grep(pattern, available_cols, value = TRUE, perl = TRUE)
      keep_cols <- unique(c(keep_cols, pattern_cols))
    }
  }

  # Remove patterns
  if (!is.null(style_config$remove_patterns) && length(style_config$remove_patterns) > 0) {
    for (pattern in style_config$remove_patterns) {
      keep_cols <- grep(pattern, keep_cols, value = TRUE, invert = TRUE, perl = TRUE)
    }
  }

  # Select columns
  keep_cols <- intersect(keep_cols, available_cols)

  if (length(keep_cols) == 0) {
    # Fallback to essential columns
    keep_cols <- c("plot_name", "country", "ddlat", "ddlon")
    keep_cols <- intersect(keep_cols, available_cols)
  }

  # Get unique by plot
  meta_data <- source_data %>%
    dplyr::select(dplyr::any_of(keep_cols)) %>%
    dplyr::distinct(.data$plot_name, .data$id_liste_plots, .keep_all = TRUE)

  # Apply column renaming if specified
  if (!is.null(style_config$rename_columns) && !is.null(style_config$rename_columns$metadata)) {
    renames <- style_config$rename_columns$metadata
    # Only rename columns that exist
    renames <- renames[names(renames) %in% names(meta_data)]
    if (length(renames) > 0) {
      meta_data <-
        meta_data %>%
        dplyr::rename(!!!rlang::set_names(names(renames), renames))
    }
  }

  # Always rename id_liste_plots -> plot_id regardless of output style, so that
  # chaining is consistent: query_plots(id_plot = metadata$metadata$plot_id)
  if ("id_liste_plots" %in% names(meta_data)) {
    meta_data <- meta_data %>% dplyr::rename(plot_id = "id_liste_plots")
  }

  return(meta_data)
}


#' Extract individuals table from query results
#'
#' @keywords internal
#' @noRd
.extract_individuals_table <- function(data, style_config, show_multiple_census) {

  # Get columns to keep
  indiv_cols <- style_config$individuals_columns

  # Handle "all" case
  if (length(indiv_cols) == 1 && indiv_cols == "all") {
    return(data)
  }

  # Get available columns
  available_cols <- names(data)

  # Always keep id_n
  keep_cols <- "id_n"

  # Add specified columns that exist
  specified_cols <- setdiff(indiv_cols, "id_n")
  keep_cols <- c(keep_cols, intersect(specified_cols, available_cols))

  # Handle census columns based on style configuration
  if (show_multiple_census && !is.null(style_config$keep_census_columns) && style_config$keep_census_columns) {
    # Keep all census-suffixed columns (e.g., stem_diameter_census_1, stem_diameter_census_2)
    census_cols <- grep("_census_\\d+$", available_cols, value = TRUE)
    keep_cols <- unique(c(keep_cols, census_cols))
  }

  # Apply keep patterns - add columns matching specified patterns
  if (!is.null(style_config$keep_patterns) && length(style_config$keep_patterns) > 0) {
    for (pattern in style_config$keep_patterns) {
      pattern_cols <- grep(pattern, available_cols, value = TRUE, perl = TRUE)
      keep_cols <- unique(c(keep_cols, pattern_cols))
    }
  }

  # Remove patterns (unless explicitly keeping census columns)
  if (!is.null(style_config$remove_patterns) && length(style_config$remove_patterns) > 0) {
    for (pattern in style_config$remove_patterns) {
      # Skip pattern if it's for census columns and we're keeping them
      if (pattern == "_census_\\d+$" && !is.null(style_config$keep_census_columns) && style_config$keep_census_columns) {
        next
      }
      keep_cols <- grep(pattern, keep_cols, value = TRUE, invert = TRUE, perl = TRUE)
    }
  }

  # Make sure we have at least basic columns
  essential <- c("id_n", "plot_name", "tag", "tax_fam", "tax_gen", "tax_sp_level", "stem_diameter")
  essential <- intersect(essential, available_cols)
  keep_cols <- unique(c(essential, keep_cols))

  # Select columns
  keep_cols <- intersect(keep_cols, available_cols)

  indiv_data <- data %>%
    dplyr::select(any_of(keep_cols))

  # Apply column renaming if specified
  if (!is.null(style_config$rename_columns) && !is.null(style_config$rename_columns$individuals)) {
    renames <- style_config$rename_columns$individuals
    # Only rename columns that exist
    renames <- renames[names(renames) %in% names(indiv_data)]
    if (length(renames) > 0) {
      indiv_data <- indiv_data %>%
        dplyr::rename(!!!rlang::set_names(names(renames), renames))
    }
  }

  # Apply census column renaming if specified (e.g., stem_diameter_census_1 -> dbh_census_1)
  if (!is.null(style_config$census_column_renames) && show_multiple_census) {
    census_renames <- style_config$census_column_renames
    current_names <- names(indiv_data)

    # Build rename vector for all census columns
    rename_vector <- c()
    for (old_prefix in names(census_renames)) {
      new_prefix <- census_renames[[old_prefix]]
      # Find all columns matching pattern: old_prefix_census_N
      pattern <- paste0("^", old_prefix, "_census_\\d+$")
      matching_cols <- grep(pattern, current_names, value = TRUE)

      for (old_col in matching_cols) {
        # Extract census number and build new name
        census_num <- sub(paste0("^", old_prefix, "_census_(\\d+)$"), "\\1", old_col)
        new_col <- paste0(new_prefix, "_census_", census_num)
        rename_vector[old_col] <- new_col
      }
    }

    if (length(rename_vector) > 0) {
      indiv_data <- indiv_data %>%
        dplyr::rename(!!!rlang::set_names(names(rename_vector), rename_vector))
      # %>%
      #   dplyr::rename(!!!rename_vector)
    }
  }

  return(indiv_data)
}


#' Extract census table from census features
#'
#' @description
#' Creates a table with one row per plot per census, including census metadata
#' like date, census number, and people involved.
#'
#' @param census_features Data frame with census feature information
#' @param main_data Main query results for joining plot_name
#'
#' @return Data frame with census information
#'
#' @keywords internal
#' @noRd
.extract_census_table <- function(census_features, main_data) {

  if (is.null(census_features) || !is.data.frame(census_features)) {
    return(NULL)
  }

  # Get unique plots
  plots <- unique(main_data$plot_name)

  # Basic census info
  census_info <- census_features %>%
    dplyr::filter(.data$plot_name %in% plots) %>%
    dplyr::select("plot_name", "typevalue", "year", "month") %>%
    dplyr::distinct()

  # Create census_date with robust handling of missing/invalid data
  if ("year" %in% names(census_info) && "month" %in% names(census_info)) {
    census_info <- census_info %>%
      dplyr::mutate(
        # Ensure numeric and valid ranges
        year_num = suppressWarnings(as.integer(.data$year)),
        month_num = suppressWarnings(as.integer(.data$month)),
        # Flag valid date components (year > 1900 and month 1-12)
        valid_date = !is.na(.data$year_num) & !is.na(.data$month_num) &
                     .data$year_num > 1900 & .data$year_num < 2100 &
                     .data$month_num >= 1 & .data$month_num <= 12
      )

    # Create census_date only if there are valid entries (avoid as.Date error on empty/all-NA)
    if (any(census_info$valid_date, na.rm = TRUE)) {
      census_info <- census_info %>%
        dplyr::mutate(
          census_date = dplyr::case_when(
            .data$valid_date ~ paste0(.data$year_num, "-", sprintf("%02d", .data$month_num)),
            TRUE ~ NA_character_
          )
        )
    } else {
      # No valid dates - add empty census_date column
      census_info$census_date <- NA_character_
    }

    census_info <- census_info %>%
      dplyr::select(-dplyr::all_of(c("year", "month", "year_num", "month_num", "valid_date")))
  }

  # Rename typevalue to census_number
  if ("typevalue" %in% names(census_info)) {
    census_info <- census_info %>%
      dplyr::rename(census_number = "typevalue")
  }

  # Add people involved (team_leader, additional_people, principal_investigator)
  people_cols <- c("team_leader", "additional_people", "principal_investigator")

  for (col in people_cols) {
    if (col %in% names(census_features)) {
      people_data <- census_features %>%
        dplyr::filter(.data$plot_name %in% plots) %>%
        dplyr::select("plot_name", "typevalue", dplyr::all_of(col)) %>%
        dplyr::distinct()

      census_info <- census_info %>%
        dplyr::left_join(
          people_data,
          by = c("plot_name", "census_number" = "typevalue")
        )
    }
  }

  # Order by plot and census number
  census_info <- census_info %>%
    dplyr::arrange(.data$plot_name, .data$census_number)

  return(census_info)
}


#' Extract height-diameter pairs from individual data
#'
#' @description
#' Extracts all stem diameter and tree height pairs across all available censuses.
#' When \code{hd_source} is provided (long-format all-census data), it is used as
#' the primary source so that measurements from non-selected censuses are included.
#' Census date is added when measurements are linked to a specific census.
#'
#' @param data Main query results with individual measurements (used for id/tag lookup)
#' @param show_multiple_census Logical: were multiple censuses shown in main data?
#' @param hd_source Long-format tibble from \code{query_individual_features} with
#'   all-census height/diameter data. When provided, used instead of \code{data}.
#'
#' @return Data frame with id_n, plot_name, tag, D (dbh), H (height), and optionally
#'   POM and census_date columns
#'
#' @keywords internal
#' @noRd
.extract_height_diameter_pairs <- function(data, show_multiple_census, hd_source = NULL) {

  # -----------------------------------------------------------------------
  # Path 1: use all-census long-format source when available
  # -----------------------------------------------------------------------
  if (!is.null(hd_source) && nrow(hd_source) > 0) {

    hd_traits <- c("tree_height", "stem_diameter", "height_of_stem_diameter")

    hd_long <- hd_source %>%
      dplyr::filter(.data$trait %in% hd_traits)

    if (nrow(hd_long) == 0) {
      return(NULL)
    }

    # Build census_date from year/month/day when available
    if ("census_year" %in% names(hd_long)) {
      hd_long <- hd_long %>%
        dplyr::mutate(
          census_date = suppressWarnings(
            lubridate::make_date(
              year  = .data$census_year,
              month = dplyr::coalesce(.data$census_month, 1L),
              day   = dplyr::coalesce(.data$census_day,   1L)
            )
          ),
          # Set census_date to NA when there is no census linkage
          census_date = dplyr::if_else(is.na(.data$census_name), lubridate::NA_Date_, .data$census_date)
        ) %>%
        dplyr::select(-dplyr::any_of(c("census_typevalue", "census_day", "census_month", "census_year")))
    }

    # Pivot to wide: one row per (individual, census)
    id_cols <- c("id_data_individuals", "census_name", "census_date")
    id_cols <- intersect(id_cols, names(hd_long))

    hd_wide <- hd_long %>%
      dplyr::select(dplyr::all_of(id_cols), "trait", "traitvalue") %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(id_cols),
        names_from  = "trait",
        values_from = "traitvalue",
        values_fn   = mean
      )

    if (!"tree_height" %in% names(hd_wide) || nrow(hd_wide) == 0) {
      return(NULL)
    }

    hd_wide <- hd_wide %>%
      dplyr::filter(!is.na(.data$tree_height))

    if (nrow(hd_wide) == 0) {
      return(NULL)
    }

    # Join with main data to get plot_name and tag
    id_lookup <- data %>%
      dplyr::select(dplyr::any_of(c("id_n", "plot_name", "tag"))) %>%
      dplyr::distinct()

    hd_wide <- hd_wide %>%
      dplyr::left_join(id_lookup, by = c("id_data_individuals" = "id_n"))

    # Rename to output format
    hd_wide <- hd_wide %>%
      dplyr::rename(
        id_n = "id_data_individuals",
        D    = "stem_diameter",
        H    = "tree_height"
      )

    if ("height_of_stem_diameter" %in% names(hd_wide)) {
      hd_wide <- hd_wide %>% dplyr::rename(POM = "height_of_stem_diameter")
    }

    keep_cols <- c("id_n", "plot_name", "tag", "D", "H",
                   "POM", "census_name", "census_date")
    result <- hd_wide %>%
      dplyr::select(dplyr::any_of(keep_cols)) %>%
      dplyr::filter(!is.na(.data$D), !is.na(.data$H))

    if (nrow(result) == 0) {
      return(NULL)
    }
    return(result)
  }

  # -----------------------------------------------------------------------
  # Path 2: fall back to main_data columns (no hd_source available)
  # -----------------------------------------------------------------------

  # Check if height data exists — handle both plain and census-suffixed column names
  has_height <- any(names(data) %in% c("tree_height", "height", "H")) ||
                any(grepl("^tree_height_census_\\d+$", names(data)))

  if (!has_height) {
    return(NULL)
  }

  # Check for diameter column
  has_dbh <- any(names(data) %in% c("stem_diameter", "dbh", "D")) ||
             any(grepl("^stem_diameter_census_\\d+$", names(data)))

  if (!has_dbh) {
    return(NULL)
  }

  # If census-suffixed columns exist (show_multiple_census = TRUE or detection-only fix)
  has_census_cols <- any(grepl("^tree_height_census_\\d+$", names(data)))

  if (show_multiple_census || has_census_cols) {
    # Select relevant columns
    hd_cols <- c("id_n", "plot_name", "tag", "quadrat", "locality_name")

    diam_cols   <- grep("stem_diameter_c",         names(data), value = TRUE)
    height_cols <- grep("tree_height_c",           names(data), value = TRUE)
    pom_cols    <- grep("height_of_stem_diameter_c", names(data), value = TRUE)
    issue_cols  <- grep("issue_agg_tree_h",        names(data), value = TRUE)

    hd_cols <- intersect(c(hd_cols, diam_cols, height_cols, pom_cols, issue_cols), names(data))

    hd_data <- data %>% dplyr::select(dplyr::any_of(hd_cols))

    # Keep only rows that have at least one non-NA height
    height_check_cols <- grep("tree_height_census", names(hd_data), value = TRUE)
    if (length(height_check_cols) > 0) {
      hd_data <- hd_data %>%
        dplyr::filter(dplyr::if_any(dplyr::any_of(height_check_cols), ~ !is.na(.)))
    }

    # Pivot to long format
    hd_long <- hd_data %>%
      tidyr::pivot_longer(
        cols         = dplyr::matches("_census_\\d+$"),
        names_to     = c(".value", "census"),
        names_pattern = "(.*)_census_(\\d+)"
      ) %>%
      dplyr::mutate(census = as.integer(.data$census))

    if ("issue_agg_tree_height" %in% names(hd_long)) {
      hd_long <- hd_long %>%
        dplyr::filter(is.na(.data$issue_agg_tree_height) | .data$issue_agg_tree_height == "") %>%
        dplyr::select(-dplyr::all_of("issue_agg_tree_height"))
    }

    hd_long <- hd_long %>% dplyr::filter(!is.na(.data$tree_height))

    select_cols <- list(
      id_n      = rlang::sym("id_n"),
      plot_name = rlang::sym("plot_name"),
      tag       = rlang::sym("tag"),
      D         = rlang::sym("stem_diameter"),
      H         = rlang::sym("tree_height")
    )

    if ("height_of_stem_diameter" %in% names(hd_long)) {
      select_cols$POM <- rlang::sym("height_of_stem_diameter")
    }

    result <- hd_long %>%
      dplyr::select(!!!select_cols) %>%
      dplyr::filter(!is.na(.data$D), !is.na(.data$H))

  } else {
    # Plain single-census columns
    dbh_col <- if ("stem_diameter" %in% names(data)) {
      "stem_diameter"
    } else if ("dbh" %in% names(data)) {
      "dbh"
    } else {
      "D"
    }

    height_col <- if ("tree_height" %in% names(data)) {
      "tree_height"
    } else if ("height" %in% names(data)) {
      "height"
    } else {
      "H"
    }

    select_cols <- list(
      id_n      = rlang::sym("id_n"),
      plot_name = rlang::sym("plot_name"),
      tag       = rlang::sym("tag"),
      D         = rlang::sym(dbh_col),
      H         = rlang::sym(height_col)
    )

    pom_col <- if ("height_of_stem_diameter" %in% names(data)) {
      "height_of_stem_diameter"
    } else if ("pom" %in% names(data)) {
      "pom"
    } else {
      NULL
    }

    if (!is.null(pom_col)) {
      select_cols$POM <- rlang::sym(pom_col)
    }

    result <- data %>%
      dplyr::select(!!!select_cols) %>%
      dplyr::filter(!is.na(.data$D), !is.na(.data$H))
  }

  if (nrow(result) == 0) {
    return(NULL)
  }

  return(result)
}


#' Print method for plot_query_list
#'
#' @param x A plot_query_list object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.plot_query_list <- function(x, ...) {

  style <- attr(x, "style")
  style_desc <- attr(x, "style_description")

  cli::cli_h1("Query Results")
  if (!is.null(style_desc)) {
    cli::cli_text("Output style: {.strong {style}} - {style_desc}")
  }
  cli::cli_text("")

  for (table_name in names(x)) {
    table <- x[[table_name]]

    if (is.data.frame(table)) {
      # Check if it's an sf object
      is_sf <- inherits(table, "sf")

      cli::cli_h2("${table_name}")

      if (is_sf) {
        geom_type <- tryCatch(class(sf::st_geometry(table))[1], error = function(e) "unknown")
        cli::cli_text("  {nrow(table)} features (sf object)")
        cli::cli_text("  Geometry type: {geom_type}")
      } else {
        cli::cli_text("  {nrow(table)} rows × {ncol(table)} columns")
      }

      # Show first few column names
      col_display <- if (ncol(table) <= 8) {
        paste(names(table), collapse = ", ")
      } else {
        paste(c(names(table)[1:8], "..."), collapse = ", ")
      }
      cli::cli_text("  Columns: {col_display}")
      cli::cli_text("")
    }
  }

  table_names <- paste0("$", names(x), collapse = ", ")
  cli::cli_text("{.emph Access tables with: {table_names}}")
  cli::cli_text("{.emph Use names(result) to see all available tables}")

  invisible(x)
}
