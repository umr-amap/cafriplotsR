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

  # Get style configuration
  style_config <- .plot_output_styles[[style]]

  if (is.null(style_config)) {
    cli::cli_alert_warning("Unknown style '{style}', using 'standard'")
    style <- "standard"
    style_config <- .plot_output_styles[[style]]
  }

  # Handle data structure (could be data frame or list from current query_plots)
  if (is.list(data) && !is.data.frame(data)) {
    # Already a list (has $extract, $census_features, etc.)
    main_data <- data$extract
    census_features <- data$census_features
    coordinates <- data$coordinates
    coordinates_sf <- data$coordinates_sf
  } else {
    # Simple data frame
    main_data <- data
    census_features <- NULL
    coordinates <- NULL
    coordinates_sf <- NULL
  }

  # Initialize result list
  result <- list()

  # 1. Extract metadata table
  result$metadata <- .extract_metadata_table(main_data, style_config, extract_individuals)

  # 2. Extract individuals table (if individuals were extracted)
  if (extract_individuals) {
    result$individuals <- .extract_individuals_table(main_data, style_config, show_multiple_census)
  }

  # 3. Extract additional tables based on style
  if ("censuses" %in% style_config$additional_tables && !is.null(census_features)) {
    result$censuses <- .extract_census_table(census_features, main_data)
  }

  if ("height_diameter" %in% style_config$additional_tables && extract_individuals) {
    hd_pairs <- .extract_height_diameter_pairs(main_data, show_multiple_census)
    if (!is.null(hd_pairs) && nrow(hd_pairs) > 0) {
      result$height_diameter <- hd_pairs
    }
  }

  # 4. Add coordinates if available (from show_all_coordinates = TRUE)
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
#' @keywords internal
#' @noRd
.extract_metadata_table <- function(data, style_config, extract_individuals) {

  # Get columns to keep
  meta_cols <- style_config$metadata_columns

  # Handle "all" case
  if (length(meta_cols) == 1 && meta_cols == "all") {
    if (extract_individuals) {
      # Keep only plot-level columns (no individual measurements)
      # Get unique by plot_name
      meta_data <- data %>%
        dplyr::select(-matches("^(dbh|height|pom|tag|id_n)")) %>%
        dplyr::distinct(plot_name, .keep_all = TRUE)
    } else {
      meta_data <- data
    }
    return(meta_data)
  }

  # Get available columns
  available_cols <- names(data)

  # Keep specified columns that exist
  keep_cols <- intersect(meta_cols, available_cols)

  # Handle common features
  if (!is.null(style_config$keep_common_features) && style_config$keep_common_features) {
    feat_cols <- grep("^feat_", available_cols, value = TRUE)
    # Keep features present in >10% of rows (not all NA)
    if (length(feat_cols) > 0) {
      common_feats <- feat_cols[sapply(data[feat_cols], function(x) mean(!is.na(x)) > 0.1)]
      keep_cols <- c(keep_cols, common_feats)
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
  meta_data <- data %>%
    dplyr::select(all_of(keep_cols)) %>%
    dplyr::distinct(plot_name, .keep_all = TRUE)

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

  # If multiple censuses, keep census-related columns
  if (show_multiple_census) {
    census_cols <- grep("census", available_cols, value = TRUE, ignore.case = TRUE)
    keep_cols <- unique(c(keep_cols, census_cols))
  }

  # Remove patterns
  if (!is.null(style_config$remove_patterns) && length(style_config$remove_patterns) > 0) {
    for (pattern in style_config$remove_patterns) {
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
    dplyr::select(all_of(keep_cols))

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
    dplyr::filter(plot_name %in% plots) %>%
    dplyr::select(plot_name, typevalue, year, month) %>%
    dplyr::distinct()

  # Create census_date
  if ("year" %in% names(census_info) && "month" %in% names(census_info)) {
    census_info <- census_info %>%
      dplyr::mutate(
        census_date = as.Date(paste(year, month, "01", sep = "-")),
        census_date = format(census_date, "%Y-%m")
      ) %>%
      dplyr::select(-year, -month)
  }

  # Rename typevalue to census_number
  if ("typevalue" %in% names(census_info)) {
    census_info <- census_info %>%
      dplyr::rename(census_number = typevalue)
  }

  # Add people involved (team_leader, additional_people, principal_investigator)
  people_cols <- c("team_leader", "additional_people", "principal_investigator")

  for (col in people_cols) {
    if (col %in% names(census_features)) {
      people_data <- census_features %>%
        dplyr::filter(plot_name %in% plots) %>%
        dplyr::select(plot_name, typevalue, !!sym(col)) %>%
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
    dplyr::arrange(plot_name, census_number)

  return(census_info)
}


#' Extract height-diameter pairs from individual data
#'
#' @description
#' Extracts all stem diameter and tree height pairs, handling multiple censuses
#' by pivoting census columns to long format. Based on user's example code.
#'
#' @param data Main query results with individual measurements
#' @param show_multiple_census Logical: were multiple censuses shown?
#'
#' @return Data frame with id_n, plot_name, tag, D (dbh), H (height), POM
#'
#' @keywords internal
#' @noRd
.extract_height_diameter_pairs <- function(data, show_multiple_census) {

  # Check if height data exists
  height_cols <- grep("tree_height", names(data), value = TRUE, ignore.case = TRUE)

  if (length(height_cols) == 0) {
    return(NULL)
  }

  # If multiple censuses, need to pivot
  if (show_multiple_census) {
    # Select relevant columns
    hd_cols <- c("id_n", "plot_name", "tag", "quadrat", "locality_name")

    # Add stem_diameter columns
    diam_cols <- grep("stem_diameter_c", names(data), value = TRUE)
    hd_cols <- c(hd_cols, diam_cols)

    # Add tree_height columns
    height_cols <- grep("tree_height_c", names(data), value = TRUE)
    hd_cols <- c(hd_cols, height_cols)

    # Add height_of_stem_diameter (POM) columns
    pom_cols <- grep("height_of_stem_diameter_c", names(data), value = TRUE)
    hd_cols <- c(hd_cols, pom_cols)

    # Add issue columns for filtering
    issue_cols <- grep("issue_agg_tree_h", names(data), value = TRUE)
    hd_cols <- c(hd_cols, issue_cols)

    # Keep only available columns
    hd_cols <- intersect(hd_cols, names(data))

    hd_data <- data %>%
      dplyr::select(all_of(hd_cols))

    # Remove rows without any height data
    height_check_cols <- grep("tree_height_census", names(hd_data), value = TRUE)
    if (length(height_check_cols) > 0) {
      hd_data <- hd_data %>%
        dplyr::filter(if_any(all_of(height_check_cols), ~ !is.na(.)))
    }

    # Pivot longer for census columns
    hd_long <- hd_data %>%
      tidyr::pivot_longer(
        cols = matches("_census_\\d+$"),
        names_to = c(".value", "census"),
        names_pattern = "(.*)_census_(\\d+)"
      ) %>%
      dplyr::mutate(census = as.integer(census))

    # Filter out rows with issues or missing height
    if ("issue_agg_tree_height" %in% names(hd_long)) {
      hd_long <- hd_long %>%
        dplyr::filter(is.na(issue_agg_tree_height) | issue_agg_tree_height == "") %>%
        dplyr::select(-issue_agg_tree_height)
    }

    hd_long <- hd_long %>%
      dplyr::filter(!is.na(tree_height))

    # Rename columns to final format
    result <- hd_long %>%
      dplyr::select(
        id_n, plot_name, tag,
        D = stem_diameter,
        H = tree_height,
        POM = height_of_stem_diameter
      ) %>%
      dplyr::filter(!is.na(D), !is.na(H))

  } else {
    # Single census - simpler extraction
    result <- data %>%
      dplyr::select(
        id_n, plot_name, tag,
        D = dbh,
        H = height,
        POM = pom
      ) %>%
      dplyr::filter(!is.na(D), !is.na(H))
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

  cli::cli_text("{.emph Access tables with: $metadata, $individuals, etc.}")
  cli::cli_text("{.emph Use names(result) to see all available tables}")

  invisible(x)
}
