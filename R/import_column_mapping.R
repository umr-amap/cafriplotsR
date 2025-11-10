# Column Mapping for Plot Metadata Import
#
# Smart column mapping with fuzzy matching and domain-specific synonyms
# Handles cases like: dbh = stem_diameter, PI = principal_investigator

#' Get Column Synonym Dictionary
#'
#' Returns a comprehensive dictionary mapping common column name variations
#' to standard database column names. Includes both textual variations and
#' domain-specific semantic equivalents (e.g., dbh = stem_diameter).
#'
#' @return Named list where names are standard columns and values are character
#'   vectors of synonyms
#'
#' @keywords internal
.get_column_synonyms <- function() {
  list(
    # Plot identification
    plot_name = c(
      "plot_id", "plotid", "plot.id", "plot code", "plot_code", "plotcode",
      "site_id", "siteid", "site.id", "site_name", "sitename", "site.name",
      "plot no", "plot_no", "plotno", "plot number", "plot_number",
      "transect_id", "transect_name", "transect"
    ),

    # Survey method
    method = c(
      "survey_method", "surveymethod", "survey.method",
      "sampling_method", "samplingmethod", "sampling.method",
      "protocol", "survey_type", "plot_type", "method_type",
      "methodology", "technique"
    ),

    # Geographic: Country
    country = c(
      "pays", "pais", "country_name", "countryname", "country.name",
      "nation", "state", "country code", "country_code"
    ),

    # Geographic: Coordinates (MANY variations!)
    ddlat = c(
      "latitude", "lat", "y", "coord_y", "coordy", "coord.y",
      "lat_dd", "latdd", "lat.dd", "decimal_latitude", "declat",
      "latitude_decimal", "lat_decimal", "dd_lat", "dd.lat",
      "y_coord", "ycoord", "y.coord", "northing"
    ),

    ddlon = c(
      "longitude", "lon", "long", "lng", "x", "coord_x", "coordx", "coord.x",
      "lon_dd", "londd", "lon.dd", "long_dd", "decimal_longitude", "declon",
      "longitude_decimal", "lon_decimal", "dd_lon", "dd.lon", "dd_long",
      "x_coord", "xcoord", "x.coord", "easting"
    ),

    # Elevation
    elevation = c(
      "altitude", "elev", "alt", "elevation_m", "elevationm", "elevation.m",
      "altitude_m", "altitudem", "altitude.m",
      "height_masl", "height masl", "masl", "elevation_masl",
      "elev_m", "elevm", "elev.m", "altitude_masl"
    ),

    # Locality
    locality_name = c(
      "locality", "location", "site", "place", "place_name", "placename",
      "site_name", "sitename", "site.name", "location_name", "locationname",
      "area", "area_name", "areaname", "region", "village", "town"
    ),

    # Province
    province = c(
      "state", "region", "province_name", "provincename", "province.name",
      "admin1", "admin_1", "admin level 1", "department", "district"
    ),

    # Dates
    date_y = c(
      "year", "yyyy", "yr", "survey_year", "surveyyear", "survey.year",
      "census_year", "censusyear", "census.year", "year_survey",
      "sampling_year", "date_year", "year_of_survey"
    ),

    date_m = c(
      "month", "mm", "mon", "survey_month", "surveymonth", "survey.month",
      "census_month", "censusmonth", "census.month", "month_survey",
      "sampling_month", "date_month", "month_of_survey"
    ),

    date_d = c(
      "day", "dd", "survey_day", "surveyday", "survey.day",
      "census_day", "censusday", "census.day", "day_survey",
      "sampling_day", "date_day", "day_of_survey"
    ),

    date_begin = c(
      "survey_date", "surveydate", "survey.date", "start_date", "startdate",
      "date_start", "datestart", "date.start", "beginning_date", "beginningdate",
      "census_date", "censusdate", "census.date", "sampling_date", "samplingdate",
      "date", "date_survey", "datesurvey", "date.survey",
      "date_debut", "date_recensement"
    ),

    # People: Team leader
    team_leader = c(
      "team_lead", "teamlead", "team.lead", "leader",
      "field_leader", "fieldleader", "field.leader",
      "survey_leader", "surveyleader", "survey.leader",
      "team leader name", "team_leader_name", "lead", "chef equipe"
    ),

    # People: Principal Investigator (PI)
    principal_investigator = c(
      "PI", "pi", "P.I.", "p.i.", "lead_PI", "leadPI", "lead.PI",
      "investigator", "lead_investigator", "leadinvestigator",
      "principal investigator", "principal_investigator_name",
      "lead_scientist", "leadscientist", "lead.scientist",
      "chief_investigator", "chiefinvestigator", "chief.investigator",
      "primary_investigator", "primaryinvestigator", "primary.investigator",
      "responsable", "chercheur principal"
    ),

    # People: Data manager
    data_manager = c(
      "datamanager", "data.manager", "data manager name",
      "data_contact", "datacontact", "data.contact",
      "data_curator", "datacurator", "data.curator",
      "manager", "database_manager", "databasemanager",
      "gestionnaire", "gestionnaire donnees"
    ),

    # People: Additional people
    additional_people = c(
      "collaborators", "team_members", "teammembers", "team.members",
      "collectors", "field_team", "fieldteam", "field.team",
      "other_people", "otherpeople", "other.people",
      "team", "crew", "personnel", "staff",
      "autres personnes", "collaborateurs"
    ),

    # People: Data provider
    data_provider = c(
      "dataprovider", "data.provider", "data provider name",
      "provider", "data_source", "datasource", "data.source",
      "source", "institution", "organization", "organisation",
      "fournisseur", "fournisseur donnees"
    ),

    # Plot characteristics
    plot_area = c(
      "area", "surface", "plot_size", "plotsize", "plot.size",
      "area_ha", "areaha", "area.ha", "size", "plot_area_ha",
      "area_plot", "areaplot", "area.plot", "superficie",
      "plot area (ha)", "plot_area_hectares"
    ),

    vegetation_type = c(
      "vegetation", "veg_type", "vegtype", "veg.type",
      "vegetation_class", "vegetationclass", "vegetation.class",
      "habitat", "habitat_type", "habitattype", "habitat.type",
      "forest_class", "forestclass", "forest.class",
      "type vegetation", "type_vegetation"
    ),

    forest_type = c(
      "forest", "forest_class", "forestclass", "forest.class",
      "forest_category", "forestcategory", "forest.category",
      "stand_type", "standtype", "stand.type",
      "type foret", "type_foret", "classe foret"
    ),

    # Tree measurements (for future individual-level imports)
    # IMPORTANT: Domain-specific synonyms that aren't textually similar!
    dbh = c(
      # Direct variations
      "diameter", "diam", "d", "diameter_cm", "diam_cm",
      "dbh_cm", "dbhcm", "dbh.cm", "d_cm", "dcm",
      # Semantic equivalents (NOT textually similar!)
      "stem_diameter", "stemdiameter", "stem.diameter",
      "trunk_diameter", "trunkdiameter", "trunk.diameter",
      "diameter_breast_height", "diameter at breast height",
      "breast_height_diameter", "circumference", "circ",
      "diameter_130", "diam_130", "d_130", "d130",
      "diametre", "diametre_130", "circonference"
    ),

    tree_height = c(
      "height", "h", "ht", "tree_height_m", "treeheight",
      "total_height", "totalheight", "total.height",
      "height_m", "heightm", "height.m", "h_m", "hm",
      "hauteur", "hauteur_arbre"
    ),

    # Tag/Individual ID (for tree-level data)
    tag = c(
      "tree_id", "treeid", "tree.id", "tree_number", "treenumber",
      "tree_tag", "treetag", "tree.tag", "tag_number", "tagnumber",
      "individual_id", "individualid", "individual.id",
      "stem_id", "stemid", "stem.id", "id", "numero", "numero_arbre"
    )
  )
}


#' Get Import Column Routing Configuration
#'
#' Extends the existing get_column_routing() system with import-specific
#' configuration including synonym mappings and validation rules.
#'
#' @param table_type Character: Type of table ("plots", "individuals", etc.)
#' @param con Database connection (optional)
#'
#' @return List with routing configuration including synonyms
#'
#' @examples
#' \dontrun{
#' config <- get_import_column_routing("plots")
#' # Returns: direct_columns, subplot_features, synonyms, validation_rules
#' }
#'
#' @export
get_import_column_routing <- function(table_type = "plots", con = NULL) {

  if (is.null(con)) {
    con <- call.mydb()
  }

  # Get base routing config
  base_config <- get_column_routing(table_type, con)

  # Add import-specific configuration
  base_config$import_config <- list(

    # Column synonyms for smart mapping
    column_synonyms = .get_column_synonyms(),

    # Required columns for plots
    required_columns = c("plot_name", "method", "country"),

    # Optional but recommended columns
    recommended_columns = c("ddlat", "ddlon", "date_y", "locality_name"),

    # Validation rules
    validation_rules = list(
      plot_name = list(
        type = "character",
        unique = TRUE,
        required = TRUE,
        check_existing = TRUE,
        message = "Plot names must be unique and not already in database"
      ),

      date_y = list(
        type = "integer",
        min = 1900,
        max = lubridate::year(Sys.Date()),
        severity = "error",
        message = "Year must be between 1900 and current year"
      ),

      date_m = list(
        type = "integer",
        min = 1,
        max = 12,
        severity = "error",
        message = "Month must be between 1 and 12"
      ),

      date_d = list(
        type = "integer",
        min = 1,
        max = 31,
        severity = "error",
        message = "Day must be between 1 and 31"
      ),

      ddlat = list(
        type = "numeric",
        min = -90,
        max = 90,
        severity = "error",
        message = "Latitude must be between -90 and 90"
      ),

      ddlon = list(
        type = "numeric",
        min = -180,
        max = 180,
        severity = "error",
        message = "Longitude must be between -180 and 180"
      ),

      elevation = list(
        type = "numeric",
        min = -500,
        max = 6000,
        severity = "warning",
        message = "Elevation seems unusual (expected -500 to 6000m)"
      ),

      plot_area = list(
        type = "numeric",
        min = 0,
        max = 100,
        severity = "warning",
        message = "Plot area seems unusual (expected 0 to 100 ha)"
      )
    )
  )

  # Add subplot feature validation from database
  subplot_features <- tryCatch({
    subplot_list(con)
  }, error = function(e) {
    NULL
  })

  if (!is.null(subplot_features) && nrow(subplot_features) > 0) {
    for (i in 1:nrow(subplot_features)) {
      feature_type <- subplot_features$type[i]

      # Skip if already defined above
      if (feature_type %in% names(base_config$import_config$validation_rules)) {
        next
      }

      # Build validation rule from database
      rule <- list(
        type = subplot_features$valuetype[i],
        is_subplot_feature = TRUE
      )

      if (!is.na(subplot_features$minallowedvalue[i])) {
        rule$min <- subplot_features$minallowedvalue[i]
      }

      if (!is.na(subplot_features$maxallowedvalue[i])) {
        rule$max <- subplot_features$maxallowedvalue[i]
      }

      if (!is.na(subplot_features$expectedunit[i])) {
        rule$expectedunit <- subplot_features$expectedunit[i]
      }

      # Set severity based on type
      if (!is.null(rule$min) || !is.null(rule$max)) {
        rule$severity <- "error"
        rule$message <- sprintf(
          "%s must be between %s and %s",
          feature_type,
          rule$min %||% "min",
          rule$max %||% "max"
        )
      } else {
        rule$severity <- "warning"
      }

      base_config$import_config$validation_rules[[feature_type]] <- rule
    }
  }

  return(base_config)
}


#' Map User Columns to Database Schema
#'
#' Automatically maps user column names to database column names using:
#' 1. Exact matching
#' 2. Synonym dictionary (including domain-specific like dbh = stem_diameter)
#' 3. Fuzzy string matching
#'
#' @param user_data Data frame with user columns to map
#' @param config Import configuration from get_import_column_routing()
#' @param similarity_threshold Numeric: minimum similarity for fuzzy matching (0-1). Default: 0.6
#' @param interactive Logical: allow user to review mappings? Default: FALSE
#'
#' @return Named character vector: user_col_name = database_col_name
#'
#' @examples
#' \dontrun{
#' # Get config
#' config <- get_import_column_routing("plots")
#'
#' # Map columns
#' user_data <- read.csv("messy_data.csv")
#' mapping <- map_user_columns(user_data, config)
#'
#' # Result: c("Plot ID" = "plot_name", "Latitude" = "ddlat", ...)
#' }
#'
#' @export
map_user_columns <- function(user_data,
                             config,
                             similarity_threshold = 0.6,
                             interactive = FALSE) {

  user_cols <- colnames(user_data)

  # Get all valid database columns
  schema_cols <- c(
    config$direct_columns,
    if (!is.null(config$subplot_features)) config$subplot_features else character(0)
  )

  # Storage for mappings
  mappings <- setNames(rep(NA_character_, length(user_cols)), user_cols)
  mapping_methods <- setNames(rep(NA_character_, length(user_cols)), user_cols)
  mapping_confidence <- setNames(rep(NA_real_, length(user_cols)), user_cols)

  synonyms <- config$import_config$column_synonyms

  for (user_col in user_cols) {

    # Clean column name for matching
    user_col_clean <- tolower(trimws(user_col))

    # 1. EXACT MATCH
    if (user_col %in% schema_cols || user_col_clean %in% tolower(schema_cols)) {
      exact_match <- schema_cols[tolower(schema_cols) == user_col_clean]
      if (length(exact_match) > 0) {
        mappings[user_col] <- exact_match[1]
        mapping_methods[user_col] <- "exact"
        mapping_confidence[user_col] <- 1.0
        next
      }
    }

    # 2. SYNONYM MATCH (including domain-specific!)
    synonym_match <- .find_synonym_match(user_col_clean, synonyms)
    if (!is.null(synonym_match)) {
      mappings[user_col] <- synonym_match
      mapping_methods[user_col] <- "synonym"
      mapping_confidence[user_col] <- 1.0
      next
    }

    # 3. FUZZY MATCH
    fuzzy_result <- .fuzzy_match_column(user_col_clean, schema_cols, similarity_threshold)
    if (!is.null(fuzzy_result$match)) {
      mappings[user_col] <- fuzzy_result$match
      mapping_methods[user_col] <- "fuzzy"
      mapping_confidence[user_col] <- fuzzy_result$similarity
      next
    }

    # 4. NO MATCH
    mapping_methods[user_col] <- "none"
    mapping_confidence[user_col] <- 0
  }

  # Create mapping result with metadata
  result <- list(
    mappings = mappings,
    methods = mapping_methods,
    confidence = mapping_confidence,
    unmapped = user_cols[is.na(mappings)]
  )

  # Print summary
  cli::cli_h2("Column Mapping Results")
  cli::cli_alert_success("Exact matches: {sum(mapping_methods == 'exact', na.rm=TRUE)}")
  cli::cli_alert_success("Synonym matches: {sum(mapping_methods == 'synonym', na.rm=TRUE)}")
  cli::cli_alert_info("Fuzzy matches: {sum(mapping_methods == 'fuzzy', na.rm=TRUE)}")

  if (length(result$unmapped) > 0) {
    cli::cli_alert_warning("Unmapped columns: {length(result$unmapped)}")
    cli::cli_ul(result$unmapped)
  }

  if (interactive) {
    result <- .review_mappings_interactive(result, user_data, schema_cols, config)
  }

  return(result)
}


#' Find Synonym Match (Internal Helper)
#'
#' Searches synonym dictionary for match with robust normalization
#' Handles spaces, underscores, dots interchangeably
#'
#' @param user_col_clean Cleaned user column name (lowercase, trimmed)
#' @param synonyms Synonym dictionary
#'
#' @return Database column name or NULL
#' @keywords internal
.find_synonym_match <- function(user_col_clean, synonyms) {

  # Normalize: remove spaces, underscores, dots for matching
  normalize <- function(x) {
    gsub("[_\\. ]", "", tolower(trimws(x)))
  }

  user_col_normalized <- normalize(user_col_clean)

  for (target_col in names(synonyms)) {
    # First check if matches target column name itself
    if (user_col_normalized == normalize(target_col)) {
      return(target_col)
    }

    # Then check synonyms
    synonym_list_normalized <- sapply(synonyms[[target_col]], normalize)

    if (user_col_normalized %in% synonym_list_normalized) {
      return(target_col)
    }
  }

  return(NULL)
}


#' Fuzzy Match Column (Internal Helper)
#'
#' Uses string similarity to find best match
#'
#' @param user_col_clean Cleaned user column name
#' @param schema_cols Database column names
#' @param threshold Similarity threshold
#'
#' @return List with match and similarity, or NULL
#' @keywords internal
.fuzzy_match_column <- function(user_col_clean, schema_cols, threshold = 0.6) {

  # Calculate similarities
  similarities <- stringdist::stringsim(user_col_clean, tolower(schema_cols))

  # Find best match above threshold
  best_idx <- which.max(similarities)
  best_similarity <- similarities[best_idx]

  if (length(best_idx) > 0 && best_similarity >= threshold) {
    return(list(
      match = schema_cols[best_idx],
      similarity = best_similarity
    ))
  }

  return(NULL)
}


#' Review Mappings Interactively (Internal Helper)
#'
#' Allow user to review and adjust automatic mappings
#'
#' @param result Mapping result from map_user_columns
#' @param user_data User data
#' @param schema_cols Valid database columns
#' @param config Import configuration
#'
#' @return Updated mapping result
#' @keywords internal
.review_mappings_interactive <- function(result, user_data, schema_cols, config) {

  cli::cli_h2("Review Mappings")
  cli::cli_alert_info("Press Enter to accept, or type new mapping")

  # Review fuzzy and unmapped columns
  review_cols <- names(result$mappings)[
    result$methods %in% c("fuzzy", "none") | result$confidence < 0.8
  ]

  for (col in review_cols) {
    current_mapping <- result$mappings[col]
    method <- result$methods[col]
    confidence <- result$confidence[col]

    # Show context
    cli::cli_text("\n{.strong User column:} {.field {col}}")
    cli::cli_text("Sample values: {paste(head(user_data[[col]], 3), collapse=', ')}")

    if (!is.na(current_mapping)) {
      cli::cli_text("{.strong Suggested:} {.field {current_mapping}} ({method}, confidence: {round(confidence, 2)})")
    } else {
      cli::cli_text("{.strong Suggested:} {.emph No match found}")
    }

    # Get user input
    user_input <- readline(prompt = "Accept (Enter) or provide mapping: ")

    if (nzchar(user_input)) {
      # User provided custom mapping
      if (user_input %in% schema_cols) {
        result$mappings[col] <- user_input
        result$methods[col] <- "manual"
        result$confidence[col] <- 1.0
        cli::cli_alert_success("Mapped to: {user_input}")
      } else if (tolower(user_input) == "skip") {
        result$mappings[col] <- NA
        cli::cli_alert_info("Skipped")
      } else {
        cli::cli_alert_warning("Invalid mapping: {user_input}")
      }
    } else if (!is.na(current_mapping)) {
      cli::cli_alert_success("Accepted: {current_mapping}")
    }
  }

  # Update unmapped list
  result$unmapped <- names(result$mappings)[is.na(result$mappings)]

  return(result)
}


#' Print Mapping Summary
#'
#' Display detailed summary of column mappings
#'
#' @param mapping_result Result from map_user_columns()
#'
#' @return Invisibly returns a summary data frame
#'
#' @examples
#' \dontrun{
#' mapping <- map_user_columns(my_data, config)
#' print_mapping_summary(mapping)
#' }
#'
#' @export
print_mapping_summary <- function(mapping_result) {

  cli::cli_h1("Column Mapping Summary")

  # Create summary table
  summary_df <- tibble::tibble(
    user_column = names(mapping_result$mappings),
    database_column = as.character(mapping_result$mappings),
    method = mapping_result$methods,
    confidence = round(mapping_result$confidence, 2)
  ) %>%
    dplyr::arrange(desc(confidence), method)

  # Print by method
  for (method_type in c("exact", "synonym", "fuzzy", "manual", "none")) {
    subset <- summary_df %>% dplyr::filter(method == method_type)

    if (nrow(subset) > 0) {
      method_label <- switch(method_type,
        "exact" = "Exact Matches",
        "synonym" = "Synonym Matches",
        "fuzzy" = "Fuzzy Matches",
        "manual" = "Manual Mappings",
        "none" = "Unmapped Columns"
      )

      cli::cli_h2(method_label)

      for (i in 1:nrow(subset)) {
        if (!is.na(subset$database_column[i])) {
          cli::cli_li("{.field {subset$user_column[i]}} → {.val {subset$database_column[i]}} (confidence: {subset$confidence[i]})")
        } else {
          cli::cli_li("{.field {subset$user_column[i]}} → {.emph no mapping}")
        }
      }
    }
  }

  invisible(summary_df)
}
