#' Output style configurations for query_plots()
#'
#' @description
#' Defines how query_plots() results should be structured and which columns
#' to include based on the output style selected.
#'
#' @keywords internal
#' @noRd
.plot_output_styles <- list(

  minimal = list(
    description = "Essential plot metadata only",
    metadata_columns = c(
      "plot_name", "country", "locality_name", "method",
      "ddlat", "ddlon", "elevation",
      "census_date", "principal_investigator",
      "id_liste_plots"
    ),
    individuals_columns = c(
      "id_n", "plot_name", "tag", "tax_fam", "tax_gen", "tax_sp_level",
      "stem_diameter", "census_date"
    ),
    keep_patterns = c(),  # Optional: patterns to keep (e.g., "^feat_", "^trait_")
    remove_patterns = c("^id_(?!n|liste_plots)", "^feat_", "^trait_", "^date_modif"),
    additional_tables = c(),
    rename_columns = list(
      metadata = c("ddlat" = "latitude", "ddlon" = "longitude"),
      individuals = c("tax_fam" = "family", "tax_gen" = "genus", "tax_sp_level" = "species")
    )
  ),

  standard = list(
    description = "Standard output for general analysis",
    metadata_columns = c(
      "plot_name", "country", "locality_name", "method",
      "ddlat", "ddlon", "elevation",
      "first_census",
      "last_census",
      "n_census",
      "id_liste_plots", "data_provider", "principal_investigator", "data_manager", "team_leader"
    ),
    individuals_columns = c(
      "id_n", "plot_name", "tag", "quadrat", "subplot_name",
      "tax_fam", "tax_gen", "tax_sp_level",
      "dbh", "height", "census_date"
    ),
    keep_common_features = TRUE,  # Keep features present in >10% of plots
    keep_patterns = c(),  # Optional: patterns to keep (e.g., "^feat_", "^trait_")
    remove_patterns = c("^id_(?!n|liste_plots)", "^date_modif"),
    additional_tables = c(),
    rename_columns = list(
      metadata = c("ddlat" = "latitude", "ddlon" = "longitude"),
      individuals = c("tax_fam" = "family", "tax_gen" = "genus", "tax_sp_level" = "species")
    )
  ),

  permanent_plot = list(
    description = "Organized output for permanent plot monitoring (single or most recent census)",
    metadata_columns = c(
      "id_liste_plots", "plot_name", "country", "locality_name", "method",
      "ddlat", "ddlon", "elevation", "data_provider", "plot_area",
      "forest_description",
      "first_census",
      "last_census",
      "n_census", 
      "data_provider", 
      "principal_investigator", 
      "data_manager", 
      "team_leader"
    ),
    individuals_columns = c(
      "id_n", "plot_name", "tag", "quadrat",
      "tax_fam", "tax_gen", "tax_sp_level",
      "stem_diameter", "tree_height", "census_date",
      "number_of_stem",
      "traitvalue",
      "traitvalue_char",
      "trait",
      "traitdescription",
      "valuetype",
      "census_date"
    ),
    keep_patterns = c("wood_density",
                      "stem_diameter",
                      "observations",
                      "light",
                      "position_",
                      "phenology",
                      "succession_guild",
                      "census_date",
                      "stem_status",
                      "mortality_risk_flag"),  # Optional: patterns to keep (e.g., "^feat_", "^trait_")
    remove_patterns = c("^id_(?!n|liste_plots)", "^date_modif", "_census_\\d+$"),  # Remove census suffix columns
    additional_tables = c("censuses", "height_diameter"),
    keep_all_features = FALSE,  # Features go to census table
    rename_columns = list(
      metadata = c("ddlat" = "latitude",
                   "ddlon" = "longitude"),
      individuals = c("tax_fam" = "family",
                      "tax_gen" = "genus",
                      "tax_sp_level" = "species",
                      "stem_diameter" = "dbh",
                      "tree_height" = "height",
                      "height_of_stem_diameter" = "pom")
    )
  ),

  permanent_plot_multi_census = list(
    description = "Organized output for permanent plots with multiple censuses shown (show_multiple_census = TRUE)",
    metadata_columns = c(
      "id_liste_plots", "plot_name", "country", "locality_name", "method",
      "ddlat", "ddlon", "elevation",
      "id_liste_plots",
      "first_census",
      "last_census",
      "n_census", "data_provider", "principal_investigator", "data_manager", "team_leader",
      "census_date"
    ),
    individuals_columns = c(
      "id_n", "plot_name", "tag", "quadrat",
      "tax_fam", "tax_gen", "tax_sp_level",
      "number_of_stem",
      "traitvalue",
      "traitvalue_char",
      "trait",
      "traitdescription",
      "valuetype",
      "time"
      # Census-specific columns added dynamically (stem_diameter_census_1, etc.)
    ),
    keep_census_columns = TRUE,  # Keep all _census_N columns
    keep_patterns = c("wood_density",
                      "stem_diameter",
                      "observations",
                      "light",
                      "position_",
                      "census_date",
                      "stem_status",
                      "mortality_risk_flag"),  # Optional: patterns to keep (e.g., "^feat_", "^trait_")
    remove_patterns = c("^id_(?!n|liste_plots)", "^date_modif"),
    additional_tables = c("censuses", "height_diameter"),
    keep_all_features = FALSE,
    rename_columns = list(
      metadata = c("ddlat" = "latitude", "ddlon" = "longitude"),
      individuals = c("tax_fam" = "family", "tax_gen" = "genus", "tax_sp_level" = "species")
      # Census columns renamed dynamically: stem_diameter_census_1 -> dbh_census_1, etc.
    ),
    census_column_renames = c(
      "stem_diameter" = "dbh",
      "tree_height" = "height"
    )
  ),

  transect = list(
    description = "Simplified output for transect/walk surveys",
    metadata_columns = c(
      "plot_name", "country", "locality_name", "method",
      "ddlat", "ddlon", "elevation",
      "transect_length", "transect_width",
      "census_date",
      "id_liste_plots"
    ),
    individuals_columns = c(
      "id_n", "plot_name", "tag", "distance_along_transect",
      "tax_fam", "tax_gen", "tax_sp_level", "dbh",
      "colnbr", "colnam_specimen"
    ),
    keep_patterns = c("position_",
                      "strate",
                      "transect_part"),  # Optional: patterns to keep (e.g., "^feat_", "^trait_")
    remove_patterns = c("^id_(?!n|liste_plots)", "height", "pom", "growth", "mortality", "^date_modif"),
    additional_tables = c(),
    rename_columns = list(
      metadata = c("ddlat" = "latitude", "ddlon" = "longitude"),
      individuals = c("tax_fam" = "family", "tax_gen" = "genus", "tax_sp_level" = "species")
    )
  ),

  full = list(
    description = "Complete export with all columns",
    metadata_columns = "all",
    individuals_columns = "all",
    keep_patterns = c(),  # No patterns applied in full mode
    remove_patterns = c(),
    additional_tables = c()
  ),

  census_pairs = list(
    description = "One row per consecutive census pair per individual",
    metadata_columns = c(
      "plot_name", "country", "locality_name", "method",
      "ddlat", "ddlon", "elevation",
      "first_census", "last_census", "n_census",
      "id_liste_plots", "data_provider", "principal_investigator"
    ),
    individuals_columns = c(
      "id_n", "plot_name", "tag", "quadrat",
      "tax_fam", "tax_gen", "tax_sp_level",
      "number_of_stem",
      "valuetype",
      "time"
      # Census-specific columns added dynamically (stem_diameter_census_1, etc.)
    ),
    keep_patterns = c("wood_density",
                      "stem_diameter",
                      "observations",
                      "light",
                      "phenology",
                      "succession_guild",
                      "census_date",
                      "stem_status",
                      "mortality_risk_flag",
                      "date_"),
    remove_patterns = c("^id_(?!n|liste_plots)", "^date_modif"),
    additional_tables = c(),
    rename_columns = list(
      metadata = c("ddlat" = "latitude", "ddlon" = "longitude"),
      individuals = c(
        "tax_fam" = "family", "tax_gen" = "genus", "tax_sp_level" = "species",
        "stem_diameter_0" = "dbh_0", "stem_diameter_1" = "dbh_1",
        "tree_height_0" = "height_0", "tree_height_1" = "height_1",
        "date_census0" = "date_census_0",
        "date_census1" = "date_census_1"
      )
    )
  )
)

#' Method to style mapping
#'
#' Maps plot method names to appropriate output styles
#'
#' @keywords internal
#' @noRd
.method_to_style_map <- c(
  "Savana plot 40x40m" = "permanent_plot",
  "1 ha plot" = "permanent_plot",
  "1ha-IRD" = "permanent_plot",
  "Long Transect" = "transect",
  "line transect" = "transect",
  "Transect MBG style Large" = "transect",
  "0.4-ha-IRD" = "transect"
)

#' Detect output style from method field
#'
#' @param data Data frame with method column
#' @return Character string with detected style
#'
#' @keywords internal
#' @noRd
.detect_style_from_method <- function(data) {

  if (!"method" %in% names(data)) {
    return("standard")
  }

  # Get unique methods from data
  methods <- unique(data$method)
  methods <- methods[!is.na(methods)]

  if (length(methods) == 0) {
    return("standard")
  }
  
  
  if (length(methods) > 1) {
    return("standard")
  }

  # Find matching style
  for (method in methods) {
    if (method %in% names(.method_to_style_map)) {
      detected <- .method_to_style_map[[method]]
      return(detected)
    }
  }

  # Default if no match
  return("standard")
}
