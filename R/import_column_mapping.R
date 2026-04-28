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
      "site_id", "siteid", "site.id", 
      "plot no", "plot_no", "plotno", "plot number", "plot_number",
      "transect_id", "transect_name", "transect", "plot", "transect_num",
      "transect num", "plot name", "nom du plot", "numéro du plot",
      "transect_plot_name"
    ),

    # Survey method
    method = c(
      "survey_method", "surveymethod", "survey.method",
      "sampling_method", "samplingmethod", "sampling.method",
      "protocol", "survey_type", "plot_type", "method_type",
      "methodology", "technique", "sampling", "méthode","protocole"
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
      "area", "area_name", "areaname", "region", "village", "town", "localité"
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
      "sampling_year", "date_year", "year_of_survey", "année"
    ),

    date_m = c(
      "month", "mm", "mon", "survey_month", "surveymonth", "survey.month",
      "census_month", "censusmonth", "census.month", "month_survey",
      "sampling_month", "date_month", "month_of_survey", "mois"
    ),

    data_d = c(
      "day", "dd", "survey_day", "surveyday", "survey.day",
      "census_day", "censusday", "census.day", "day_survey",
      "sampling_day", "date_day", "day_of_survey", "jour", "colday",
      "jour"
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

    data_provider = c(
      "dataprovider", "data.provider", "data provider name",
      "provider", "data_source", "datasource", "data.source",
      "source", "institution", "organization", "organisation",
      "fournisseur", "fournisseur donnees"
    ),


    # IMPORTANT: Domain-specific synonyms that aren't textually similar!
    stem_diameter = c(
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
    
    height_of_stem_diameter = c(
      # Direct variations
      "point of measurement", "POM", "Height of DBH",
      "DBH height", "hauteur de mesure"
    ),
    
    position_x = c(
      "X"
    ),
    
    position_y = c(
      "Y"
    ),
    
    observations = c(
      "comment",
      "commentaires",
      "commentaire",
      "commmentaire"
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
      "stem_id", "stemid", "stem.id", "id", "numero", "numero_arbre",
      "ind_num_sous_plot", "Etiquette"
    ),

    # Taxonomy columns (for individuals import)
    idtax_n = c(
      "idtax", "id_tax", "taxonomy_id", "taxonomyid", "taxonomy.id",
      "taxon_id", "taxonid", "taxon.id", "id_taxon",
      "tax_id", "taxid", "tax.id", "species_id", "speciesid", "species.id",
      "taxon code", "taxon_code", "taxoncode"
    ),

    original_tax_name = c(
      "original_name", "originalname", "original.name",
      "scientific_name", "scientificname", "scientific.name",
      "species_name", "speciesname", "species.name", "species",
      "taxon_name", "taxonname", "taxon.name", "taxon",
      "name", "nom_scientifique", "nom scientifique", "espece",
      "binomial", "latin_name", "latinname", "latin.name",
      "full_name", "fullname", "full.name", "nom_original",
      "taxonomy", "tax_name", "taxname", "original_taxon"
    ),

    # Herbarium specimens (optional, for individuals)
    herbarium_nbe_type = c(
      "herbarium_type", "herbariumtype", "herbarium.type",
      "specimen_type", "specimentype", "specimen.type",
      "voucher_type", "vouchertype", "voucher.type",
      "type", "specimen type", "voucher type", "herbarium type",
      "type_specimen", "type specimen"
    ),

    herbarium_nbe_char = c(
      "herbarium_number", "herbariumnumber", "herbarium.number",
      "herbarium_code", "herbariumcode", "herbarium.code",
      "specimen_number", "specimennumber", "specimen.number",
      "specimen_code", "specimencode", "specimen.code",
      "voucher_number", "vouchernumber", "voucher.number",
      "voucher_code", "vouchercode", "voucher.code",
      "herbarium_id", "herbariumid", "herbarium.id",
      "specimen_id", "specimenid", "specimen.id",
      "accession", "accession_number", "accessionnumber",
      "barcode", "herbarium barcode", "numero herbier",
      "code herbier", "numero specimen"
    ),

    # Multi-stem identifier (optional, for individuals)
    multi_tiges_id = c(
      "multi_stem", "multistem", "multi.stem",
      "stem_id", "stemid", "stem.id",
      "multistem_id", "multistemid", "multistem.id",
      "stem_code", "stemcode", "stem.code",
      "stem letter", "stem_letter", "stemletter",
      "multi tige", "multi_tige", "tige",
      "stem", "stem identifier", "stem_identifier",
      "stem_grouping", "stemgrouping", "stem.grouping"
    )
  )
}


#' Get Column Descriptions
#'
#' Returns descriptions for database columns to help users understand what data is expected.
#' Includes both flat table columns (hard-coded) and feature columns (from database).
#'
#' @param con Database connection
#' @param table_type Character: "plots" or "individuals"
#'
#' @return Named list of column descriptions (and additional info for traits)
#' @keywords internal
.get_column_descriptions <- function(con, table_type = "plots") {

  # Warning suffix for plot metadata when importing individuals
  plot_metadata_warning <- if (table_type == "individuals") {
    " ⚠️ For individuals import: Skip this column - import via Plot Metadata instead."
  } else {
    ""
  }

  # Hard-coded descriptions for flat table columns (with category for grouped dropdowns)
  flat_descriptions <- list(
    # Plot identification
    plot_name = list(
      description = "Unique identifier for the plot (required). Must be unique across the database.",
      category = "Identification"
    ),

    # Geographic
    country = list(
      description = paste0("Country where the plot is located.", plot_metadata_warning),
      category = "Location"
    ),
    province = list(
      description = paste0("Province, state, or administrative region within the country.", plot_metadata_warning),
      category = "Location"
    ),
    locality_name = list(
      description = paste0("Name of the locality, village, or specific location.", plot_metadata_warning),
      category = "Location"
    ),
    ddlat = list(
      description = paste0("Latitude in decimal degrees. Range: -90 to 90.", plot_metadata_warning),
      category = "Location"
    ),
    ddlon = list(
      description = paste0("Longitude in decimal degrees. Range: -180 to 180.", plot_metadata_warning),
      category = "Location"
    ),
    elevation = list(
      description = paste0("Elevation above sea level in meters. Typical range: -500 to 6000m.", plot_metadata_warning),
      category = "Location"
    ),

    # Plot characteristics
    method = list(
      description = paste0("Survey method or protocol used. E.g., '1ha-IRD', 'transect', etc.", plot_metadata_warning),
      category = "Sampling"
    ),

    # Dates
    date_y = list(
      description = paste0("Year of survey/census. Format: YYYY (e.g., 2023).", plot_metadata_warning),
      category = "Dates"
    ),
    date_m = list(
      description = paste0("Month of survey/census. Range: 1-12.", plot_metadata_warning),
      category = "Dates"
    ),
    data_d = list(
      description = paste0("Day of survey/census. Range: 1-31.", plot_metadata_warning),
      category = "Dates"
    ),

    # Individual tree columns
    idtax_n = list(
      description = "Taxonomic ID from the taxonomic database (required for individuals). Use taxonomic matching app to get this ID.",
      category = "Identification"
    ),
    original_tax_name = list(
      description = "Original taxonomic name before standardization (required for individuals). Keeps traceability of the original field identification.",
      category = "Identification"
    ),
    tag = list(
      description = "Tree tag or identifier within the plot (recommended). If not provided, will be auto-generated as sequential integers (1, 2, 3, ...) per plot.",
      category = "Identification"
    ),

    # Herbarium specimens (optional for individuals)
    herbarium_nbe_type = list(
      description = "Type or source of herbarium specimen (optional). E.g., 'IRD plot 4181', institution name.",
      category = "Specimens"
    ),
    herbarium_nbe_char = list(
      description = "Herbarium specimen reference number or barcode (optional). E.g., 'Lejoly 485', accession number.",
      category = "Specimens"
    ),

    # Multi-stem trees (optional for individuals)
    multi_tiges_id = list(
      description = "Multi-stem identifier for trees with multiple stems (optional). Links secondary stems to the main individual by referencing the main stem's tag.",
      category = "Identification"
    ),

    # --- Additional columns for query output documentation ---

    # Individual identification (query output)
    id_n = list(
      description = "Unique database identifier for the individual tree.",
      category = "Identification"
    ),
    subplot_name = list(
      description = "Name of the subplot within the plot.",
      category = "Identification"
    ),
    number_of_stem = list(
      description = "Number of stems for the individual tree.",
      category = "Measurement"
    ),

    # Measurement columns (raw database names)
    stem_diameter = list(
      description = "Stem diameter at breast height in cm.",
      category = "Measurement"
    ),
    tree_height = list(
      description = "Total tree height in meters.",
      category = "Measurement"
    ),
    height_of_stem_diameter = list(
      description = "Height at which stem diameter was measured (point of measurement) in meters.",
      category = "Measurement"
    ),

    # Renamed measurement columns (output style aliases)
    dbh = list(
      description = "Stem diameter at breast height in cm (renamed from stem_diameter).",
      category = "Measurement"
    ),
    height = list(
      description = "Total tree height in meters (renamed from tree_height).",
      category = "Measurement"
    ),
    pom = list(
      description = "Point of measurement: height at which diameter was measured (renamed from height_of_stem_diameter).",
      category = "Measurement"
    ),

    # Renamed location columns (output style aliases)
    latitude = list(
      description = "Latitude in decimal degrees (renamed from ddlat).",
      category = "Location"
    ),
    longitude = list(
      description = "Longitude in decimal degrees (renamed from ddlon).",
      category = "Location"
    ),

    # Renamed taxonomy columns (output style aliases)
    family = list(
      description = "Taxonomic family (renamed from tax_fam).",
      category = "Taxonomy"
    ),
    genus = list(
      description = "Taxonomic genus (renamed from tax_gen).",
      category = "Taxonomy"
    ),
    species = list(
      description = "Species epithet (renamed from tax_sp_level).",
      category = "Taxonomy"
    ),

    # Taxonomy columns (raw database names)
    tax_fam = list(
      description = "Taxonomic family of the tree.",
      category = "Taxonomy"
    ),
    tax_gen = list(
      description = "Taxonomic genus of the tree.",
      category = "Taxonomy"
    ),
    tax_sp_level = list(
      description = "Species epithet of the tree.",
      category = "Taxonomy"
    ),

    # Plot-level identifiers (output)
    plot_id = list(
      description = "Unique database identifier for the plot (renamed from id_liste_plots).",
      category = "Identification"
    ),
    id_liste_plots = list(
      description = "Unique database identifier for the plot.",
      category = "Identification"
    ),

    # Census summary columns
    census_date = list(
      description = "Date of the census measurement.",
      category = "Dates"
    ),
    first_census = list(
      description = "Date of the earliest census for this plot.",
      category = "Dates"
    ),
    last_census = list(
      description = "Date of the most recent census for this plot.",
      category = "Dates"
    ),
    n_census = list(
      description = "Total number of censuses conducted for this plot.",
      category = "Dates"
    ),
    time = list(
      description = "Time interval between censuses in years.",
      category = "Dates"
    ),

    # Plot features (commonly appear in metadata)
    plot_area = list(
      description = "Total area of the plot (typically in hectares).",
      category = "Sampling"
    ),
    forest_description = list(
      description = "Description of the forest type or vegetation.",
      category = "Sampling"
    ),
    distance_along_transect = list(
      description = "Distance of the individual along the transect (meters).",
      category = "Measurement"
    ),
    transect_length = list(
      description = "Total length of the transect (meters).",
      category = "Sampling"
    ),
    transect_width = list(
      description = "Width of the transect (meters).",
      category = "Sampling"
    )
  )

  # Fetch feature descriptions from database
  feature_info <- list()

  if (table_type == "plots") {
    # Get subplot feature descriptions
    tryCatch({
      subplot_desc <- DBI::dbGetQuery(con, "
        SELECT type, typedescription, category
        FROM subplotype_list
        WHERE typedescription IS NOT NULL AND typedescription != ''
      ")

      if (nrow(subplot_desc) > 0) {
        for (i in 1:nrow(subplot_desc)) {
          feature_info[[subplot_desc$type[i]]] <- list(
            description = subplot_desc$typedescription[i],
            category = if (!is.na(subplot_desc$category[i])) subplot_desc$category[i] else "Other"
          )
        }
      }
    }, error = function(e) {
      message("Note: Could not fetch subplot feature descriptions (", e$message, ").")
    })
  } else if (table_type == "individuals") {
    # Get individual feature/trait descriptions with additional info
    tryCatch({
      trait_info <- DBI::dbGetQuery(con, "
        SELECT trait, traitdescription, factorlevels, expectedunit, category
        FROM traitlist
        WHERE traitdescription IS NOT NULL AND traitdescription != ''
      ")

      if (nrow(trait_info) > 0) {
        for (i in 1:nrow(trait_info)) {
          info <- list(
            description = trait_info$traitdescription[i],
            category = if (!is.na(trait_info$category[i])) trait_info$category[i] else "Other"
          )

          # Add factor levels if available
          if (!is.na(trait_info$factorlevels[i]) && trait_info$factorlevels[i] != "") {
            info$factorlevels <- trait_info$factorlevels[i]
          }

          # Add expected unit if available
          if (!is.na(trait_info$expectedunit[i]) && trait_info$expectedunit[i] != "") {
            info$expectedunit <- trait_info$expectedunit[i]
          }

          feature_info[[trait_info$trait[i]]] <- info
        }
      }
    }, error = function(e) {
      message("Note: Could not fetch trait descriptions (", e$message, ").")
    })
  }

  # Combine flat and feature info (both are already list-of-lists with description + category)
  c(flat_descriptions, feature_info)
}


#' Build Grouped Choices for Schema Column Dropdowns
#'
#' Builds a named list of named character vectors suitable for \code{selectInput}
#' or \code{selectizeInput} with optgroups. Columns are grouped by their category
#' from the database or hardcoded config.
#'
#' @param column_descriptions Named list of column descriptions (from \code{.get_column_descriptions()}).
#'   Each element should be a list with at least \code{description} and optionally \code{category}.
#' @param schema_columns Character vector of all valid schema column names.
#' @param user_col Optional: name of the user column being mapped, used to sort
#'   choices within each group by string similarity (most relevant first).
#'
#' @return A named list where names are category labels and values are named
#'   character vectors of column choices (suitable for selectInput optgroups).
#'
#' @keywords internal
get_schema_choices_grouped <- function(column_descriptions, schema_columns,
                                       user_col = NULL) {

  # Build a data frame of column -> category
  col_categories <- data.frame(
    col = schema_columns,
    category = vapply(schema_columns, function(col) {
      info <- column_descriptions[[col]]
      if (!is.null(info) && !is.null(info$category)) {
        info$category
      } else {
        "Other"
      }
    }, character(1)),
    stringsAsFactors = FALSE
  )

  # If user_col provided, compute similarity for sorting within groups
  if (!is.null(user_col)) {
    user_col_clean <- tolower(trimws(user_col))
    col_categories$sim <- stringdist::stringsim(user_col_clean, tolower(col_categories$col))
  } else {
    col_categories$sim <- 0
  }

  # Build label for each column: "col_name - description (unit)"
  col_categories$label <- vapply(col_categories$col, function(col) {
    info <- column_descriptions[[col]]
    if (!is.null(info) && !is.null(info$description)) {
      desc <- info$description
      # Truncate long descriptions
      if (nchar(desc) > 60) desc <- paste0(substr(desc, 1, 57), "...")
      # Add unit if available
      if (!is.null(info$expectedunit) && !is.na(info$expectedunit) && info$expectedunit != "") {
        paste0(col, " - ", desc, " [", info$expectedunit, "]")
      } else {
        paste0(col, " - ", desc)
      }
    } else {
      col
    }
  }, character(1))


  # Define a preferred category order
  # Includes both flat column categories (Identification, Location, Dates, Specimens)
  # and database feature categories from subplotype_list and traitlist
  category_order <- c(
    # Flat column categories (plots/individuals direct columns)
    "Identification", "Location", "Dates", "Specimens",
    # Shared categories
    "Sampling", "Sampling identification", "People", "Position", "Observation",
    # Subplot feature categories
    "Environment", "Soil",
    # Trait categories
    "Stem-level trait", "Stem status", "Leaf trait", "Wood trait",
    "Phenology", "Classification", "Vitality",
    # Catch-all
    "Other trait", "Other"
  )

  # Sort categories: known ones first in order, unknown ones at the end alphabetically
  unique_cats <- unique(col_categories$category)
  known <- intersect(category_order, unique_cats)
  unknown <- setdiff(unique_cats, category_order)
  ordered_cats <- c(known, sort(unknown))

  # Build the grouped list
  grouped_choices <- list()

  for (cat in ordered_cats) {
    cat_rows <- col_categories[col_categories$category == cat, , drop = FALSE]
    # Sort within group: by similarity (desc), then alphabetically
    cat_rows <- cat_rows[order(-cat_rows$sim, cat_rows$col), , drop = FALSE]

    choices <- stats::setNames(cat_rows$col, cat_rows$label)
    grouped_choices[[cat]] <- choices
  }

  grouped_choices
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

  tryCatch({
    if (is.null(con)) {
      con <- call.mydb()
    }

    # Get base routing config
    base_config <- get_column_routing(table_type, con)

    # Set required and recommended columns based on table type
    if (table_type == "individuals") {
      required_cols <- c("plot_name", "idtax_n", "original_tax_name")
      recommended_cols <- c("tag")
    } else {
      # Default: plots
      required_cols <- c("plot_name", "method", "country")
      recommended_cols <- c("ddlat", "ddlon", "date_y", "locality_name", "data_d", "date_m", "date_y")
    }

    # Add import-specific configuration

    # Get column synonyms (merged by import type for better matching)
    col_synonyms <- if (table_type == "individuals") {
      # For individuals: merge all synonyms (direct columns + traits + features)
      # Use modifyList to properly merge with later lists overwriting earlier ones
      # This ensures trait_column_synonyms (with "dbh") overwrites column_synonyms (without "dbh")
      col_syn <- .get_column_synonyms()
      col_syn <- modifyList(col_syn, .get_individual_column_synonyms())
      col_syn <- modifyList(col_syn, .get_trait_column_synonyms())
      col_syn <- modifyList(col_syn, .get_individual_feature_synonyms())
      col_syn
    } else {
      # For plots: merge base columns with subplot feature synonyms
      col_syn <- .get_column_synonyms()
      col_syn <- modifyList(col_syn, .get_subplot_feature_synonyms())
      col_syn
    }

    # Get column descriptions
    col_descriptions <- .get_column_descriptions(con, table_type)

    base_config$import_config <- list(

      # Column synonyms for smart mapping
      column_synonyms = col_synonyms,

      # Column descriptions for user guidance
      column_descriptions = col_descriptions,

    # Required columns
    required_columns = required_cols,

    # Optional but recommended columns
    recommended_columns = recommended_cols,

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
        message = "Latitude must be between -90 and 90",
        utm_hint = TRUE  # Flag to check for UTM coordinates
      ),

      ddlon = list(
        type = "numeric",
        min = -180,
        max = 180,
        severity = "error",
        message = "Longitude must be between -180 and 180",
        utm_hint = TRUE  # Flag to check for UTM coordinates
      ),

      elevation = list(
        type = "numeric",
        min = -500,
        max = 6000,
        severity = "warning",
        message = "Elevation seems unusual (expected -500 to 6000m)"
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

  }, error = function(e) {
    message("ERROR in get_import_column_routing:")
    message("  Table type: ", table_type)
    message("  Error message: ", e$message)
    message("  Error call: ", deparse(e$call))

    # Re-throw the error so it can be caught by the Shiny app
    stop(e)
  })
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
    if (!is.null(config$subplot_features)) config$subplot_features else character(0),
    if (!is.null(config$feature_columns)) config$feature_columns else character(0)
  )

  # Storage for mappings
  mappings <- setNames(rep(NA_character_, length(user_cols)), user_cols)
  mapping_methods <- setNames(rep(NA_character_, length(user_cols)), user_cols)
  mapping_confidence <- setNames(rep(NA_real_, length(user_cols)), user_cols)

  synonyms <- config$import_config$column_synonyms

  # Separate direct and feature columns for scoring
  direct_cols <- config$direct_columns
  feature_cols <- c(
    if (!is.null(config$subplot_features)) config$subplot_features else character(0),
    if (!is.null(config$feature_columns)) config$feature_columns else character(0)
  )
  required_cols <- config$import_config$required_columns

  # Storage for mapping alternatives (for potential future UI enhancements)
  mapping_alternatives <- setNames(vector("list", length(user_cols)), user_cols)

  for (user_col in user_cols) {

    # Clean column name for matching
    user_col_clean <- tolower(trimws(user_col))

    # Score all candidates using category-aware scoring
    candidates <- .score_candidates(
      user_col_clean, direct_cols, feature_cols,
      synonyms, required_cols, similarity_threshold
    )

    mapping_alternatives[[user_col]] <- candidates

    # Select best match if any candidates found
    if (!is.null(candidates) && length(candidates) > 0) {
      best <- candidates[[1]]
      mappings[user_col] <- best$match
      mapping_methods[user_col] <- best$method
      mapping_confidence[user_col] <- best$final_score
    } else {
      mapping_methods[user_col] <- "none"
      mapping_confidence[user_col] <- 0
    }
  }

  # -------------------------------------------------------------------
  # DEDUPLICATION: Handle multiple user columns mapping to same DB column
  # -------------------------------------------------------------------

  # Find duplicate target mappings
  mapped_cols <- !is.na(mappings)
  if (sum(mapped_cols) > 0) {
    target_counts <- table(mappings[mapped_cols])
    duplicated_targets <- names(target_counts[target_counts > 1])

    if (length(duplicated_targets) > 0) {
      message("Found ", length(duplicated_targets), " database column(s) with multiple user column mappings")

      for (target in duplicated_targets) {
        # Find all user columns mapped to this target
        duplicate_user_cols <- names(mappings)[which(mappings == target)]

        # Create priority scores: exact=4, synonym=3, fuzzy=2, none=1
        priority_map <- c("exact" = 4, "synonym" = 3, "fuzzy" = 2, "none" = 1)
        priorities <- priority_map[mapping_methods[duplicate_user_cols]]

        # Composite score: (priority * 100) + (confidence * 10)
        scores <- (priorities * 100) + (mapping_confidence[duplicate_user_cols] * 10)

        # Find best mapping
        best_user_col <- duplicate_user_cols[which.max(scores)]
        loser_user_cols <- setdiff(duplicate_user_cols, best_user_col)

        # Report
        message("Target '", target, "': keeping '", best_user_col, "' (",
                mapping_methods[best_user_col], ", conf=",
                round(mapping_confidence[best_user_col], 2), ")")
        message("  Unmarking: ", paste(loser_user_cols, collapse = ", "))

        # Unmap losers (set to NA = skip)
        mappings[loser_user_cols] <- NA
        mapping_methods[loser_user_cols] <- "none"
        mapping_confidence[loser_user_cols] <- 0
      }
    }
  }

  # Create mapping result with metadata
  result <- list(
    mappings = mappings,
    methods = mapping_methods,
    confidence = mapping_confidence,
    unmapped = user_cols[is.na(mappings)],
    alternatives = mapping_alternatives
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

  # Normalize: remove ALL special characters, spaces, underscores, dots, brackets
  normalize <- function(x) {
    gsub("[^a-z0-9]", "", tolower(trimws(x)))
  }

  user_col_normalized <- normalize(user_col_clean)

  # STEP 1: Try exact match first (highest confidence)
  for (target_col in names(synonyms)) {
    # First check if matches target column name itself
    if (user_col_normalized == normalize(target_col)) {
      return(target_col)
    }

    # Then check synonyms for exact match
    synonym_list_normalized <- sapply(synonyms[[target_col]], normalize)

    if (user_col_normalized %in% synonym_list_normalized) {
      return(target_col)
    }
  }

  # STEP 2: Try pattern/substring match (e.g., "DBH [cm]" contains "dbh")
  # Use composite scoring: prioritize longest synonym, then highest similarity
  best_match <- NULL
  best_match_score <- 0

  # Separator-preserving form: keep underscores/dashes for word boundary detection.
  # Full normalization (used in STEP 1) strips underscores, so "elevation_meters_asl"
  # becomes "elevationmetersasl" — making the boundary regex unreliable. Keeping
  # separators here lets us correctly detect that "elevation_meters" ends at a real
  # word boundary in "elevation_meters_asl".
  to_sep_form <- function(x) {
    x <- gsub("[^a-z0-9_-]", "_", tolower(trimws(x)))
    x <- gsub("[_-]+", "_", x)
    gsub("^_|_$", "", x)
  }
  user_col_with_sep <- to_sep_form(user_col_clean)

  for (target_col in names(synonyms)) {
    for (synonym in synonyms[[target_col]]) {
      synonym_norm <- normalize(synonym)

      # Skip very short synonyms to avoid false positives (e.g., "y", "x", "h")
      if (nchar(synonym_norm) < 3) {
        next
      }

      # Check if synonym is contained in user column name at word boundaries
      synonym_with_sep <- to_sep_form(synonym)
      pattern <- sprintf("(^|_)%s(_|$)", synonym_with_sep)
      if (grepl(pattern, user_col_with_sep)) {
        # Calculate similarity to target column for tiebreaking
        similarity <- stringdist::stringsim(user_col_clean, target_col)

        # Composite score: length (weighted 100x) + similarity (weighted 10x)
        # This ensures longer matches win, but similarity breaks ties
        composite_score <- (nchar(synonym_norm) * 100) + (similarity * 10)

        if (composite_score > best_match_score) {
          best_match <- target_col
          best_match_score <- composite_score
        }
      }
    }
  }

  if (!is.null(best_match)) {
    return(best_match)
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


#' Score All Candidate Column Matches (Internal Helper)
#'
#' Evaluates all possible matches across exact, synonym, and fuzzy strategies
#' for both direct and feature columns. Returns candidates ranked by final score.
#'
#' @param user_col_clean Cleaned user column name
#' @param direct_cols Direct database columns
#' @param feature_cols All feature columns (subplot + trait features)
#' @param synonyms Synonym dictionary (names are target columns, values are synonym lists)
#' @param required_cols Required column names
#' @param similarity_threshold Fuzzy match threshold
#'
#' @return List of candidate matches, each with: match, method, category, base_score, final_score.
#'         Sorted descending by final_score. Returns NULL if no candidates found.
#'
#' @keywords internal
.score_candidates <- function(user_col_clean, direct_cols, feature_cols,
                              synonyms, required_cols, similarity_threshold = 0.6) {

  candidates <- list()

  # Helper to add a candidate
  add_candidate <- function(match, method, category, base_score) {
    if (!is.na(match) && !is.null(match) && match != "") {
      candidates[[length(candidates) + 1]] <<- list(
        match = match,
        method = method,
        category = category,
        base_score = base_score
      )
    }
  }

  # Category multipliers
  multipliers <- c(
    required_direct = 2.0,
    direct = 1.5,
    feature = 1.0
  )

  # ========== DIRECT COLUMNS ==========

  # 1. Exact match in direct columns
  exact_d <- direct_cols[tolower(direct_cols) == user_col_clean]
  if (length(exact_d) > 0) {
    cat <- if (exact_d[1] %in% required_cols) "required_direct" else "direct"
    add_candidate(exact_d[1], "exact", cat, 1.0)
  }

  # 2. Synonym match in direct columns
  # Filter synonyms to only include direct column targets
  dir_synonyms <- synonyms[names(synonyms) %in% direct_cols]
  if (length(dir_synonyms) > 0) {
    syn_d <- .find_synonym_match(user_col_clean, dir_synonyms)
    if (!is.null(syn_d)) {
      cat <- if (syn_d %in% required_cols) "required_direct" else "direct"
      add_candidate(syn_d, "synonym", cat, 0.9)
    }
  }

  # 3. Fuzzy match in direct columns
  fz_d <- .fuzzy_match_column(user_col_clean, direct_cols, similarity_threshold)
  if (!is.null(fz_d$match)) {
    cat <- if (fz_d$match %in% required_cols) "required_direct" else "direct"
    add_candidate(fz_d$match, "fuzzy", cat, fz_d$similarity)
  }

  # ========== FEATURE COLUMNS ==========

  # 4. Exact match in feature columns
  exact_f <- feature_cols[tolower(feature_cols) == user_col_clean]
  if (length(exact_f) > 0) {
    add_candidate(exact_f[1], "exact", "feature", 1.0)
  }

  # 5. Synonym match in feature columns
  # Filter synonyms to only include feature column targets
  feat_synonyms <- synonyms[names(synonyms) %in% feature_cols]
  if (length(feat_synonyms) > 0) {
    syn_f <- .find_synonym_match(user_col_clean, feat_synonyms)
    if (!is.null(syn_f)) {
      add_candidate(syn_f, "synonym", "feature", 0.9)
    }
  }

  # 6. Fuzzy match in feature columns
  fz_f <- .fuzzy_match_column(user_col_clean, feature_cols, similarity_threshold)
  if (!is.null(fz_f$match)) {
    add_candidate(fz_f$match, "fuzzy", "feature", fz_f$similarity)
  }

  # Return NULL if no candidates found
  if (length(candidates) == 0) {
    return(NULL)
  }

  # Apply multipliers and compute final scores
  for (i in seq_along(candidates)) {
    candidates[[i]]$final_score <-
      candidates[[i]]$base_score * multipliers[candidates[[i]]$category]
  }

  # Sort descending by final_score
  candidates[order(-sapply(candidates, function(x) x$final_score))]
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
