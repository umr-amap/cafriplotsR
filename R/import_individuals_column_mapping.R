# Column Mapping for Individual Data Import
#
# Smart column mapping for individual tree data with fuzzy matching and
# domain-specific synonyms (e.g., dbh = stem_diameter, tree_id = tag)

#' Get Individual Column Synonym Dictionary
#'
#' Returns a comprehensive dictionary mapping common column name variations
#' to standard database column names for individual tree data.
#'
#' @return Named list where names are standard columns and values are character
#'   vectors of synonyms
#'
#' @keywords internal
.get_individual_column_synonyms <- function() {
  list(
    # Individual identification
    plot_name = c(
      "plot", "plot_id", "plotid", "plot.id", "plot code", "plot_code", "plotcode",
      "site_id", "siteid", "site.id", "site_name", "sitename", "site.name",
      "plot no", "plot_no", "plotno", "plot number", "plot_number",
      "transect_id", "transect_name", "transect", "parcelle", "nom_parcelle",
      "plot_ref",
      "plot_number",
      "Nom de la parcelle",
      "Nom du plot",
      "Nom du transect"
    ),

    tag = c(
      "tree_id", "treeid", "tree.id", "tree_tag", "treetag", "tree.tag",
      "tree_number", "treenumber", "tree.number", "tree_no", "treeno", "tree.no",
      "individual_id", "individualid", "individual.id",
      "tree number", "number", "no", "num", "numero", "arbre",
      "id_arbre", "id arbre", "tag number", "tag_number"
    ),

    # Taxonomy (pre-matched)
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

    # Herbarium specimens (optional)
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

    # Multi-stem identifier (optional)
    multi_tiges_id = c(
      "multi_stem", "multistem", "multi.stem",
      "stem_id", "stemid", "stem.id",
      "multistem_id", "multistemid", "multistem.id",
      "stem_code", "stemcode", "stem.code",
      "stem letter", "stem_letter", "stemletter",
      "multi tige", "multi_tige", "tige",
      "stem", "stem identifier", "stem_identifier"
    )
  )
}


#' Get Trait Column Synonym Dictionary
#'
#' Returns synonyms for common trait/feature column names.
#' Includes domain-specific equivalents like dbh = stem_diameter.
#'
#' @return Named list where names are standard trait names and values are
#'   character vectors of synonyms
#'
#' @keywords internal
.get_trait_column_synonyms <- function() {
  list(
    # Stem diameter
    stem_diameter = c(
      "dbh", "d.b.h", "d.b.h.", "diameter", "diam", "d",
      "stem_diam", "stemdiam", "stem.diam",
      "tree_diameter", "treediameter", "tree.diameter",
      "trunk_diameter", "trunkdiameter", "trunk.diameter",
      "diameter_breast_height", "diameterbreastheight",
      "breast_height_diameter", "breastheightdiameter",
      "diametre", "diamètre", "diam_cm", "dbh_cm", "circ",
      "circonference", "circonférence"
    ),

    # Height of measurement
    height_of_stem_diameter = c(
      "pom", "p.o.m", "p.o.m.", "point_of_measurement", "pointofmeasurement",
      "measurement_height", "measurementheight", "measurement.height",
      "height_measurement", "heightmeasurement", "height.measurement",
      "dbh_height", "dbhheight", "dbh.height",
      "measure_height", "measureheight", "measure.height",
      "hauteur_mesure", "hauteur mesure", "haut_mes",
      "height_of_measurement", "heightofmeasurement"
    ),

    # Tree height
    tree_height = c(
      "height", "h", "ht", "total_height", "totalheight", "total.height",
      "tree_h", "treeh", "tree.h", "h_tree", "htree", "h.tree",
      "hauteur", "h_total", "htotal", "h.total",
      "tree height", "total tree height", "hauteur totale",
      "hauteur_arbre", "hauteur arbre"
    ),

    # Crown width
    crown_width = c(
      "crown_diameter", "crowndiameter", "crown.diameter",
      "crown_diam", "crowndiam", "crown.diam",
      "canopy_width", "canopywidth", "canopy.width",
      "canopy_diameter", "canopydiameter", "canopy.diameter",
      "crown", "canopy", "diam_crown", "diamcrown",
      "largeur_couronne", "diametre_couronne", "diamètre couronne"
    ),

    # Specific leaf area
    specific_leaf_area = c(
      "sla", "s.l.a", "s.l.a.",
      "leaf_area_mass", "leafareamass", "leaf.area.mass",
      "specific_leaf", "specificleaf", "specific.leaf",
      "sla_cm2g", "slacm2g", "sla.cm2g",
      "aire foliaire specifique", "sla_value"
    ),

    # Wood specific gravity
    wood_specific_gravity = c(
      "wsg", "w.s.g", "w.s.g.",
      "wood_density", "wooddensity", "wood.density",
      "specific_gravity", "specificgravity", "specific.gravity",
      "wood_sg", "woodsg", "wood.sg",
      "density", "dens", "densite", "densité",
      "densité bois", "densite_bois", "wood dens",
      "wd", "ws_gravity", "wsgravity"
    ),

    # Leaf area
    leaf_area = c(
      "la", "l.a", "l.a.",
      "leaf_surface", "leafsurface", "leaf.surface",
      "foliage_area", "foliagearea", "foliage.area",
      "leaf_size", "leafsize", "leaf.size",
      "area_leaf", "arealeaf", "area.leaf",
      "aire_foliaire", "aire foliaire", "surface_feuille",
      "la_cm2", "lacm2", "la.cm2"
    ),

    # Census date (for features)
    census_date = c(
      "date", "survey_date", "surveydate", "survey.date",
      "measurement_date", "measurementdate", "measurement.date",
      "census_date", "censusdate", "census.date",
      "date_census", "datecensus", "date.census",
      "date_survey", "datesurvey", "date.survey",
      "date_recensement", "date recensement", "date_mesure",
      "observation_date", "observationdate", "obs_date"
    )
  )
}


#' Map Individual Data Columns
#'
#' Automatically maps user column names from individual data import files
#' to database schema. Handles both the individuals sheet (flat columns) and
#' the features sheet (trait measurements).
#'
#' Uses multiple strategies:
#' 1. Exact matching
#' 2. Synonym dictionary (including domain-specific like dbh = stem_diameter)
#' 3. Fuzzy string matching
#'
#' @param data Data frame with all columns in a single flat table (RECOMMENDED).
#'   The simplest approach - provide your complete dataset and the function
#'   will interactively guide you through column classification and mapping.
#' @param individuals_data Data frame from individuals sheet (OLD APPROACH).
#'   Must have columns that map to: plot_name, tag, idtax_n, original_tax_name.
#'   For backward compatibility - use `data` parameter instead for easier workflow.
#' @param features_data Data frame from features sheet (OLD APPROACH).
#'   Should have linking columns (plot_name, tag) plus trait measurements.
#'   For backward compatibility - use `data` parameter instead for easier workflow.
#' @param method Method type (e.g., "1ha-IRD", "Large"). Used for validation.
#' @param similarity_threshold Numeric: minimum similarity for fuzzy matching (0-1).
#'   Default: 0.6
#' @param interactive Logical: enable interactive column classification and mapping.
#'   Default: TRUE (highly recommended for new single-table workflow)
#' @param con Database connection. If NULL, creates temporary connection.
#'
#' @return List with mapped data:
#'   - individuals: Data frame with standardized individual column names
#'   - features: Data frame with standardized trait/feature column names (if any features found)
#'   - mapping_info: Details about how columns were mapped
#'
#' @section Two Workflows:
#' **NEW RECOMMENDED WORKFLOW (single flat table):**
#'
#' Simply provide all your data in one table. The function will interactively
#' guide you to classify each column as either an individual column (plot_name,
#' tag, idtax_n, etc.) or a feature/trait measurement (stem_diameter, height, etc.).
#'
#' **OLD WORKFLOW (two separate tables):**
#'
#' Manually separate data into individuals and features tables before calling.
#' Still supported for backward compatibility.
#'
#' @section Typical Usage:
#' This function is typically called after:
#' 1. Taxonomy standardization (separate step using taxonomic matching tools!)
#' 2. Data collection/template filling
#'
#' And before:
#' 1. Data validation (validate_individual_data())
#' 2. Database import (import_individual_data())
#'
#' @examples
#' \dontrun{
#' # NEW RECOMMENDED APPROACH: Single flat table
#' my_trees <- readxl::read_excel("field_data.xlsx")
#' # Columns: Plot, TreeID, Species, idtax, DBH, Height, WoodDensity, etc.
#'
#' mapped <- map_individual_columns(data = my_trees, interactive = TRUE)
#' # Interactive prompts guide you through:
#' # - Automatic matching where possible
#' # - For unmapped columns: "Is this a feature?" (yes/no/skip)
#' # - If NO: Select from list of individual columns
#' # - If YES: Select from list of available traits
#'
#' # OLD APPROACH: Two separate tables (still works)
#' individuals <- readxl::read_excel("file.xlsx", sheet = "individuals")
#' features <- readxl::read_excel("file.xlsx", sheet = "features")
#' mapped <- map_individual_columns(
#'   individuals_data = individuals,
#'   features_data = features
#' )
#'
#' # Access results
#' mapped$individuals  # Standardized individual data
#' mapped$features     # Standardized trait data (if any)
#' mapped$mapping_info # Details about mappings
#' }
#'
#' @seealso
#' [validate_individual_data()] for data validation
#' [import_individual_data()] for database import
#'
#' @export
map_individual_columns <- function(data = NULL,
                                   individuals_data = NULL,
                                   features_data = NULL,
                                   method = NULL,
                                   similarity_threshold = 0.6,
                                   interactive = TRUE,
                                   con = NULL) {

  # -------------------------------------------------------------------
  # Parameter Validation
  # -------------------------------------------------------------------

  # Check for conflicting parameters
  if (!is.null(data) && !is.null(individuals_data)) {
    stop(
      "Cannot provide both 'data' and 'individuals_data'.\n",
      "Use 'data' for single table workflow OR 'individuals_data'/'features_data' for two-table workflow.",
      call. = FALSE
    )
  }

  # Check that at least one is provided
  if (is.null(data) && is.null(individuals_data)) {
    stop(
      "Must provide either:\n",
      "  - 'data' (single flat table, recommended) OR\n",
      "  - 'individuals_data' (two-table approach for backward compatibility)",
      call. = FALSE
    )
  }

  # Create connection if not provided
  close_on_exit <- FALSE
  if (is.null(con)) {
    con <- call.mydb()
    close_on_exit <- TRUE
  }

  cli::cli_h1("Mapping Individual Data Columns")

  # -------------------------------------------------------------------
  # Workflow Detection and Routing
  # -------------------------------------------------------------------

  use_single_table <- !is.null(data)

  if (use_single_table) {
    cli::cli_alert_info("Using {.strong single flat table} workflow (recommended)")
    cat("\n")

    # NEW WORKFLOW: Single flat table with interactive classification
    result <- .map_flat_table_interactive(
      data = data,
      con = con,
      similarity_threshold = similarity_threshold,
      interactive = interactive
    )

  } else {
    cli::cli_alert_info("Using {.strong two-table} workflow (individuals + features)")
    cat("\n")

    # OLD WORKFLOW: Two separate tables
    result <- .map_two_tables(
      individuals_data = individuals_data,
      features_data = features_data,
      con = con,
      similarity_threshold = similarity_threshold,
      interactive = interactive
    )
  }

  # Cleanup
  if (close_on_exit) {
    DBI::dbDisconnect(con)
  }

  # -------------------------------------------------------------------
  # Print Mapping Summary
  # -------------------------------------------------------------------

  .print_mapping_summary(result)

  # -------------------------------------------------------------------
  # Return Results
  # -------------------------------------------------------------------

  cat("\n")
  cli::cli_rule("Mapping Complete")
  cat("\n")
  cli::cli_alert_success("Individual data columns mapped successfully")
  cat("\n")
  cli::cli_alert_info("Next steps:")
  cli::cli_ol(c(
    "Review mapped data structure",
    "Use {.fn validate_individual_data} to check data quality",
    "Use {.fn import_individual_data} to import to database"
  ))
  cat("\n")

  invisible(result)
}


#' Print Mapping Summary (Internal)
#'
#' Displays a formatted table showing how each column was mapped.
#'
#' @param result Result list from map_individual_columns
#' @keywords internal
.print_mapping_summary <- function(result) {

  # Check if we have column_classifications (new workflow)
  if (!is.null(result$mapping_info$column_classifications)) {

    cat("\n")
    cli::cli_rule("Column Mapping Summary")
    cat("\n")

    mapping_df <- result$mapping_info$column_classifications

    # Format for display
    display_df <- data.frame(
      `Your Column` = mapping_df$original_name,
      `Mapped To` = ifelse(is.na(mapping_df$mapped_to),
                          cli::col_red("(skipped)"),
                          mapping_df$mapped_to),
      `Type` = ifelse(is.na(mapping_df$type),
                     "-",
                     ifelse(mapping_df$type == "individual",
                           cli::col_blue("Individual"),
                           cli::col_green("Feature"))),
      `Method` = ifelse(is.na(mapping_df$method),
                       "-",
                       mapping_df$method),
      `Confidence` = ifelse(is.na(mapping_df$confidence),
                           "-",
                           sprintf("%.0f%%", mapping_df$confidence * 100)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # Print using kableExtra for nice formatting
    table_output <- kableExtra::kable(
      display_df,
      format = "html",
      escape = FALSE,
      align = c("l", "l", "c", "c", "c")
    ) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        position = "left"
      )

    print(table_output)

    # Summary statistics
    cat("\n")
    n_mapped <- sum(!is.na(mapping_df$mapped_to))
    n_skipped <- sum(is.na(mapping_df$mapped_to))
    n_individual <- sum(mapping_df$type == "individual", na.rm = TRUE)
    n_feature <- sum(mapping_df$type == "feature", na.rm = TRUE)

    cli::cli_text("{cli::col_silver('Summary:')}")
    summary_items <- c(
      paste0(cli::col_blue(n_individual), " individual column", if (n_individual != 1) "s" else ""),
      paste0(cli::col_green(n_feature), " feature column", if (n_feature != 1) "s" else "")
    )
    if (n_skipped > 0) {
      summary_items <- c(summary_items,
                        paste0(cli::col_red(n_skipped), " skipped column", if (n_skipped != 1) "s" else ""))
    }
    cli::cli_ul(summary_items)

  } else if (!is.null(result$mapping_info$individuals_mapping)) {
    # Old workflow - simpler summary
    cat("\n")
    cli::cli_rule("Column Mapping Summary")
    cat("\n")

    if (!is.null(result$mapping_info$individuals_mapping)) {
      cli::cli_alert_info("Individuals sheet:")
      indiv_map <- result$mapping_info$individuals_mapping
      cli::cli_text("  {length(indiv_map$exact_matches)} exact, {length(indiv_map$synonym_matches)} synonym, {length(indiv_map$fuzzy_matches)} fuzzy matches")
    }

    if (!is.null(result$mapping_info$features_mapping)) {
      cli::cli_alert_info("Features sheet:")
      feat_map <- result$mapping_info$features_mapping
      cli::cli_text("  {length(feat_map$exact_matches)} exact, {length(feat_map$synonym_matches)} synonym, {length(feat_map$fuzzy_matches)} fuzzy matches")
    }
  }

  cat("\n")
}


#' Map Two Separate Tables (Old Workflow) - Internal
#'
#' Refactored old workflow for backward compatibility.
#' Maps individuals and features tables separately.
#'
#' @param individuals_data Data frame with individual columns
#' @param features_data Data frame with trait columns (optional)
#' @param con Database connection
#' @param similarity_threshold Fuzzy matching threshold
#' @param interactive Enable interactive review
#'
#' @return List with individuals, features, and mapping_info
#' @keywords internal
.map_two_tables <- function(individuals_data,
                            features_data = NULL,
                            con,
                            similarity_threshold = 0.6,
                            interactive = TRUE) {

  # -------------------------------------------------------------------
  # 1. Map individuals sheet (flat columns)
  # -------------------------------------------------------------------

  cli::cli_h2("Step 1: Mapping 'individuals' sheet")

  individual_synonyms <- .get_individual_column_synonyms()

  # Define core individual columns
  core_individual_cols <- c(
    "plot_name", "tag", "idtax_n", "original_tax_name",
    "herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id"
  )

  individuals_mapping <- .map_sheet_columns(
    user_data = individuals_data,
    expected_columns = core_individual_cols,
    synonyms = individual_synonyms,
    sheet_name = "individuals",
    similarity_threshold = similarity_threshold,
    interactive = interactive
  )

  # Apply mapping to individuals data
  individuals_mapped <- .apply_column_mapping(
    individuals_data,
    individuals_mapping$final_mapping
  )

  # -------------------------------------------------------------------
  # 2. Map features sheet (traits) if provided
  # -------------------------------------------------------------------

  features_mapped <- NULL
  features_mapping <- NULL

  if (!is.null(features_data)) {
    cli::cli_h2("Step 2: Mapping 'features' sheet")

    # Get available traits from database
    all_traits <- traits_list()
    trait_names <- all_traits$trait

    # Linking columns for features
    linking_cols <- c("plot_name", "tag", "census_date", "census_id")

    # Expected columns = linking + all available traits
    expected_feature_cols <- c(linking_cols, trait_names)

    # Get trait synonyms
    trait_synonyms <- .get_trait_column_synonyms()

    # Combine individual and trait synonyms for linking columns
    combined_synonyms <- c(individual_synonyms, trait_synonyms)

    features_mapping <- .map_sheet_columns(
      user_data = features_data,
      expected_columns = expected_feature_cols,
      synonyms = combined_synonyms,
      sheet_name = "features",
      similarity_threshold = similarity_threshold,
      interactive = interactive
    )

    # Apply mapping to features data
    features_mapped <- .apply_column_mapping(
      features_data,
      features_mapping$final_mapping
    )
  } else {
    cli::cli_alert_info("No features sheet provided - skipping trait mapping")
  }

  # -------------------------------------------------------------------
  # 3. Return results
  # -------------------------------------------------------------------

  result <- list(
    individuals = individuals_mapped,
    features = features_mapped,
    mapping_info = list(
      individuals_mapping = individuals_mapping,
      features_mapping = features_mapping
    )
  )

  return(result)
}


#' Map Flat Table Interactively (New Workflow) - Internal
#'
#' Main coordinator for single flat table workflow. Performs automatic mapping
#' where possible, then interactively classifies unmapped columns as either
#' individual columns or trait/feature columns.
#'
#' @param data Data frame with all columns mixed together
#' @param con Database connection
#' @param similarity_threshold Fuzzy matching threshold
#' @param interactive Enable interactive classification
#'
#' @return List with individuals, features, and mapping_info
#' @keywords internal
.map_flat_table_interactive <- function(data,
                                        con,
                                        similarity_threshold = 0.6,
                                        interactive = TRUE) {

  cli::cli_h2("Step 1: Automatic Column Mapping")

  # -------------------------------------------------------------------
  # 1. Get available columns and synonyms
  # -------------------------------------------------------------------

  # Individual columns
  core_individual_cols <- c("plot_name", "tag", "idtax_n", "original_tax_name",
                           "herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id")
  individual_synonyms <- .get_individual_column_synonyms()

  # Trait columns
  all_traits <- traits_list()
  trait_names <- all_traits$trait
  trait_synonyms <- .get_trait_column_synonyms()

  # All possible columns
  all_possible_cols <- c(core_individual_cols, trait_names)
  all_synonyms <- c(individual_synonyms, trait_synonyms)

  # -------------------------------------------------------------------
  # 2. Try automatic mapping for all columns
  # -------------------------------------------------------------------

  user_cols <- colnames(data)
  column_classifications <- data.frame(
    original_name = user_cols,
    mapped_to = NA_character_,
    type = NA_character_,  # "individual" or "feature"
    method = NA_character_,  # "exact", "synonym", "fuzzy", "interactive"
    confidence = NA_real_,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(user_cols)) {
    user_col <- user_cols[i]
    user_col_clean <- tolower(trimws(user_col))

    # PRIORITY 1: Try exact match with INDIVIDUAL columns first
    if (user_col_clean %in% tolower(core_individual_cols)) {
      matched_col <- core_individual_cols[tolower(core_individual_cols) == user_col_clean]
      column_classifications$mapped_to[i] <- matched_col
      column_classifications$type[i] <- "individual"
      column_classifications$method[i] <- "exact"
      column_classifications$confidence[i] <- 1.0
      next
    }

    # PRIORITY 2: Try exact synonym match with INDIVIDUAL columns
    individual_synonym_match <- NA_character_
    for (col_name in names(individual_synonyms)) {
      if (user_col_clean %in% tolower(individual_synonyms[[col_name]])) {
        individual_synonym_match <- col_name
        break
      }
    }

    if (!is.na(individual_synonym_match)) {
      column_classifications$mapped_to[i] <- individual_synonym_match
      column_classifications$type[i] <- "individual"
      column_classifications$method[i] <- "synonym"
      column_classifications$confidence[i] <- 0.9
      next
    }

    # PRIORITY 2.5: Try pattern/substring synonym match with INDIVIDUAL columns
    # This catches cases like "DBH [cm]" or "dbh_cm" matching "dbh" synonym
    # Normalize by removing special characters, spaces, brackets, underscores
    user_col_normalized <- gsub("[^a-z0-9]", "", user_col_clean)

    individual_pattern_match <- NA_character_
    best_pattern_score <- 0  # Composite score: length + similarity
    best_pattern_length <- 0
    best_pattern_similarity <- 0

    for (col_name in names(individual_synonyms)) {
      synonyms_for_col <- tolower(individual_synonyms[[col_name]])

      for (synonym in synonyms_for_col) {
        # Normalize synonym same way
        synonym_normalized <- gsub("[^a-z0-9]", "", synonym)

        # Skip very short synonyms to avoid false positives (e.g., "h", "d")
        if (nchar(synonym_normalized) < 3) {
          next
        }

        # Check if synonym is contained in user column name
        if (grepl(synonym_normalized, user_col_normalized, fixed = TRUE)) {
          # Calculate overall similarity for tiebreaking
          similarity <- stringdist::stringsim(user_col_clean, col_name)

          # Composite score: prioritize length, then similarity
          # Length is weighted 100x more important than similarity
          composite_score <- (nchar(synonym_normalized) * 100) + (similarity * 10)

          # Pick best match based on:
          # 1. Longest synonym (most specific)
          # 2. Highest similarity to target column name (tiebreaker)
          if (composite_score > best_pattern_score) {
            individual_pattern_match <- col_name
            best_pattern_score <- composite_score
            best_pattern_length <- nchar(synonym_normalized)
            best_pattern_similarity <- similarity
          }
        }
      }
    }

    if (!is.na(individual_pattern_match)) {
      # Use similarity-adjusted confidence (higher similarity = higher confidence)
      adjusted_confidence <- 0.80 + (best_pattern_similarity * 0.1)  # 0.80-0.90 range

      column_classifications$mapped_to[i] <- individual_pattern_match
      column_classifications$type[i] <- "individual"
      column_classifications$method[i] <- "synonym"
      column_classifications$confidence[i] <- round(adjusted_confidence, 2)
      next
    }

    # PRIORITY 3: Try fuzzy match with INDIVIDUAL columns
    if (similarity_threshold > 0) {
      similarities_indiv <- stringdist::stringsim(user_col_clean, tolower(core_individual_cols))
      best_match_idx_indiv <- which.max(similarities_indiv)
      best_similarity_indiv <- similarities_indiv[best_match_idx_indiv]

      if (best_similarity_indiv >= similarity_threshold) {
        matched_col <- core_individual_cols[best_match_idx_indiv]
        column_classifications$mapped_to[i] <- matched_col
        column_classifications$type[i] <- "individual"
        column_classifications$method[i] <- "fuzzy"
        column_classifications$confidence[i] <- best_similarity_indiv
        next
      }
    }

    # PRIORITY 4: Try exact match with TRAIT columns
    if (user_col_clean %in% tolower(trait_names)) {
      matched_col <- trait_names[tolower(trait_names) == user_col_clean]
      column_classifications$mapped_to[i] <- matched_col
      column_classifications$type[i] <- "feature"
      column_classifications$method[i] <- "exact"
      column_classifications$confidence[i] <- 1.0
      next
    }

    # PRIORITY 5: Try exact synonym match with TRAIT columns
    trait_synonym_match <- NA_character_
    for (col_name in names(trait_synonyms)) {
      if (user_col_clean %in% tolower(trait_synonyms[[col_name]])) {
        trait_synonym_match <- col_name
        break
      }
    }

    if (!is.na(trait_synonym_match)) {
      column_classifications$mapped_to[i] <- trait_synonym_match
      column_classifications$type[i] <- "feature"
      column_classifications$method[i] <- "synonym"
      column_classifications$confidence[i] <- 0.9
      next
    }

    # PRIORITY 5.5: Try pattern/substring synonym match with TRAIT columns
    # This catches cases like "DBH [cm]" or "height_m" matching trait synonyms
    trait_pattern_match <- NA_character_
    best_trait_pattern_score <- 0  # Composite score: length + similarity
    best_trait_pattern_similarity <- 0

    for (col_name in names(trait_synonyms)) {
      synonyms_for_trait <- tolower(trait_synonyms[[col_name]])

      for (synonym in synonyms_for_trait) {
        # Normalize synonym same way as user column
        synonym_normalized <- gsub("[^a-z0-9]", "", synonym)

        # Skip very short synonyms to avoid false positives
        if (nchar(synonym_normalized) < 3) {
          next
        }

        # Check if synonym is contained in user column name
        if (grepl(synonym_normalized, user_col_normalized, fixed = TRUE)) {
          # Calculate overall similarity for tiebreaking
          similarity <- stringdist::stringsim(user_col_clean, col_name)

          # Composite score: prioritize length, then similarity
          composite_score <- (nchar(synonym_normalized) * 100) + (similarity * 10)

          # Pick best match
          if (composite_score > best_trait_pattern_score) {
            trait_pattern_match <- col_name
            best_trait_pattern_score <- composite_score
            best_trait_pattern_similarity <- similarity
          }
        }
      }
    }

    if (!is.na(trait_pattern_match)) {
      # Use similarity-adjusted confidence (0.80-0.90 range)
      adjusted_confidence <- 0.80 + (best_trait_pattern_similarity * 0.1)

      column_classifications$mapped_to[i] <- trait_pattern_match
      column_classifications$type[i] <- "feature"
      column_classifications$method[i] <- "synonym"
      column_classifications$confidence[i] <- round(adjusted_confidence, 2)
      next
    }

    # PRIORITY 6: Try fuzzy match with TRAIT columns
    if (similarity_threshold > 0) {
      similarities_trait <- stringdist::stringsim(user_col_clean, tolower(trait_names))
      best_match_idx_trait <- which.max(similarities_trait)
      best_similarity_trait <- similarities_trait[best_match_idx_trait]

      if (best_similarity_trait >= similarity_threshold) {
        matched_col <- trait_names[best_match_idx_trait]
        column_classifications$mapped_to[i] <- matched_col
        column_classifications$type[i] <- "feature"
        column_classifications$method[i] <- "fuzzy"
        column_classifications$confidence[i] <- best_similarity_trait
        next
      }
    }
  }

  # -------------------------------------------------------------------
  # DEDUPLICATION: Handle multiple user columns mapping to same DB column
  # -------------------------------------------------------------------

  # Find duplicate target mappings (multiple user columns → same DB column)
  mapped_rows <- which(!is.na(column_classifications$mapped_to))
  if (length(mapped_rows) > 0) {
    target_counts <- table(column_classifications$mapped_to[mapped_rows])
    duplicated_targets <- names(target_counts[target_counts > 1])

    if (length(duplicated_targets) > 0) {
      cli::cli_alert_warning("Found {length(duplicated_targets)} database column(s) with multiple user column mappings")

      for (target in duplicated_targets) {
        # Find all user columns mapped to this target
        duplicate_indices <- which(column_classifications$mapped_to == target)

        # Create priority scores for each mapping:
        # exact=4, synonym=3, fuzzy=2, interactive=1
        priority_map <- c("exact" = 4, "synonym" = 3, "fuzzy" = 2, "interactive" = 1)
        priorities <- priority_map[column_classifications$method[duplicate_indices]]

        # Composite score: (priority * 100) + confidence
        scores <- (priorities * 100) + (column_classifications$confidence[duplicate_indices] * 10)

        # Find best mapping
        best_idx <- duplicate_indices[which.max(scores)]
        loser_indices <- setdiff(duplicate_indices, best_idx)

        # Report what's being kept
        best_col <- column_classifications$original_name[best_idx]
        loser_cols <- column_classifications$original_name[loser_indices]

        cli::cli_alert_info("Target '{target}': keeping '{best_col}' ({column_classifications$method[best_idx]}, conf={column_classifications$confidence[best_idx]})")
        cli::cli_alert_info("  Unmarking: {paste(loser_cols, collapse = ', ')}")

        # Unmap the losers (set to NA so they become unmapped)
        column_classifications$mapped_to[loser_indices] <- NA
        column_classifications$type[loser_indices] <- NA
        column_classifications$method[loser_indices] <- NA
        column_classifications$confidence[loser_indices] <- NA
      }

      cat("\n")
    }
  }

  # Report automatic mapping results
  n_mapped <- sum(!is.na(column_classifications$mapped_to))
  n_unmapped <- sum(is.na(column_classifications$mapped_to))

  cli::cli_alert_success("Automatically mapped {n_mapped} column{?s}")

  if (n_unmapped > 0) {
    cli::cli_alert_info("{n_unmapped} column{?s} need{?s/} manual classification")
  }

  # -------------------------------------------------------------------
  # 3. Interactive classification for unmapped columns
  # -------------------------------------------------------------------

  if (n_unmapped > 0 && interactive) {
    cat("\n")
    cli::cli_h2("Step 2: Interactive Column Classification")
    cat("\n")

    unmapped_indices <- which(is.na(column_classifications$mapped_to))

    for (idx in unmapped_indices) {
      user_col <- column_classifications$original_name[idx]

      # Get sample values
      sample_values <- head(data[[user_col]], 3)

      # Ask user: is this a feature?
      classification <- .classify_column_interactive(user_col, sample_values)

      if (classification == "yes") {
        # Select trait column
        selected_trait <- .select_trait_column(user_col, con)

        if (!is.na(selected_trait)) {
          column_classifications$mapped_to[idx] <- selected_trait
          column_classifications$type[idx] <- "feature"
          column_classifications$method[idx] <- "interactive"
          column_classifications$confidence[idx] <- 1.0
        }

      } else if (classification == "no") {
        # Select individual column
        selected_indiv_col <- .select_individual_column(user_col)

        if (!is.na(selected_indiv_col)) {
          column_classifications$mapped_to[idx] <- selected_indiv_col
          column_classifications$type[idx] <- "individual"
          column_classifications$method[idx] <- "interactive"
          column_classifications$confidence[idx] <- 1.0
        }
      }
      # If "skip", leave as NA
    }
  }

  # -------------------------------------------------------------------
  # 4. Separate data into individuals and features
  # -------------------------------------------------------------------

  cat("\n")
  cli::cli_h2("Step 3: Separating Data")

  separated_data <- .separate_individuals_features(data, column_classifications)

  # -------------------------------------------------------------------
  # 5. Return results
  # -------------------------------------------------------------------

  result <- list(
    individuals = separated_data$individuals,
    features = separated_data$features,
    mapping_info = list(
      column_classifications = column_classifications,
      n_individual_cols = sum(column_classifications$type == "individual", na.rm = TRUE),
      n_feature_cols = sum(column_classifications$type == "feature", na.rm = TRUE),
      n_skipped = sum(is.na(column_classifications$mapped_to))
    )
  )

  return(result)
}


#' Classify Column Interactively - Internal
#'
#' Asks user whether a column represents a feature/trait measurement.
#'
#' @param column_name Name of the column to classify
#' @param sample_values Sample values from the column (for user reference)
#'
#' @return Character: "yes", "no", or "skip"
#' @keywords internal
.classify_column_interactive <- function(column_name, sample_values) {

  cat("\n")
  cli::cli_rule(paste("Column:", column_name))

  # Show sample values
  sample_display <- paste(head(sample_values, 3), collapse = ", ")
  cli::cli_alert_info("Sample values: {sample_display}")
  cat("\n")

  # Explain the question with examples
  cli::cli_text("{.strong What is this column?}")
  cat("\n")
  cli::cli_ul(c(
    "{.emph YES} - Feature/Trait measurement (e.g., DBH, height, wood density, leaf area)",
    "{.emph NO}  - Individual identification (e.g., plot name, tree tag, species name, taxonomy ID)",
    "{.emph SKIP} - Neither, ignore this column"
  ))
  cat("\n")

  # Ask question
  response <- readline(prompt = "Your choice (yes/no/skip): ")
  response_clean <- tolower(trimws(response))

  # Validate response
  while (!response_clean %in% c("yes", "y", "no", "n", "skip", "s", "")) {
    cli::cli_alert_warning("Invalid response. Please enter 'yes', 'no', or 'skip'")
    response <- readline(prompt = "Your choice (yes/no/skip): ")
    response_clean <- tolower(trimws(response))
  }

  # Normalize response
  if (response_clean %in% c("yes", "y")) {
    return("yes")
  } else if (response_clean %in% c("no", "n")) {
    return("no")
  } else {
    return("skip")
  }
}


#' Select Individual Column Interactively - Internal
#'
#' Uses .find_cat() to let user select which individual column
#' this represents from a numbered list.
#'
#' @param column_name Name of the column being mapped
#'
#' @return Character: selected individual column name, or NA if skipped
#' @keywords internal
.select_individual_column <- function(column_name) {

  cat("\n")
  cli::cli_alert_info("Select which {.strong individual column} '{column_name}' represents:")
  cat("\n")

  # Prepare individual columns table
  individual_cols <- data.frame(
    column_name = c("plot_name", "tag", "idtax_n", "original_tax_name",
                    "herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id"),
    description = c(
      "Plot identifier (required)",
      "Tree tag/number (optional - auto-generated if missing)",
      "Taxonomy ID (required)",
      "Original taxonomic name (required)",
      "Herbarium type specimen reference",
      "Herbarium specimen code/number",
      "Multi-stem identifier"
    ),
    stringsAsFactors = FALSE
  )

  # Create display column for .find_cat()
  individual_cols$display <- paste0(individual_cols$column_name, " - ", individual_cols$description)

  # Use .find_cat() for selection
  result <- .find_cat(
    value_to_search = column_name,
    compared_table = individual_cols,
    column_name = "column_name",
    field_label = "Individual Column"
  )

  # Extract selected index from result
  selected_idx <- result$selected_name

  if (!is.na(selected_idx) && selected_idx > 0 && selected_idx <= nrow(result$sorted_matches)) {
    # IMPORTANT: Get from sorted_matches, not original table!
    # .find_cat() reorders the table, so index refers to sorted order
    # The column was renamed to 'comp_value' by .find_cat()
    selected_col <- result$sorted_matches$comp_value[selected_idx]
    cli::cli_alert_success("Mapped to: {.field {selected_col}}")
    return(selected_col)
  } else {
    cli::cli_alert_info("Column skipped")
    return(NA_character_)
  }
}


#' Select Trait Column Interactively - Internal
#'
#' Uses .find_cat() to let user select which trait/feature
#' this represents from available traits in the database.
#'
#' @param column_name Name of the column being mapped
#' @param con Database connection
#'
#' @return Character: selected trait name, or NA if skipped
#' @keywords internal
.select_trait_column <- function(column_name, con) {

  cat("\n")
  cli::cli_alert_info("Select which {.strong trait/feature} '{column_name}' represents:")
  cat("\n")

  # Get all available traits
  all_traits <- traits_list()

  # Prepare traits table for .find_cat()
  traits_table <- data.frame(
    trait = all_traits$trait,
    description = ifelse(
      !is.na(all_traits$traitdescription) & all_traits$traitdescription != "",
      all_traits$traitdescription,
      paste0("Trait: ", all_traits$trait)
    ),
    stringsAsFactors = FALSE
  )

  # Add units if available
  if ("expectedunit" %in% names(all_traits)) {
    traits_table$description <- ifelse(
      !is.na(all_traits$expectedunit) & all_traits$expectedunit != "",
      paste0(traits_table$description, " (", all_traits$expectedunit, ")"),
      traits_table$description
    )
  }

  # Create display column
  traits_table$display <- paste0(traits_table$trait, " - ", traits_table$description)

  # Use .find_cat() for selection with fuzzy matching
  result <- .find_cat(
    value_to_search = column_name,
    compared_table = traits_table,
    column_name = "trait",
    field_label = "Trait/Feature"
  )

  # Extract selected index from result
  selected_idx <- result$selected_name

  if (!is.na(selected_idx) && selected_idx > 0 && selected_idx <= nrow(result$sorted_matches)) {
    # IMPORTANT: Get from sorted_matches, not original table!
    # .find_cat() reorders the table, so index refers to sorted order
    # The column was renamed to 'comp_value' by .find_cat()
    selected_trait <- result$sorted_matches$comp_value[selected_idx]
    cli::cli_alert_success("Mapped to: {.field {selected_trait}}")
    return(selected_trait)
  } else {
    cli::cli_alert_info("Column skipped")
    return(NA_character_)
  }
}


#' Separate Flat Table into Individuals and Features - Internal
#'
#' Based on column classifications, separates the flat table into
#' two dataframes: individuals (core data) and features (traits).
#'
#' @param data Original flat table data frame
#' @param column_classifications Data frame with classification results
#'
#' @return List with individuals and features dataframes
#' @keywords internal
.separate_individuals_features <- function(data, column_classifications) {

  # Get classified columns
  individual_cols <- column_classifications %>%
    dplyr::filter(type == "individual", !is.na(mapped_to))

  feature_cols <- column_classifications %>%
    dplyr::filter(type == "feature", !is.na(mapped_to))

  # -------------------------------------------------------------------
  # Build individuals dataframe
  # -------------------------------------------------------------------

  if (nrow(individual_cols) > 0) {
    individuals <- data[, individual_cols$original_name, drop = FALSE]
    names(individuals) <- individual_cols$mapped_to

    cli::cli_alert_success("Created individuals table with {ncol(individuals)} column{?s}")
  } else {
    cli::cli_alert_warning("No individual columns mapped")
    individuals <- NULL
  }

  # -------------------------------------------------------------------
  # Build features dataframe
  # -------------------------------------------------------------------

  features <- NULL

  if (nrow(feature_cols) > 0) {
    # Features need linking columns (plot_name, tag) + trait columns

    # Check if we have at least plot_name (required for linking)
    has_plot_name <- "plot_name" %in% individual_cols$mapped_to

    if (has_plot_name) {
      # Get feature columns from original data
      feature_original_names <- feature_cols$original_name
      features_data_part <- data[, feature_original_names, drop = FALSE]
      names(features_data_part) <- feature_cols$mapped_to

      # Get linking columns from INDIVIDUALS table (not original data)
      # This ensures we get auto-generated tags if they exist
      linking_col_names <- intersect(c("plot_name", "tag"), names(individuals))

      if (length(linking_col_names) > 0) {
        linking_data <- individuals[, linking_col_names, drop = FALSE]

        # Combine linking columns + feature columns
        features <- cbind(linking_data, features_data_part)

        cli::cli_alert_success("Created features table with {nrow(feature_cols)} trait{?s}")
        cli::cli_alert_info("Linked via: {paste(linking_col_names, collapse = ', ')}")
      } else {
        cli::cli_alert_warning("No linking columns available in individuals table")
        features <- NULL
      }

    } else {
      cli::cli_alert_warning(
        "Cannot create features table: missing plot_name column"
      )
      cli::cli_alert_info("Features require at least plot_name to link to individuals")
    }
  } else {
    cli::cli_alert_info("No feature columns mapped (individual data only)")
  }

  return(list(
    individuals = individuals,
    features = features
  ))
}


#' Map Sheet Columns (Internal Helper)
#'
#' Maps columns for a single sheet using exact, synonym, and fuzzy matching.
#'
#' @param user_data Data frame with user columns
#' @param expected_columns Character vector of valid database columns
#' @param synonyms Synonym dictionary
#' @param sheet_name Sheet name for messaging
#' @param similarity_threshold Fuzzy matching threshold
#' @param interactive Allow interactive review
#'
#' @return List with mapping results
#' @keywords internal
.map_sheet_columns <- function(user_data,
                               expected_columns,
                               synonyms,
                               sheet_name,
                               similarity_threshold = 0.6,
                               interactive = TRUE) {

  user_cols <- colnames(user_data)

  # Storage for mappings
  mappings <- setNames(rep(NA_character_, length(user_cols)), user_cols)
  mapping_methods <- setNames(rep(NA_character_, length(user_cols)), user_cols)
  mapping_confidence <- setNames(rep(NA_real_, length(user_cols)), user_cols)

  for (user_col in user_cols) {

    # Clean column name for matching
    user_col_clean <- tolower(trimws(user_col))

    # 1. EXACT MATCH
    if (user_col %in% expected_columns || user_col_clean %in% tolower(expected_columns)) {
      exact_match <- expected_columns[tolower(expected_columns) == user_col_clean]
      if (length(exact_match) > 0) {
        mappings[user_col] <- exact_match[1]
        mapping_methods[user_col] <- "exact"
        mapping_confidence[user_col] <- 1.0
        next
      }
    }

    # 2. SYNONYM MATCH
    synonym_match <- .find_synonym_match_individual(user_col_clean, synonyms)
    if (!is.null(synonym_match)) {
      # Verify synonym is in expected columns
      if (synonym_match %in% expected_columns) {
        mappings[user_col] <- synonym_match
        mapping_methods[user_col] <- "synonym"
        mapping_confidence[user_col] <- 1.0
        next
      }
    }

    # 3. FUZZY MATCH
    fuzzy_result <- .fuzzy_match_column_individual(user_col_clean, expected_columns, similarity_threshold)
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
  cli::cli_h3("Mapping Results for '{sheet_name}' sheet")
  cli::cli_alert_success("Exact matches: {sum(mapping_methods == 'exact', na.rm=TRUE)}")
  cli::cli_alert_success("Synonym matches: {sum(mapping_methods == 'synonym', na.rm=TRUE)}")

  if (sum(mapping_methods == "fuzzy", na.rm=TRUE) > 0) {
    cli::cli_alert_info("Fuzzy matches: {sum(mapping_methods == 'fuzzy', na.rm=TRUE)}")
  }

  if (length(result$unmapped) > 0) {
    cli::cli_alert_warning("Unmapped columns ({length(result$unmapped)}):")
    cli::cli_ul(result$unmapped)
  }

  # Interactive review if requested
  if (interactive && (sum(mapping_methods == "fuzzy", na.rm=TRUE) > 0 || length(result$unmapped) > 0)) {
    result <- .review_individual_mappings_interactive(
      result, user_data, expected_columns, sheet_name
    )
  } else {
    # Add final mapping (filter out unmapped) for non-interactive mode
    result$final_mapping <- result$mappings[!is.na(result$mappings)]
  }

  return(result)
}


#' Find Synonym Match for Individual Columns (Internal Helper)
#'
#' Searches synonym dictionary for match with robust normalization.
#' Handles spaces, underscores, dots interchangeably.
#'
#' @param user_col_clean Cleaned user column name (lowercase, trimmed)
#' @param synonyms Synonym dictionary
#'
#' @return Database column name or NULL
#' @keywords internal
.find_synonym_match_individual <- function(user_col_clean, synonyms) {

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


#' Fuzzy Match Column for Individuals (Internal Helper)
#'
#' Uses string similarity to find best match.
#'
#' @param user_col_clean Cleaned user column name
#' @param expected_cols Expected database column names
#' @param threshold Similarity threshold
#'
#' @return List with match and similarity, or NULL
#' @keywords internal
.fuzzy_match_column_individual <- function(user_col_clean, expected_cols, threshold = 0.6) {

  # Calculate similarities
  similarities <- stringdist::stringsim(user_col_clean, tolower(expected_cols))

  # Find best match above threshold
  best_idx <- which.max(similarities)
  best_similarity <- similarities[best_idx]

  if (length(best_idx) > 0 && best_similarity >= threshold) {
    return(list(
      match = expected_cols[best_idx],
      similarity = best_similarity
    ))
  }

  return(NULL)
}


#' Review Individual Mappings Interactively (Internal Helper)
#'
#' Allow user to review and adjust automatic mappings.
#'
#' @param result Mapping result
#' @param user_data User data
#' @param expected_cols Expected database columns
#' @param sheet_name Sheet name
#'
#' @return Updated mapping result
#' @keywords internal
.review_individual_mappings_interactive <- function(result, user_data, expected_cols, sheet_name) {

  cat("\n")
  cli::cli_h3("Review Mappings Interactively")
  cat("\n")

  # Review fuzzy matches
  fuzzy_indices <- which(result$methods == "fuzzy")
  if (length(fuzzy_indices) > 0) {
    cli::cli_alert_info("Reviewing fuzzy matches...")
    cat("\n")

    for (idx in fuzzy_indices) {
      user_col <- names(result$mappings)[idx]
      db_col <- result$mappings[idx]
      confidence <- result$confidence[idx]

      cli::cli_text("User column: {cli::col_yellow(user_col)}")
      cli::cli_text("Mapped to: {cli::col_green(db_col)} (confidence: {round(confidence, 2)})")

      response <- readline("Accept? (y/n/new): ")

      if (tolower(trimws(response)) == "n") {
        result$mappings[idx] <- NA_character_
        result$methods[idx] <- "none"
        result$confidence[idx] <- 0
        result$unmapped <- c(result$unmapped, user_col)
      } else if (tolower(trimws(response)) != "y" && trimws(response) != "") {
        # User provided new mapping
        new_mapping <- trimws(response)
        if (new_mapping %in% expected_cols) {
          result$mappings[idx] <- new_mapping
          result$methods[idx] <- "manual"
          result$confidence[idx] <- 1.0
        } else {
          cli::cli_alert_warning("'{new_mapping}' not in expected columns - keeping original")
        }
      }
      cat("\n")
    }
  }

  # Handle unmapped columns
  if (length(result$unmapped) > 0) {
    cat("\n")
    cli::cli_alert_info("Handling unmapped columns...")
    cat("\n")

    for (user_col in result$unmapped) {
      cli::cli_text("Unmapped column: {cli::col_yellow(user_col)}")
      cli::cli_text("Sample values: {paste(head(user_data[[user_col]], 3), collapse=', ')}")
      cat("\n")

      response <- readline("Map to (or press Enter to skip): ")

      if (trimws(response) != "") {
        new_mapping <- trimws(response)
        if (new_mapping %in% expected_cols) {
          idx <- which(names(result$mappings) == user_col)
          result$mappings[idx] <- new_mapping
          result$methods[idx] <- "manual"
          result$confidence[idx] <- 1.0
          result$unmapped <- setdiff(result$unmapped, user_col)
          cli::cli_alert_success("Mapped to: {new_mapping}")
        } else {
          cli::cli_alert_warning("'{new_mapping}' not in expected columns - skipping")
        }
      }
      cat("\n")
    }
  }

  # Add final mapping (filter out unmapped)
  result$final_mapping <- result$mappings[!is.na(result$mappings)]

  return(result)
}


#' Apply Column Mapping (Internal Helper)
#'
#' Applies the final mapping to rename columns in user data.
#'
#' @param user_data Data frame with user column names
#' @param mapping Named character vector: user_col_name = database_col_name
#'
#' @return Data frame with renamed columns
#' @keywords internal
.apply_column_mapping <- function(user_data, mapping) {

  # Keep only columns that have mappings
  mapped_cols <- names(mapping)
  user_data_subset <- user_data[, mapped_cols, drop = FALSE]

  # Rename columns
  colnames(user_data_subset) <- mapping

  return(user_data_subset)
}
