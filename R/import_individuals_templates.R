#' Generate Individual Data Import Template
#'
#' Creates an Excel template for importing individual tree data with proper
#' column structure, validation rules, and example data. The template includes
#' two sheets: core individual data and optional individual features.
#'
#' @param method Method type (e.g., "1ha-IRD", "Large"). If NULL, includes
#'   all possible columns. Use method_list() to see available methods.
#' @param include_features Logical. If TRUE, includes a second sheet for
#'   individual features (traits). Default TRUE.
#' @param output_file Path where the Excel template should be saved.
#'   Default: "individual_template.xlsx"
#' @param con Database connection. If NULL, creates temporary connection.
#' @param return_data Logical. If TRUE, returns the template data structure
#'   without writing to file or printing messages. Useful for Shiny apps.
#'   Default FALSE.
#'
#' @return Invisibly returns the template data structure. Main effect is
#'   creating the Excel file.
#'
#' @section Pre-requisite - Taxonomy Standardization:
#' **IMPORTANT**: Before importing individuals, you must standardize taxonomic names
#' to obtain `idtax_n` values. Two options:
#'
#' 1. **Automatic matching**: Use `match_taxonomic_names()` function
#' 2. **Semi-automatic with validation**: Use the Shiny taxonomic matching app
#'    (exports CSV/Excel with `idtax_n` column added)
#'
#' The `idtax_n` column must not be empty for any individual.
#'
#' @section Template Structure:
#'
#' **Sheet 1: "individuals"** (Core individual data)
#' - Mandatory columns: plot_name, tag, idtax_n, original_tax_name
#' - Optional columns: herbarium_nbe_type, herbarium_nbe_char, multi_tiges_id
#' - Method-specific requirements apply
#'
#' **Sheet 2: "features"** (Optional individual traits)
#' - Dynamic columns from traits_list()
#' - Links: plot_name, tag (to identify individual)
#' - Optional: census_date or census_id (for temporal measurements)
#'
#' @section Workflow:
#' 1. Generate template: `get_individual_template()`
#' 2. Standardize taxonomy (separate step!)
#' 3. Fill in template with your data
#' 4. Import: Use `map_individual_columns()` → `validate_individual_data()` → `import_individual_data()`
#'
#' @examples
#' \dontrun{
#' # Generate template for specific method
#' get_individual_template(method = "1ha-IRD", output_file = "my_individuals.xlsx")
#'
#' # Generate template with all possible columns
#' get_individual_template(output_file = "full_template.xlsx")
#'
#' # Without features sheet (individuals only)
#' get_individual_template(include_features = FALSE)
#' }
#'
#' @seealso
#' [traits_list()] to see available individual features
#' [method_list()] to see available methods
#'
#' @export
get_individual_template <- function(method = NULL,
                                    include_features = TRUE,
                                    output_file = "individual_template.xlsx",
                                    con = NULL,
                                    return_data = FALSE) {

  # Create connection if not provided
  if (is.null(con)) {
    con <- call.mydb()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  if (!return_data) {
    cli::cli_h1("Generating Individual Data Import Template")
  }

  # Get column definitions
  if (!return_data) {
    cli::cli_alert_info("Fetching column definitions from database...")
  }

  individual_cols <- .get_individual_columns_from_db(con, method)

  # Build individuals sheet
  if (!return_data) {
    cli::cli_alert_info("Building 'individuals' sheet...")
  }
  individuals_sheet <- .build_individuals_sheet(individual_cols, method)

  # Build features sheet if requested
  sheets <- list(individuals = individuals_sheet)

  if (include_features) {
    if (!return_data) {
      cli::cli_alert_info("Building 'features' sheet...")
    }
    features_sheet <- .build_features_sheet(con)
    sheets$features <- features_sheet
  }

  # Write to Excel only if not returning data
  if (!return_data) {
    cli::cli_alert_info("Writing template to: {output_file}")
    writexl::write_xlsx(sheets, path = output_file)

    cli::cli_alert_success("Template created successfully!")
    cat("\n")
    cli::cli_rule("Important Notes")
    cat("\n")
    cli::cli_alert_warning("Before filling the template:")
    cli::cli_ul(c(
      "Standardize taxonomic names using {.fn match_taxonomic_names} or the Shiny app {.fn launch_taxonomic_match_app()}",
      "Ensure {.field idtax_n} column has values for ALL individuals",
      "Keep {.field original_tax_name} for traceability"
    ))
    cat("\n")
    cli::cli_alert_info("Next steps:")
    cli::cli_ol(c(
      "Fill in the template with your data",
      "Use {.fn map_individual_columns} to map columns",
      "Use {.fn validate_individual_data} to check data quality",
      "Use {.fn import_individual_data} to import to database"
    ))
    cat("\n")
  }

  invisible(sheets)
}


#' Get Individual Column Definitions from Database
#'
#' Retrieves column definitions for the data_individuals table, including
#' data types, constraints, and method-specific requirements.
#'
#' @param con Database connection
#' @param method Optional method filter
#'
#' @return Tibble with column definitions
#' @keywords internal
.get_individual_columns_from_db <- function(con, method = NULL) {

  # Define core mandatory columns
  core_columns <- dplyr::tibble(
    column_name = c("plot_name", "tag", "idtax_n", "original_tax_name"),
    data_type = c("character", "numeric", "integer", "character"),
    required = c(TRUE, FALSE, TRUE, TRUE),
    description = c(
      "Plot name (must exist in database and be accessible to user)",
      "Individual tag/number (OPTIONAL - auto-generates sequential 1 to n per plot if missing)",
      "Taxonomy ID from taxa database (pre-matched using taxonomy tools)",
      "Original taxonomic name before standardization (for traceability)"
    ),
    validation = c(
      "Must match existing plot with user access",
      "If provided: numeric, unique per plot. If missing: auto-generated as 1, 2, 3, ... per plot",
      "Must exist in taxa database, cannot be NULL or 0",
      "Free text, keep original spelling/format"
    ),
    example = c("PLOT001", "1234 or leave empty", "12345", "Coula edulis")
  )

  # Optional columns
  optional_columns <- dplyr::tibble(
    column_name = c("herbarium_nbe_type", "herbarium_nbe_char", "multi_tiges_id"),
    data_type = c("character", "character", "character"),
    required = c(FALSE, FALSE, FALSE),
    description = c(
      "Herbarium specimen reference type (if specimen exists)",
      "Herbarium specimen reference code/number",
      "Multi-stem identifier (for trees with multiple stems)"
    ),
    validation = c(
      "Optional, free text",
      "Optional, free text",
      "Optional, links multiple stems of same individual. It indicates the tag of the individual to which it is a secondary stem"
    ),
    example = c("IRD plot 4181", "Lejoly 485", "11")
  )

  # Combine
  all_columns <- dplyr::bind_rows(core_columns, optional_columns)

  # Add method-specific notes
  if (!is.null(method)) {
    all_columns <- all_columns %>%
      dplyr::mutate(
        method_note = dplyr::case_when(
          column_name == "tag" & method %in% c("1ha-IRD", "Large") ~
            paste0("MANDATORY for method: ", method),
          TRUE ~ NA_character_
        )
      )
  }

  return(all_columns)
}


#' Get Trait Column Definitions from Database
#'
#' Retrieves trait definitions from traits_list() for the features template.
#'
#' @param con Database connection
#' @param common_traits_only Logical. If TRUE, only includes most common traits.
#'   If FALSE, includes all available traits.
#'
#' @return Tibble with trait definitions
#' @keywords internal
.get_trait_columns_from_db <- function(con, common_traits_only = TRUE) {

  # Get all traits from database
  all_traits <- traits_list()

  if (common_traits_only) {
    # Define most common traits for trees (using actual database trait names)
    common_trait_names <- c(
      "stem_diameter", "tree_height", "height_of_stem_diameter",
      "wood_specific_gravity", "leaf_area", "specific_leaf_area",
      "crown_width"
    )

    all_traits <- all_traits %>%
      dplyr::filter(trait %in% common_trait_names)
  }

  # Add validation info
  traits_with_validation <- all_traits %>%
    dplyr::mutate(
      validation_rule = dplyr::case_when(
        !is.na(minallowedvalue) & !is.na(maxallowedvalue) ~
          paste0("Range: ", minallowedvalue, " - ", maxallowedvalue,
                 ifelse(!is.na(expectedunit), paste0(" ", expectedunit), "")),
        !is.na(minallowedvalue) ~
          paste0("Min: ", minallowedvalue,
                 ifelse(!is.na(expectedunit), paste0(" ", expectedunit), "")),
        !is.na(maxallowedvalue) ~
          paste0("Max: ", maxallowedvalue,
                 ifelse(!is.na(expectedunit), paste0(" ", expectedunit), "")),
        TRUE ~ "No range restriction"
      ),
      example_value = dplyr::case_when(
        trait == "stem_diameter" ~ "25.4",
        trait == "tree_height" ~ "15.2",
        trait == "height_of_stem_diameter" ~ "1.3",
        trait == "wood_specific_gravity" ~ "0.65",
        trait == "leaf_area" ~ "125.5",
        trait == "specific_leaf_area" ~ "18.3",
        trait == "crown_width" ~ "8.5",
        valuetype == "numeric" ~ "10.5",
        valuetype == "character" ~ "value",
        TRUE ~ ""
      )
    )

  return(traits_with_validation)
}


#' Build Individuals Sheet Data
#'
#' Creates the data structure for the individuals sheet with headers,
#' descriptions, and example data.
#'
#' @param column_defs Column definitions from .get_individual_columns_from_db()
#' @param method Method type (optional)
#'
#' @return Tibble formatted for Excel export
#' @keywords internal
.build_individuals_sheet <- function(column_defs, method = NULL) {

  # Create column headers
  col_names <- column_defs$column_name

  # Create example rows (3 examples)
  example_row_1 <- column_defs$example
  example_row_2 <- dplyr::case_when(
    col_names == "plot_name" ~ "PLOT002",
    col_names == "tag" ~ "1235",
    col_names == "idtax_n" ~ "67890",
    col_names == "original_tax_name" ~ "Staudtia kamerunensis",
    col_names == "herbarium_nbe_type" ~ "IRD plot 45",
    col_names == "herbarium_nbe_char" ~ "IRD plot 45",
    col_names == "multi_tiges_id" ~ "",
    TRUE ~ ""
  )
  example_row_3 <- dplyr::case_when(
    col_names == "plot_name" ~ "PLOT001",
    col_names == "tag" ~ "1236",
    col_names == "idtax_n" ~ "11111",
    col_names == "original_tax_name" ~ "Guarea thompsonii",
    col_names == "herbarium_nbe_type" ~ "",
    col_names == "herbarium_nbe_char" ~ "",
    col_names == "multi_tiges_id" ~ "B",
    TRUE ~ ""
  )

  # Build sheet
  sheet_data <- dplyr::tibble(!!!base::setNames(as.list(col_names), col_names))
  sheet_data <- sheet_data %>%
    dplyr::add_row(!!!base::setNames(as.list(example_row_1), col_names)) %>%
    dplyr::add_row(!!!base::setNames(as.list(example_row_2), col_names)) %>%
    dplyr::add_row(!!!base::setNames(as.list(example_row_3), col_names))

  return(sheet_data)
}


#' Build Features Sheet Data
#'
#' Creates the data structure for the features sheet with trait columns.
#'
#' @param con Database connection
#'
#' @return Tibble formatted for Excel export
#' @keywords internal
.build_features_sheet <- function(con) {

  # Get trait definitions
  traits <- .get_trait_columns_from_db(con, common_traits_only = TRUE)

  # Core linking columns
  linking_cols <- c("plot_name", "tag", "census_date")

  # Trait columns
  trait_cols <- traits$trait

  # All columns
  all_cols <- c(linking_cols, trait_cols)

  # Create sheet with headers
  sheet_data <- dplyr::tibble(!!!setNames(as.list(all_cols), all_cols))

  # Add example rows
  example_row_1 <- c(
    "PLOT001",  # plot_name
    "1234",     # tag
    "2024-03-15",  # census_date
    traits$example_value  # trait values
  )

  example_row_2 <- c(
    "PLOT002",
    "1235",
    "2024-03-15",
    rep("", length(trait_cols))  # Empty trait values
  )

  sheet_data <- sheet_data %>%
    dplyr::add_row(!!!setNames(as.list(example_row_1), all_cols)) %>%
    dplyr::add_row(!!!setNames(as.list(example_row_2), all_cols))

  return(sheet_data)
}


#' Export Individual Template Info
#'
#' Exports a summary of template structure and requirements to console.
#'
#' @param con Database connection (optional)
#'
#' @return Invisibly returns template info structure
#' @export
print_individual_template_info <- function(con = NULL) {

  if (is.null(con)) {
    con <- call.mydb()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  cli::cli_h1("Individual Data Template Information")
  cat("\n")

  # Core columns
  cli::cli_h2("Core Individual Columns (Sheet 1)")
  core_cols <- .get_individual_columns_from_db(con)

  cli::cli_h3("Mandatory Columns")
  mandatory <- core_cols %>% dplyr::filter(required)
  for (i in 1:nrow(mandatory)) {
    cli::cli_alert_info("{.field {mandatory$column_name[i]}} ({mandatory$data_type[i]})")
    cli::cli_text("  {mandatory$description[i]}")
    cli::cli_text("  Example: {.val {mandatory$example[i]}}")
    cat("\n")
  }

  cli::cli_h3("Optional Columns")
  optional <- core_cols %>% dplyr::filter(!required)
  for (i in 1:nrow(optional)) {
    cli::cli_alert_info("{.field {optional$column_name[i]}} ({optional$data_type[i]})")
    cli::cli_text("  {optional$description[i]}")
    cat("\n")
  }

  # Feature columns
  cli::cli_h2("Individual Features (Sheet 2 - Optional)")
  cli::cli_alert_info("Common traits available:")
  traits <- .get_trait_columns_from_db(con, common_traits_only = TRUE)
  cli::cli_ul(traits$trait)
  cat("\n")
  cli::cli_alert_info("Use {.fn traits_list} to see all {nrow(traits_list())} available traits")
  cat("\n")

  # Workflow reminder
  cli::cli_h2("Workflow")
  cli::cli_ol(c(
    "Standardize taxonomy ({.fn match_taxonomic_names} or Shiny app)",
    "Generate template ({.fn get_individual_template})",
    "Fill in your data",
    "Map columns ({.fn map_individual_columns})",
    "Validate ({.fn validate_individual_data})",
    "Import ({.fn import_individual_data})"
  ))
  cat("\n")

  invisible(list(core = core_cols, traits = traits))
}
