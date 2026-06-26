# Plot Metadata Import Templates
#
# Functions for generating standardized templates for importing plot metadata
# Reuses existing lookup functions: method_list(), country_list(), subplot_list()

#' Get Plot Metadata Template
#'
#' Generate a template data frame with standard column names and example data.
#' Users can download this template, fill it with their data, and import it
#' using the plot metadata import functions.
#'
#' @param template_type Character: Type of template to generate
#'   - `"permanent_plot"`: Standard permanent forest plot (default)
#'   - `"transect"`: Linear transect survey
#'   - `"minimal"`: Minimum required fields only
#'   - `"full"`: All available optional fields included
#' @param with_examples Logical: Include example rows with sample data? Default: TRUE
#' @param include_optional Logical: Include optional columns? Default: TRUE (ignored if template_type = "minimal")
#' @param n_examples Integer: Number of example rows to include. Default: 3
#'
#' @details
#' The template includes standard column names that match the database schema:
#'
#' **Required columns:**
#' - `plot_name`: Unique identifier for the plot (character)
#' - `method`: Survey method (must match [method_list()])
#' - `country`: Country name (must match [country_list()])
#'
#' **Location columns:**
#' - `ddlat`: Latitude in decimal degrees (-90 to 90)
#' - `ddlon`: Longitude in decimal degrees (-180 to 180)
#' - `elevation`: Elevation in meters above sea level
#' - `locality_name`: Name of the locality or site
#' - `province`: Province or state name
#'
#' **Date columns:**
#' - `date_y`: Year of survey (YYYY)
#' - `date_m`: Month of survey (1-12)
#' - `date_d`: Day of survey (1-31)
#'
#' **People columns (subplot features with valuetype='table_colnam'):**
#' - Dynamically determined from database via [subplot_list()]
#' - Common examples: team_leader, principal_investigator, data_manager, etc.
#' - Multiple names can be comma-separated
#'
#' **Plot characteristics:**
#' - `plot_area`: Plot area in hectares
#' - Other characteristics as defined in database
#'
#' Column descriptions and validation rules are pulled from the database
#' via [subplot_list()] for subplot features.
#'
#' @return A tibble with template structure. If `with_examples = TRUE`, includes
#'   example rows with realistic sample data.
#'
#' @examples
#' \dontrun{
#' # Get standard template with examples
#' template <- get_plot_metadata_template()
#' View(template)
#'
#' # Get minimal template (required fields only)
#' minimal <- get_plot_metadata_template(template_type = "minimal")
#'
#' # Get full template without examples (for large imports)
#' empty_full <- get_plot_metadata_template(
#'   template_type = "full",
#'   with_examples = FALSE
#' )
#'
#' # Export to Excel for filling
#' export_plot_template("my_plots_template.xlsx", template_type = "permanent_plot")
#' }
#'
#' @seealso
#' [export_plot_template()] to save template as Excel file
#' [method_list()] to see available methods
#' [country_list()] to see valid countries
#' [subplot_list()] to see available subplot features with validation rules
#'
#' @export
get_plot_metadata_template <- function(template_type = c("permanent_plot", "transect", "minimal", "full"),
                                       with_examples = TRUE,
                                       include_optional = TRUE,
                                       n_examples = 3) {

  template_type <- match.arg(template_type)

  # Define column structure based on template type
  if (template_type == "minimal") {
    columns <- list(
      plot_name = character(),
      method = character(),
      country = character()
    )
  } else if (template_type == "full") {
    columns <- list(
      # Required
      plot_name = character(),
      method = character(),
      country = character(),
      # Location
      ddlat = numeric(),
      ddlon = numeric(),
      elevation = numeric(),
      locality_name = character(),
      province = character(),
      # Dates
      date_y = integer(),
      date_m = integer(),
      date_d = integer(),
      # People
      team_leader = character(),
      principal_investigator = character(),
      data_manager = character(),
      additional_people = character(),
      data_provider = character(),
      # Plot characteristics
      plot_area = numeric(),
      vegetation_type = character(),
      forest_type = character()
    )
  } else {
    # Standard templates (permanent_plot, transect)
    columns <- list(
      # Required
      plot_name = character(),
      method = character(),
      country = character(),
      # Location
      ddlat = numeric(),
      ddlon = numeric(),
      elevation = numeric(),
      locality_name = character(),
      # Dates
      date_y = integer(),
      date_m = integer(),
      date_d = integer(),
      # People
      team_leader = character(),
      principal_investigator = character(),
      data_manager = character(),
      additional_people = character()
    )

    if (include_optional) {
      columns$province <- character()
      columns$plot_area <- numeric()
      columns$vegetation_type <- character()
    }
  }

  # Create empty tibble with correct types
  template <- dplyr::tibble(!!!columns)

  # Add examples if requested
  if (with_examples) {
    examples <- .get_template_examples(template_type, n_examples)
    template <- dplyr::bind_rows(template, examples)
  }

  # Add column descriptions as attribute (pulled from database for subplot features)
  attr(template, "column_info") <- .get_column_descriptions_from_db()

  return(template)
}


#' Export Plot Metadata Template to Excel
#'
#' Export a plot metadata template as an Excel file that users can fill and import.
#' The Excel file includes formatted columns and example data to guide users.
#'
#' @param file_path Character: Path where the Excel file should be saved.
#'   Should end with .xlsx
#' @param template_type Character: Type of template (see [get_plot_metadata_template()])
#' @param with_examples Logical: Include example rows? Default: TRUE
#' @param include_optional Logical: Include optional columns? Default: TRUE
#' @param n_examples Integer: Number of example rows. Default: 3
#' @param open_file Logical: Open the file after creating it? Default: TRUE (Windows only)
#'
#' @return Invisibly returns the template data frame
#'
#' @examples
#' \dontrun{
#' # Export standard template
#' export_plot_template("my_plots.xlsx")
#'
#' # Export minimal template without examples
#' export_plot_template(
#'   "plots_minimal.xlsx",
#'   template_type = "minimal",
#'   with_examples = FALSE
#' )
#'
#' # Export full template for large import
#' export_plot_template(
#'   "plots_full.xlsx",
#'   template_type = "full",
#'   n_examples = 1
#' )
#' }
#'
#' @export
export_plot_template <- function(file_path,
                                  template_type = c("permanent_plot", "transect", "minimal", "full"),
                                  with_examples = TRUE,
                                  include_optional = TRUE,
                                  n_examples = 3,
                                  open_file = TRUE) {

  template_type <- match.arg(template_type)

  # Validate file path
  if (!grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    cli::cli_alert_warning("File path does not end with .xlsx - adding extension")
    file_path <- paste0(file_path, ".xlsx")
  }

  # Get template
  template <- get_plot_metadata_template(
    template_type = template_type,
    with_examples = with_examples,
    include_optional = include_optional,
    n_examples = n_examples
  )

  # Check if writexl is available
  if (!requireNamespace("writexl", quietly = TRUE)) {
    cli::cli_alert_danger("Package {.pkg writexl} required for Excel export")
    cli::cli_alert_info("Install with: install.packages('writexl')")
    stop("writexl package required")
  }

  # Export to Excel
  tryCatch({
    writexl::write_xlsx(template, file_path)
    cli::cli_alert_success("Template exported to {.file {file_path}}")
    cli::cli_alert_info("Template type: {.val {template_type}}")
    cli::cli_alert_info("Rows: {.val {nrow(template)}} (including {n_examples} example rows)")

    # Open file if requested (Windows only)
    if (open_file && .Platform$OS.type == "windows") {
      tryCatch({
        shell.exec(file_path)
      }, error = function(e) {
        cli::cli_alert_warning("Could not open file automatically")
      })
    }

  }, error = function(e) {
    cli::cli_alert_danger("Failed to export template: {e$message}")
    stop(e)
  })

  invisible(template)
}


#' Get Template Examples (Internal Helper)
#'
#' Generate example data rows for templates
#'
#' @param template_type Character: Type of template
#' @param n_examples Integer: Number of examples
#'
#' @return Tibble with example rows
#' @keywords internal
.get_template_examples <- function(template_type, n_examples = 3) {

  if (template_type == "minimal") {
    examples <- dplyr::tibble(
      plot_name = c("PLOT001", "PLOT002", "PLOT003")[1:n_examples],
      method = c("1ha permanent plot", "transect 500m", "1ha permanent plot")[1:n_examples],
      country = c("Cameroon", "Gabon", "Democratic Republic of the Congo")[1:n_examples]
    )
  } else if (template_type == "transect") {
    examples <- dplyr::tibble(
      plot_name = c("TRANSECT_A", "TRANSECT_B", "TRANSECT_C")[1:n_examples],
      method = rep("transect 500m", n_examples),
      country = c("Cameroon", "Gabon", "Central African Republic")[1:n_examples],
      ddlat = c(3.845, -0.422, 4.123)[1:n_examples],
      ddlon = c(11.523, 9.876, 18.456)[1:n_examples],
      elevation = c(450, 320, 580)[1:n_examples],
      locality_name = c("Dja Reserve", "Lope NP", "Dzanga-Sangha")[1:n_examples],
      date_y = c(2023, 2023, 2024)[1:n_examples],
      date_m = c(6, 8, 3)[1:n_examples],
      date_d = c(15, 22, 10)[1:n_examples],
      team_leader = c("John Doe", "Jane Smith", "Bob Wilson")[1:n_examples],
      principal_investigator = c("Dr. Sarah Johnson", "Prof. Marie Dubois", "Dr. Paul Nguema")[1:n_examples],
      data_manager = c("Alice Brown", "Tom Jones", "Emma Davis")[1:n_examples],
      additional_people = c("Peter Pan, Mary Jane", "Chris Lee, Ana Garcia", "David Kim")[1:n_examples]
    )
  } else {
    # permanent_plot or full
    examples <- dplyr::tibble(
      plot_name = c("DJA_PLOT_001", "LOPE_PLOT_023", "IVINDO_PLOT_012")[1:n_examples],
      method = rep("1ha permanent plot", n_examples),
      country = c("Cameroon", "Gabon", "Gabon")[1:n_examples],
      ddlat = c(3.123, -0.567, 0.789)[1:n_examples],
      ddlon = c(12.456, 11.234, 12.890)[1:n_examples],
      elevation = c(650, 420, 380)[1:n_examples],
      locality_name = c("Dja Faunal Reserve", "Lope National Park", "Ivindo National Park")[1:n_examples],
      date_y = c(2022, 2023, 2023)[1:n_examples],
      date_m = c(11, 5, 9)[1:n_examples],
      date_d = c(12, 8, 20)[1:n_examples],
      team_leader = c("Dr. Marie Blanc", "John Doe", "Dr. Jean Nguema")[1:n_examples],
      principal_investigator = c("Prof. Gilles Dauby", "Dr. Sarah White", "Prof. Pierre Dupont")[1:n_examples],
      data_manager = c("Alice Martin", "Bob Johnson", "Claire Leblanc")[1:n_examples],
      additional_people = c("Tom Smith, Lisa Brown", "Mike Wilson", "Anna Garcia, Chris Lee")[1:n_examples]
    )

    if (template_type == "full") {
      examples$province <- c("Sud", "Ogooue-Ivindo", "Ogooue-Ivindo")[1:n_examples]
      examples$data_provider <- c("IRD", "ANPN", "CENAREST")[1:n_examples]
      examples$plot_area <- c(1.0, 1.0, 0.5)[1:n_examples]
      examples$vegetation_type <- c("Tropical rainforest", "Semi-deciduous forest", "Dense evergreen forest")[1:n_examples]
      examples$forest_type <- c("Terra firme", "Transition", "Terra firme")[1:n_examples]
    }
  }

  return(examples)
}


#' Get Column Descriptions from Database (Internal Helper)
#'
#' Returns descriptions for template columns, pulling dynamically from database
#' via subplot_list() for subplot features. No hardcoding of feature lists.
#'
#' @return Named list with column descriptions and validation info
#' @keywords internal
.get_column_descriptions_from_db <- function() {

  # Try to get subplot feature info from database
  subplot_info <- tryCatch({
    subplot_list()
  }, error = function(e) {
    cli::cli_alert_warning("Could not retrieve subplot features from database")
    NULL
  })

  # Base descriptions for flat columns (stored directly in data_liste_plots)
  descriptions <- list(
    plot_name = list(
      description = "Unique identifier for the plot (required, no duplicates)",
      type = "character",
      required = TRUE
    ),
    method = list(
      description = "Survey method - must match method_list() (required)",
      type = "character",
      required = TRUE,
      lookup_table = "methodslist"
    ),
    country = list(
      description = "Country name - must match country_list() (required)",
      type = "character",
      required = TRUE,
      lookup_table = "table_countries"
    ),
    ddlat = list(
      description = "Latitude in decimal degrees",
      type = "numeric",
      min = -90,
      max = 90
    ),
    ddlon = list(
      description = "Longitude in decimal degrees",
      type = "numeric",
      min = -180,
      max = 180
    ),
    elevation = list(
      description = "Elevation in meters above sea level",
      type = "numeric"
    ),
    locality_name = list(
      description = "Name of the locality or site",
      type = "character"
    ),
    province = list(
      description = "Province or state name",
      type = "character"
    ),
    date_y = list(
      description = "Year of survey (YYYY format, e.g., 2023)",
      type = "integer",
      min = 1900,
      max = lubridate::year(Sys.Date())
    ),
    date_m = list(
      description = "Month of survey",
      type = "integer",
      min = 1,
      max = 12
    ),
    date_d = list(
      description = "Day of survey",
      type = "integer",
      min = 1,
      max = 31
    ),
    plot_area = list(
      description = "Plot area in hectares (e.g., 1.0 for 1 hectare)",
      type = "numeric"
    ),
    vegetation_type = list(
      description = "Type of vegetation (e.g., 'Tropical rainforest')",
      type = "character"
    ),
    forest_type = list(
      description = "Forest type classification (e.g., 'Terra firme', 'Flooded')",
      type = "character"
    )
  )

  # Dynamically add ALL subplot features from database
  if (!is.null(subplot_info) && nrow(subplot_info) > 0) {

    for (i in 1:nrow(subplot_info)) {
      feature_type <- subplot_info$type[i]

      # Build description from database fields
      desc_info <- list(
        description = subplot_info$typedescription[i] %||% paste0("Subplot feature: ", feature_type),
        type = subplot_info$valuetype[i] %||% "character",
        is_subplot_feature = TRUE
      )

      # Add validation info if available
      if (!is.na(subplot_info$minallowedvalue[i])) {
        desc_info$min <- subplot_info$minallowedvalue[i]
      }
      if (!is.na(subplot_info$maxallowedvalue[i])) {
        desc_info$max <- subplot_info$maxallowedvalue[i]
      }
      if (!is.na(subplot_info$expectedunit[i]) && subplot_info$expectedunit[i] != "none") {
        desc_info$expectedunit <- subplot_info$expectedunit[i]
      }

      # Special handling for table_colnam type (people columns)
      if (subplot_info$valuetype[i] == "table_colnam") {
        desc_info$description <- paste0(
          desc_info$description,
          " (separate multiple names with commas)"
        )
        desc_info$lookup_table <- "table_colnam"
      }

      descriptions[[feature_type]] <- desc_info
    }
  }

  return(descriptions)
}


#' Print Template Column Information
#'
#' Display detailed information about template columns including descriptions
#' and validation rules pulled from the database
#'
#' @param template_type Character: Type of template (see [get_plot_metadata_template()])
#'
#' @return Invisibly returns a data frame with column information
#'
#' @examples
#' \dontrun{
#' # Show information about permanent plot template
#' print_template_info("permanent_plot")
#'
#' # Show minimal template info
#' print_template_info("minimal")
#' }
#'
#' @export
print_template_info <- 
  function(template_type = c("permanent_plot", "transect", "minimal", "full")) {

  template_type <- match.arg(template_type)

  template <- get_plot_metadata_template(template_type, with_examples = FALSE)
  descriptions <- .get_column_descriptions_from_db()

  cli::cli_h1("Plot Metadata Template: {.val {template_type}}")

  cli::cli_h2("Required Columns (must be provided)")
  cli::cli_ul()
  for (col in c("plot_name", "method", "country")) {
    if (col %in% names(template)) {
      desc_info <- descriptions[[col]]
      cli::cli_li("{.field {col}}: {desc_info$description}")
    }
  }
  cli::cli_end()

  # Separate flat columns and subplot features
  flat_cols <- c()
  feature_cols <- c()

  for (col in setdiff(names(template), c("plot_name", "method", "country"))) {
    if (!is.null(descriptions[[col]]$is_subplot_feature) && descriptions[[col]]$is_subplot_feature) {
      feature_cols <- c(feature_cols, col)
    } else {
      flat_cols <- c(flat_cols, col)
    }
  }

  if (length(flat_cols) > 0) {
    cli::cli_h2("Plot Metadata Columns (stored directly)")
    cli::cli_ul()
    for (col in flat_cols) {
      desc_info <- descriptions[[col]]
      desc_text <- desc_info$description

      # Add validation info if available
      if (!is.null(desc_info$min) && !is.null(desc_info$max)) {
        desc_text <- paste0(desc_text, " [range: ", desc_info$min, " to ", desc_info$max, "]")
      }

      cli::cli_li("{.field {col}}: {desc_text}")
    }
    cli::cli_end()
  }

  if (length(feature_cols) > 0) {
    cli::cli_h2("Subplot Feature Columns (normalized storage)")
    cli::cli_ul()
    for (col in feature_cols) {
      desc_info <- descriptions[[col]]
      desc_text <- desc_info$description

      # Add type and validation info
      type_info <- paste0("[type: ", desc_info$type, "]")
      if (!is.null(desc_info$min) && !is.null(desc_info$max)) {
        type_info <- paste0(type_info, " [range: ", desc_info$min, " to ", desc_info$max, "]")
      }
      if (!is.null(desc_info$expectedunit)) {
        type_info <- paste0(type_info, " [unit: ", desc_info$expectedunit, "]")
      }

      cli::cli_li("{.field {col}}: {desc_text} {type_info}")
    }
    cli::cli_end()
  }

  cli::cli_h2("Usage")
  cli::cli_alert_info("Export template: export_plot_template('my_file.xlsx', template_type = '{template_type}')")
  cli::cli_alert_info("List valid methods: method_list()")
  cli::cli_alert_info("List valid countries: country_list()")
  cli::cli_alert_info("List all subplot features: subplot_list()")

  # Create info table
  info_df <- dplyr::tibble(
    column = names(template),
    type = sapply(names(template), function(x) descriptions[[x]]$type %||% "character"),
    required = sapply(names(template), function(x) ifelse(descriptions[[x]]$required %||% FALSE, "Yes", "No")),
    is_subplot_feature = sapply(names(template), function(x) ifelse(descriptions[[x]]$is_subplot_feature %||% FALSE, "Yes", "No")),
    description = sapply(names(template), function(x) descriptions[[x]]$description %||% ""),
    validation = sapply(names(template), function(x) {
      desc <- descriptions[[x]]
      if (!is.null(desc$min) && !is.null(desc$max)) {
        paste0(desc$min, " to ", desc$max)
      } else if (!is.null(desc$lookup_table)) {
        paste0("Lookup: ", desc$lookup_table)
      } else {
        ""
      }
    })
  )

  invisible(info_df)
}
