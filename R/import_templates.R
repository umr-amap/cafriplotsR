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
#'   - `"nested_plot"`: Plot nested in, or tiling part of, a plot already in the
#'     database (adds `parent_plot` and `parent_relation`)
#'   - `"minimal"`: Minimum required fields only
#'   - `"full"`: All available optional fields included
#' @param with_examples Logical: Include example rows with sample data? Default: TRUE
#' @param include_optional Logical: Include optional columns? Default: TRUE (ignored if template_type = "minimal")
#' @param n_examples Integer: Number of example rows to include (1 to 3). Default: 3
#' @param con Optional database connection (or pool). When supplied, the
#'   template follows the live database: the people columns are the
#'   `subplotype_list` features of valuetype `table_colnam`, plot
#'   characteristics are offered only if the database defines them, the
#'   hierarchy columns only if the plot hierarchy migration has been applied,
#'   and the example `method` / `country` values are real entries of
#'   `methodslist` / `table_countries`. Without it (or if the database cannot be
#'   queried) a static fallback is used.
#'
#' @details
#' Columns follow the three storage kinds of the import (see
#' [get_import_column_routing()]):
#'
#' **Required columns (lookups to `methodslist` / `table_countries`):**
#' - `plot_name`: Unique identifier for the plot (character)
#' - `method`: Survey method (must match [method_list()])
#' - `country`: Country name (must match [country_list()])
#'
#' **Flat columns (stored in `data_liste_plots`):**
#' - `ddlat`, `ddlon`: Decimal degrees
#' - `elevation`: Metres above sea level
#' - `locality_name`, `province`: Place names
#' - `date_y`, `date_m`, `date_d`: Survey date
#' - `subplot_shape` (full): Shape of the subplots
#' - `id_citation` (full): Numeric id of the source, from `query_citations()`
#' - `parent_plot`, `parent_relation` (nested_plot, full): the plot this one
#'   sits inside, by `plot_name`, and how (`"nested_subsample"` when the two
#'   overlap with different protocols - never sum them; `"block_member"` when
#'   this plot tiles part of the parent). The parent must already be in the
#'   database; import parents before children.
#'
#' **Features (stored in `data_liste_sub_plots`):**
#' - People (`valuetype = "table_colnam"`, e.g. `team_leader`,
#'   `principal_investigator`, `data_manager`, `additional_people`): names as
#'   they appear in `table_colnam`, comma-separated for several.
#' - `data_provider` (full): free text.
#' - `plot_area`, `vegetation_type`: when defined in `subplotype_list`.
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
#' # Follow the live database (real methods, countries and feature types)
#' template <- get_plot_metadata_template(con = call.mydb())
#'
#' # Get minimal template (required fields only)
#' minimal <- get_plot_metadata_template(template_type = "minimal")
#'
#' # Template for plots nested in existing plots
#' nested <- get_plot_metadata_template(template_type = "nested_plot")
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
get_plot_metadata_template <- function(template_type = c("permanent_plot", "transect",
                                                         "nested_plot", "minimal", "full"),
                                       with_examples = TRUE,
                                       include_optional = TRUE,
                                       n_examples = 3,
                                       con = NULL) {

  template_type <- match.arg(template_type)
  n_examples <- max(1L, min(3L, as.integer(n_examples)))

  ctx <- .get_template_context(con)

  required <- c("plot_name", "method", "country")
  location <- c("ddlat", "ddlon", "elevation", "locality_name")
  dates <- c("date_y", "date_m", "date_d")
  people <- ctx$people
  # plot characteristics are features: offer them only where the database has them
  characteristics <- intersect(c("plot_area", "vegetation_type"), ctx$features)
  hierarchy <- c("parent_plot", "parent_relation")

  col_names <- switch(
    template_type,
    minimal = required,
    full = c(required, location, "province", dates, people,
             intersect("data_provider", ctx$features),
             characteristics, "subplot_shape", "id_citation",
             if (ctx$hierarchy) hierarchy),
    nested_plot = c(required, location, dates, people, hierarchy),
    # permanent_plot, transect
    c(required, location, dates, people,
      if (include_optional) c("province", characteristics))
  )

  template <- dplyr::as_tibble(stats::setNames(
    lapply(col_names, .template_column_type),
    col_names
  ))

  if (with_examples) {
    examples <- .get_template_examples(template_type, n_examples, ctx)
    examples <- examples[, intersect(col_names, names(examples)), drop = FALSE]
    template <- dplyr::bind_rows(template, examples)
    template <- template[, col_names, drop = FALSE]
  }

  # Add column descriptions as attribute (pulled from database for subplot features)
  attr(template, "column_info") <- .get_column_descriptions_from_db(con)

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
#' @param con Optional database connection or pool; makes the template follow
#'   the live database (see [get_plot_metadata_template()]).
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
                                  template_type = c("permanent_plot", "transect", "nested_plot", "minimal", "full"),
                                  with_examples = TRUE,
                                  include_optional = TRUE,
                                  n_examples = 3,
                                  open_file = TRUE,
                                  con = NULL) {

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
    n_examples = n_examples,
    con = con
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


#' Column Prototype for a Template Column (Internal Helper)
#' @param col Character: column name
#' @keywords internal
.template_column_type <- function(col) {
  if (col %in% c("ddlat", "ddlon", "elevation", "plot_area")) return(numeric())
  if (col %in% c("date_y", "date_m", "date_d", "id_citation")) return(integer())
  character()
}


#' Live Database Context for Templates (Internal Helper)
#'
#' Collects what the template needs from the database, with a static fallback
#' for every piece so a template can always be produced.
#'
#' @param con Database connection, pool, or NULL
#' @return List: people (character), features (character), hierarchy (logical),
#'   methods (character), countries (character); the last two are NULL when
#'   unknown.
#' @keywords internal
.get_template_context <- function(con = NULL) {

  fallback <- list(
    people = c("team_leader", "principal_investigator", "data_manager",
               "additional_people"),
    # Static fallback: no database to contradict these
    features = c("data_provider", "plot_area", "vegetation_type"),
    hierarchy = TRUE,
    methods = NULL,
    countries = NULL
  )
  if (is.null(con)) return(fallback)

  tryCatch({
    subplot_info <- subplot_list(con)
    people <- subplot_info$type[subplot_info$valuetype == "table_colnam"]
    if (length(people) == 0) people <- fallback$people

    methods <- DBI::dbGetQuery(
      con, "SELECT method FROM methodslist WHERE method IS NOT NULL ORDER BY method"
    )$method
    countries <- DBI::dbGetQuery(
      con, "SELECT country FROM table_countries WHERE country IS NOT NULL ORDER BY country"
    )$country

    list(
      people = people,
      features = subplot_info$type,
      hierarchy = isTRUE(.has_plot_hierarchy(con)),
      methods = if (length(methods)) methods else NULL,
      countries = if (length(countries)) countries else NULL
    )
  }, error = function(e) {
    message("Note: Could not read the database for the template (", e$message,
            "). Using static columns.")
    fallback
  })
}


#' Get Template Examples (Internal Helper)
#'
#' Generate example data rows for templates. Real `method` / `country` values
#' are used when the database context supplies them.
#'
#' @param template_type Character: Type of template
#' @param n_examples Integer: Number of examples (1 to 3)
#' @param ctx List from `.get_template_context()` (optional)
#'
#' @return Tibble with example rows (a superset of the template columns; the
#'   caller keeps the ones it needs)
#' @keywords internal
.get_template_examples <- function(template_type, n_examples = 3, ctx = NULL) {

  idx <- seq_len(max(1L, min(3L, as.integer(n_examples))))

  pick <- function(real, fallback) {
    if (is.null(real) || length(real) == 0) return(fallback[idx])
    rep_len(utils::head(real, 3), 3)[idx]
  }
  methods <- pick(ctx$methods, rep("1ha permanent plot", 3))
  countries <- pick(ctx$countries, c("Cameroon", "Gabon", "Gabon"))

  if (template_type == "minimal") {
    return(dplyr::tibble(
      plot_name = c("PLOT001", "PLOT002", "PLOT003")[idx],
      method = methods,
      country = countries
    ))
  }

  examples <- dplyr::tibble(
    plot_name = c("DJA_PLOT_001", "LOPE_PLOT_023", "IVINDO_PLOT_012")[idx],
    method = methods,
    country = countries,
    ddlat = c(3.123, -0.567, 0.789)[idx],
    ddlon = c(12.456, 11.234, 12.890)[idx],
    elevation = c(650, 420, 380)[idx],
    locality_name = c("Dja Faunal Reserve", "Lope National Park", "Ivindo National Park")[idx],
    province = c("Sud", "Ogooue-Ivindo", "Ogooue-Ivindo")[idx],
    date_y = c(2022L, 2023L, 2023L)[idx],
    date_m = c(11L, 5L, 9L)[idx],
    date_d = c(12L, 8L, 20L)[idx],
    # Names must match table_colnam; several names are comma-separated
    team_leader = c("Jane Doe", "John Smith", "Marie Martin")[idx],
    principal_investigator = c("Jane Doe", "John Smith", "Marie Martin")[idx],
    data_manager = c("Alex Moreau", "Alex Moreau", "Sam Leroy")[idx],
    additional_people = c("Tom Brown, Lisa Green", "Mike Wilson", "Anna Garcia, Chris Lee")[idx],
    data_provider = c("Institution A", "Institution B", "Institution A")[idx],
    plot_area = c(1.0, 1.0, 0.5)[idx],
    vegetation_type = c("Tropical rainforest", "Semi-deciduous forest", "Dense evergreen forest")[idx],
    subplot_shape = NA_character_,
    id_citation = NA_integer_,
    parent_plot = NA_character_,
    parent_relation = NA_character_
  )

  if (template_type == "transect") {
    examples$plot_name <- c("TRANSECT_A", "TRANSECT_B", "TRANSECT_C")[idx]
    examples$locality_name <- c("Dja Reserve", "Lope NP", "Dzanga-Sangha")[idx]
  }

  if (template_type %in% c("nested_plot", "full")) {
    # Parents are NOT created by the import: they must already be in the
    # database, so the examples name an existing plot, and the first row is a
    # top-level plot with no parent.
    examples$plot_name <- c("DJA_PLOT_001", "DJA_PLOT_001_REGEN", "DJA_BLOCK_A2")[idx]
    examples$parent_plot <- c(NA, "DJA_PLOT_001", "DJA_BLOCK_A")[idx]
    examples$parent_relation <- c(NA, "nested_subsample", "block_member")[idx]
  }

  examples
}


#' Get Column Descriptions from Database (Internal Helper)
#'
#' Returns descriptions for template columns, pulling dynamically from database
#' via subplot_list() for subplot features. No hardcoding of feature lists.
#'
#' @return Named list with column descriptions and validation info
#' @keywords internal
.get_column_descriptions_from_db <- function(con = NULL) {

  # Try to get subplot feature info from database
  subplot_info <- tryCatch({
    subplot_list(con)
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
    subplot_shape = list(
      description = "Shape of the subplots (optional)",
      type = "character"
    ),
    id_citation = list(
      description = "Numeric id of the source in table_citations (optional); look it up with query_citations()",
      type = "integer",
      lookup_table = "table_citations"
    ),
    parent_plot = list(
      description = paste0(
        "plot_name of the plot this one sits inside (optional). The parent must ",
        "already be in the database - import parents before children. Requires parent_relation."
      ),
      type = "character",
      lookup_table = "data_liste_plots"
    ),
    parent_relation = list(
      description = paste0(
        "'nested_subsample' (overlaps the parent with a different protocol - never sum) or ",
        "'block_member' (tiles part of the parent). Required whenever parent_plot is given."
      ),
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
#' @param con Optional database connection or pool (see [get_plot_metadata_template()])
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
  function(template_type = c("permanent_plot", "transect", "nested_plot", "minimal", "full"),
                            con = NULL) {

  template_type <- match.arg(template_type)

  template <- get_plot_metadata_template(template_type, with_examples = FALSE, con = con)
  descriptions <- .get_column_descriptions_from_db(con)

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
