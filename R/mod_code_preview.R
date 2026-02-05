#' Code Preview Module - UI
#'
#' UI component for displaying equivalent R code
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_code_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Add rclipboard dependency
    rclipboard::rclipboardSetup(),
    shiny::uiOutput(ns("code_preview_panel"))
  )
}

#' Code Preview Module - Server
#'
#' Server logic for generating and displaying equivalent R code
#'
#' @param id Module namespace ID
#' @param filters Reactive returning named list of filter values
#' @param selected_plots Reactive returning vector of selected plot IDs
#' @param extraction_options Reactive returning named list of extraction options
#' @param metadata_available Reactive returning TRUE when metadata has been queried
#' @param individuals_available Reactive returning TRUE when individuals have been extracted
#' @param individual_features_options Reactive returning named list of individual features options (optional)
#' @param individual_features_available Reactive returning TRUE when individual features have been queried (optional)
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_code_preview_server <- function(id, filters, selected_plots, extraction_options,
                                     metadata_available, individuals_available, i18n,
                                     individual_features_options = NULL,
                                     individual_features_available = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate code for metadata query
    generate_metadata_code <- function(filters) {
      # Build filter arguments
      args <- c()

      # Text/character filters
      if (!is.null(filters$country) && length(filters$country) > 0) {
        args <- c(args, sprintf('  country = c(%s)',
          paste0('"', filters$country, '"', collapse = ", ")))
      }

      if (!is.null(filters$plot_name) && length(filters$plot_name) > 0) {
        args <- c(args, sprintf('  plot_name = c(%s)',
          paste0('"', filters$plot_name, '"', collapse = ", ")))
      }

      if (!is.null(filters$locality_name) && nzchar(filters$locality_name)) {
        args <- c(args, sprintf('  locality_name = "%s"', filters$locality_name))
      }

      if (!is.null(filters$method) && length(filters$method) > 0) {
        args <- c(args, sprintf('  method = c(%s)',
          paste0('"', filters$method, '"', collapse = ", ")))
      }

      if (!is.null(filters$tag) && nzchar(filters$tag)) {
        args <- c(args, sprintf('  tag = "%s"', filters$tag))
      }

      # Numeric ID filters
      if (!is.null(filters$id_plot)) {
        args <- c(args, sprintf('  id_plot = %s', filters$id_plot))
      }

      if (!is.null(filters$id_individual)) {
        args <- c(args, sprintf('  id_individual = %s', filters$id_individual))
      }

      if (!is.null(filters$id_tax)) {
        args <- c(args, sprintf('  id_tax = %s', filters$id_tax))
      }

      if (!is.null(filters$id_specimen)) {
        args <- c(args, sprintf('  id_specimen = %s', filters$id_specimen))
      }

      # Boolean filters
      if (!is.null(filters$exact_match) && isTRUE(filters$exact_match)) {
        args <- c(args, '  exact_match = TRUE')
      }

      # Always add metadata-specific options
      args <- c(args, '  extract_individuals = FALSE')

      # Build the code
      if (length(args) == 1) {
        # Only extract_individuals = FALSE, no filters
        code <- paste0(
          "# Query plot metadata\n",
          "metadata <- query_plots(\n",
          args[1], "\n",
          ")"
        )
      } else {
        code <- paste0(
          "# Query plot metadata\n",
          "metadata <- query_plots(\n",
          paste(args, collapse = ",\n"),
          "\n)"
        )
      }

      return(code)
    }

    # Generate code for individual extraction
    generate_individuals_code <- function(plot_ids, options, filters = NULL, use_metadata_ref = FALSE) {
      args <- c()

      # Plot IDs - use metadata reference if available and multiple plots
      if (use_metadata_ref && length(plot_ids) > 1) {
        # Suggest using metadata output instead of long vector
        args <- c(args, '  id_plot = metadata$metadata$id_liste_plots')
      } else if (length(plot_ids) == 1) {
        args <- c(args, sprintf('  id_plot = %s', plot_ids))
      } else {
        args <- c(args, sprintf('  id_plot = c(%s)', paste(plot_ids, collapse = ", ")))
      }

      # Tag filter (if provided)
      if (!is.null(filters$tag) && nzchar(filters$tag)) {
        args <- c(args, sprintf('  tag = "%s"', filters$tag))
      }

      # Always extracting individuals
      args <- c(args, '  extract_individuals = TRUE')

      # Output style (only if not default)
      if (!is.null(options$output_style) && options$output_style != "auto") {
        args <- c(args, sprintf('  output_style = "%s"', options$output_style))
      }

      # Census options
      if (!is.null(options$census_strategy) && options$census_strategy != "last") {
        args <- c(args, sprintf('  census_strategy = "%s"', options$census_strategy))
      }

      if (isTRUE(options$show_multiple_census)) {
        args <- c(args, '  show_multiple_census = TRUE')
      }

      # Data organization
      if (isTRUE(options$concatenate_stem)) {
        args <- c(args, '  concatenate_stem = TRUE')
      }

      if (!isTRUE(options$remove_ids)) {
        args <- c(args, '  remove_ids = FALSE')
      }

      if (!isTRUE(options$remove_obs_with_issue)) {
        args <- c(args, '  remove_obs_with_issue = FALSE')
      }

      if (isTRUE(options$include_issue)) {
        args <- c(args, '  include_issue = TRUE')
      }

      # Additional extraction options
      if (!isTRUE(options$extract_traits)) {
        args <- c(args, '  extract_traits = FALSE')
      }

      if (!isTRUE(options$extract_individual_features)) {
        args <- c(args, '  extract_individual_features = FALSE')
      }

      if (!isTRUE(options$extract_subplot_features)) {
        args <- c(args, '  extract_subplot_features = FALSE')
      }

      if (isTRUE(options$traits_to_genera)) {
        args <- c(args, '  traits_to_genera = TRUE')
      }

      # Build the code with helpful comment if using metadata reference
      if (use_metadata_ref && length(plot_ids) > 1) {
        code <- paste0(
          "# Extract individual tree data from selected plots\n",
          "# Note: Column name might be 'plot_id' instead of 'id_liste_plots' depending on output_style\n",
          "individuals <- query_plots(\n",
          paste(args, collapse = ",\n"),
          "\n)"
        )
      } else {
        code <- paste0(
          "# Extract individual tree data from selected plots\n",
          "individuals <- query_plots(\n",
          paste(args, collapse = ",\n"),
          "\n)"
        )
      }

      return(code)
    }

    # Generate code for individual features query
    generate_individual_features_code <- function(feat_opts, output_style = NULL) {
      args <- c()

      # Individual IDs - reference to extracted data
      # Path depends on output_style: 'full' uses $extract, others use $individuals
      if (!is.null(output_style) && output_style == "full") {
        args <- c(args, '  individual_ids = unique(individuals$extract$id_n)')
      } else {
        args <- c(args, '  individual_ids = unique(individuals$individuals$id_n)')
      }

      # Trait IDs
      if (!is.null(feat_opts$trait_ids) && length(feat_opts$trait_ids) > 0) {
        args <- c(args, sprintf('  trait_ids = c(%s)', paste(feat_opts$trait_ids, collapse = ", ")))
      } else {
        args <- c(args, '  trait_ids = NULL  # All traits')
      }

      # Format
      if (!is.null(feat_opts$format) && feat_opts$format != "wide") {
        args <- c(args, sprintf('  format = "%s"', feat_opts$format))
      }

      # Include multi census
      if (isTRUE(feat_opts$include_multi_census)) {
        args <- c(args, '  include_multi_census = TRUE')
      }

      # Census strategy (only if not default)
      if (!is.null(feat_opts$census_strategy) && feat_opts$census_strategy != "last") {
        args <- c(args, sprintf('  census_strategy = "%s"', feat_opts$census_strategy))
      }

      # Include metadata
      if (isTRUE(feat_opts$include_metadata)) {
        args <- c(args, '  include_metadata = TRUE')
      }

      # Remove issues
      if (!isTRUE(feat_opts$remove_issues)) {
        args <- c(args, '  remove_issues = FALSE')
      }

      # Build the code
      code <- paste0(
        "# Query individual-level features from extracted individuals\n",
        "individual_features <- query_individual_features(\n",
        paste(args, collapse = ",\n"),
        "\n)"
      )

      return(code)
    }

    # Render the code preview panel
    output$code_preview_panel <- shiny::renderUI({
      # Check if we have any data to show code for
      has_metadata <- isTRUE(metadata_available())
      has_individuals <- isTRUE(individuals_available())
      has_individual_features <- !is.null(individual_features_available) &&
                                 isTRUE(individual_features_available())

      if (!has_metadata && !has_individuals && !has_individual_features) {
        return(NULL)
      }

      # Generate code sections
      code_sections <- list()

      # Metadata code
      if (has_metadata) {
        current_filters <- filters()
        metadata_code <- generate_metadata_code(current_filters)
        code_sections$metadata <- metadata_code
      }

      # Individuals code
      if (has_individuals) {
        current_plots <- selected_plots()
        current_options <- extraction_options()
        current_filters <- filters()
        individuals_code <- generate_individuals_code(current_plots, current_options, current_filters, use_metadata_ref = has_metadata)
        code_sections$individuals <- individuals_code
      }

      # Individual features code
      if (has_individual_features) {
        current_feat_opts <- individual_features_options()
        current_options <- extraction_options()
        if (isTRUE(current_feat_opts$enabled)) {
          individual_features_code <- generate_individual_features_code(
            current_feat_opts,
            output_style = current_options$output_style
          )
          code_sections$individual_features <- individual_features_code
        }
      }

      # Build UI
      shiny::wellPanel(
        style = "background-color: #f5f5f5; border: 1px solid #e0e0e0;",
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::h5(
              shiny::icon("code"),
              " ",
              i18n()$t("Equivalent R Code")
            ),
            shiny::p(
              class = "text-muted",
              style = "font-size: 0.9em;",
              i18n()$t("Use this code to reproduce the same query programmatically with query_plots()")
            )
          )
        ),

        # Metadata code section
        if (has_metadata) {
          shiny::tagList(
            shiny::h6(
              shiny::icon("table"),
              " ",
              i18n()$t("Metadata Query")
            ),
            shiny::tags$pre(
              style = "background-color: #282c34; color: #abb2bf; padding: 15px; border-radius: 5px; overflow-x: auto; font-family: 'Fira Code', 'Consolas', monospace; font-size: 0.85em;",
              shiny::tags$code(
                id = ns("code_metadata"),
                code_sections$metadata
              )
            ),
            rclipboard::rclipButton(
              ns("copy_metadata"),
              i18n()$t("Copy to clipboard"),
              code_sections$metadata,
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "copyCodeToClipboard_%s('%s')",
                gsub("-", "_", id),
                ns("code_metadata")
              )
            ),
            shiny::br(),
            shiny::br()
          )
        },

        # Individuals code section
        if (has_individuals) {
          shiny::tagList(
            shiny::h6(
              shiny::icon("tree"),
              " ",
              i18n()$t("Individual Extraction")
            ),
            shiny::tags$pre(
              style = "background-color: #282c34; color: #abb2bf; padding: 15px; border-radius: 5px; overflow-x: auto; font-family: 'Fira Code', 'Consolas', monospace; font-size: 0.85em;",
              shiny::tags$code(
                id = ns("code_individuals"),
                code_sections$individuals
              )
            ),
            rclipboard::rclipButton(
              ns("copy_individuals"),
              i18n()$t("Copy to clipboard"),
              code_sections$individuals,
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "copyCodeToClipboard_%s('%s')",
                gsub("-", "_", id),
                ns("code_individuals")
              )
            ),
            shiny::br(),
            shiny::br()
          )
        },

        # Individual features code section
        if (has_individual_features) {
          shiny::tagList(
            shiny::h6(
              shiny::icon("microscope"),
              " ",
              i18n()$t("Individual Features Query")
            ),
            shiny::tags$pre(
              style = "background-color: #282c34; color: #abb2bf; padding: 15px; border-radius: 5px; overflow-x: auto; font-family: 'Fira Code', 'Consolas', monospace; font-size: 0.85em;",
              shiny::tags$code(
                id = ns("code_individual_features"),
                code_sections$individual_features
              )
            ),
            rclipboard::rclipButton(
              ns("copy_individual_features"),
              i18n()$t("Copy to clipboard"),
              code_sections$individual_features,
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "copyCodeToClipboard_%s('%s')",
                gsub("-", "_", id),
                ns("code_individual_features")
              )
            )
          )
        },

        # Combined code for full workflow
        if (has_metadata && has_individuals) {
          # Build combined code with optional individual features
          combined_parts <- c(
            "# Complete workflow: Query and extract forest plot data",
            "library(CafriplotsR)\n",
            "# Step 1: Connect to database",
            "# (credentials will be requested interactively)\n",
            code_sections$metadata,
            "\n# Step 2: Review metadata and select plots of interest",
            "# View(metadata$metadata)  # Examine available plots\n",
            code_sections$individuals
          )

          if (has_individual_features) {
            combined_parts <- c(
              combined_parts,
              "\n# Step 3: Query individual-level features (optional)",
              code_sections$individual_features,
              "\n# Step 4: Access results",
              "# individuals$individuals     # Individual tree data",
              "# individuals$metadata        # Plot metadata",
              "# individual_features         # Individual-level feature measurements"
            )
          } else {
            combined_parts <- c(
              combined_parts,
              "\n# Step 3: Access results",
              "# individuals$individuals  # Individual tree data",
              "# individuals$metadata     # Plot metadata"
            )
          }

          combined_code <- paste(combined_parts, collapse = "\n")

          shiny::tagList(
            shiny::hr(),
            shiny::h6(
              shiny::icon("file-code"),
              " ",
              i18n()$t("Complete Workflow Script")
            ),
            shiny::p(
              class = "text-muted",
              style = "font-size: 0.85em;",
              i18n()$t("Full script combining metadata query and individual extraction")
            ),
            shiny::tags$pre(
              style = "background-color: #282c34; color: #abb2bf; padding: 15px; border-radius: 5px; overflow-x: auto; font-family: 'Fira Code', 'Consolas', monospace; font-size: 0.85em; max-height: 400px;",
              shiny::tags$code(
                id = ns("code_combined"),
                combined_code
              )
            ),
            rclipboard::rclipButton(
              ns("copy_combined"),
              i18n()$t("Copy complete script"),
              combined_code,
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "copyCodeToClipboard_%s('%s')",
                gsub("-", "_", id),
                ns("code_combined")
              )
            )
          )
        }
      )
    })

    return(NULL)
  })
}
