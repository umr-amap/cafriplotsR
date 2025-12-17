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
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_code_preview_server <- function(id, filters, selected_plots, extraction_options,
                                     metadata_available, individuals_available, i18n) {
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

    # Render the code preview panel
    output$code_preview_panel <- shiny::renderUI({
      # Check if we have any data to show code for
      has_metadata <- isTRUE(metadata_available())
      has_individuals <- isTRUE(individuals_available())

      if (!has_metadata && !has_individuals) {
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
                code_sections$metadata
              )
            ),
            shiny::actionButton(
              ns("copy_metadata"),
              i18n()$t("Copy to clipboard"),
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "navigator.clipboard.writeText(`%s`).then(function() { alert('%s'); });",
                gsub("`", "\\`", code_sections$metadata),
                i18n()$t("Code copied!")
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
                code_sections$individuals
              )
            ),
            shiny::actionButton(
              ns("copy_individuals"),
              i18n()$t("Copy to clipboard"),
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "navigator.clipboard.writeText(`%s`).then(function() { alert('%s'); });",
                gsub("`", "\\`", code_sections$individuals),
                i18n()$t("Code copied!")
              )
            )
          )
        },

        # Combined code for full workflow
        if (has_metadata && has_individuals) {
          combined_code <- paste0(
            "# Complete workflow: Query and extract forest plot data\n",
            "library(CafriplotsR)\n\n",
            "# Step 1: Connect to database\n",
            "# (credentials will be requested interactively)\n\n",
            code_sections$metadata,
            "\n\n",
            "# Step 2: Review metadata and select plots of interest\n",
            "# View(metadata$metadata)  # Examine available plots\n\n",
            code_sections$individuals,
            "\n\n",
            "# Step 3: Access results\n",
            "# individuals$individuals  # Individual tree data\n",
            "# individuals$metadata     # Plot metadata"
          )

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
                combined_code
              )
            ),
            shiny::actionButton(
              ns("copy_combined"),
              i18n()$t("Copy complete script"),
              icon = shiny::icon("copy"),
              class = "btn-sm btn-outline-secondary",
              onclick = sprintf(
                "navigator.clipboard.writeText(`%s`).then(function() { alert('%s'); });",
                gsub("`", "\\`", combined_code),
                i18n()$t("Code copied!")
              )
            )
          )
        }
      )
    })

    return(NULL)
  })
}
