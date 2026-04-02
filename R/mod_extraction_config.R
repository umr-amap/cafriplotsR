#' Extraction Configuration Module - UI
#'
#' UI component for configuring individual extraction options
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_extraction_config_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    # All UI elements rendered dynamically for i18n support
    shiny::uiOutput(ns("config_ui"))
  )
}

#' Extraction Configuration Module - Server
#'
#' Server logic for extraction configuration
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing selected plot IDs
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return A reactive list containing:
#'   - options: Named list of extraction options
#'   - execute_trigger: Reactive counter that increments on execute
#'
#' @keywords internal
#' @export
mod_extraction_config_server <- function(id, selected_plots, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    execute_counter <- shiny::reactiveVal(0)
    individual_features_counter <- shiny::reactiveVal(0)

    # Render the complete UI with translations
    output$config_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(i18n()$t("Individual Extraction Configuration")),
        shiny::p(
          class = "text-muted",
          i18n()$t("Configure how individual tree data should be extracted and formatted")
        ),

        # Output Style Selection
        shiny::wellPanel(
          shiny::h5(shiny::icon("table"), " ", i18n()$t("Output Style")),
          shiny::radioButtons(
            ns("output_style"),
            NULL,
            choices = stats::setNames(
              c("auto", "minimal", "standard", "permanent_plot",
                "permanent_plot_multi_census", "transect", "full"),
              c(i18n()$t("Auto-detect from method"),
                i18n()$t("Minimal (essential columns only)"),
                i18n()$t("Standard (common analysis)"),
                i18n()$t("Permanent Plot (single census)"),
                i18n()$t("Permanent Plot (multi-census)"),
                i18n()$t("Transect (walk surveys)"),
                i18n()$t("Full (all columns)"))
            ),
            selected = isolate(input$output_style) %||% "auto"
          ),
          shiny::uiOutput(ns("style_description"))
        ),

        # Census Handling
        shiny::wellPanel(
          shiny::h5(shiny::icon("calendar"), " ", i18n()$t("Census Handling")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::radioButtons(
                ns("census_strategy"),
                i18n()$t("Census Strategy"),
                choices = stats::setNames(
                  c("last", "first", "mean"),
                  c(i18n()$t("Last census"),
                    i18n()$t("First census"),
                    i18n()$t("Mean across censuses"))
                ),
                selected = isolate(input$census_strategy) %||% "last"
              )
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("show_multiple_census"),
                i18n()$t("Show multiple census data"),
                value = isolate(input$show_multiple_census) %||% FALSE
              ),
              shiny::helpText(i18n()$t("Creates separate columns for each census"))
            )
          ),
          shiny::hr(),
          shiny::radioButtons(
            ns("individual_features_format"),
            i18n()$t("Individual features format"),
            choices = stats::setNames(
              c("wide", "long", "census_pairs"),
              c(i18n()$t("Wide format (one row per individual)"),
                i18n()$t("Long format (one row per measurement)"),
                i18n()$t("Census pairs (one row per census interval)"))
            ),
            selected = isolate(input$individual_features_format) %||% "wide",
            inline = TRUE
          ),
          shiny::helpText(
            i18n()$t("In long format, each measurement has its own row (e.g. two diameters = two rows). Census filtering is still applied.")
          )
        ),

        # Data Organization Options
        shiny::wellPanel(
          shiny::h5(shiny::icon("cog"), " ", i18n()$t("Data Organization")),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("concatenate_stem"),
                i18n()$t("Concatenate multiple stems"),
                value = isolate(input$concatenate_stem) %||% FALSE
              ),
              shiny::helpText(i18n()$t("Combine data from multiple stems per individual"))
            ),
            shiny::column(
              6,
              shiny::checkboxInput(
                ns("remove_ids"),
                i18n()$t("Remove database IDs"),
                value = isolate(input$remove_ids) %||% TRUE
              ),
              shiny::helpText(i18n()$t("Remove internal ID columns for cleaner output"))
            )
          ),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::selectInput(
                ns("issues"),
                i18n()$t("Issue handling"),
                choices = c(
                  "Remove flagged records" = "remove",
                  "Include issue columns" = "include",
                  "Ignore (keep all, no flags)" = "ignore"
                ),
                selected = isolate(input$issues) %||% "remove"
              ),
              shiny::helpText(i18n()$t("How to handle flagged problematic records"))
            )
          )
        ),

        # Additional Extraction Options
        shiny::wellPanel(
          shiny::h5(shiny::icon("plus-circle"), " ", i18n()$t("Additional Data")),
          shiny::checkboxInput(
            ns("extract_traits"),
            i18n()$t("Extract taxonomic traits (wood density, etc.)"),
            value = isolate(input$extract_traits) %||% TRUE
          ),
          shiny::checkboxInput(
            ns("extract_individual_features"),
            i18n()$t("Extract individual-level features"),
            value = isolate(input$extract_individual_features) %||% TRUE
          ),
          shiny::checkboxInput(
            ns("extract_subplot_features"),
            i18n()$t("Extract subplot-level features"),
            value = isolate(input$extract_subplot_features) %||% TRUE
          ),
          shiny::hr(),
          shiny::checkboxInput(
            ns("traits_to_genera"),
            i18n()$t("Fallback to genus-level traits when species data unavailable"),
            value = isolate(input$traits_to_genera) %||% FALSE
          )
        ),

        # Execute Button
        shiny::hr(),
        shiny::actionButton(
          ns("extract_individuals"),
          i18n()$t("Extract Individuals from Selected Plots"),
          icon = shiny::icon("download"),
          class = "btn-success btn-lg btn-block"
        ),

        # Individual Features Query Section (placed after extract button)
        shiny::hr(),
        shiny::wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #17a2b8;",
          shiny::h5(shiny::icon("microscope"), " ", i18n()$t("Query Individual Features Separately")),

          shiny::p(
            class = "text-muted",
            style = "font-size: 0.95em;",
            i18n()$t("This optional step extracts all attributes linked to individuals, including trait measurements and observations.")
          ),

          shiny::checkboxInput(
            ns("enable_individual_features_query"),
            i18n()$t("Enable separate extraction of individual-level features"),
            value = FALSE
          ),

          # Hidden panel that shows when checkbox is enabled
          shinyjs::hidden(
            shiny::div(
              id = ns("individual_features_config_panel"),
              shiny::hr(),

              # Trait Selection
              shiny::radioButtons(
                ns("trait_selection_mode"),
                i18n()$t("Trait Selection"),
                choices = stats::setNames(
                  c("all", "specific"),
                  c(i18n()$t("All available traits"),
                    i18n()$t("Specific traits only"))
                ),
                selected = "all"
              ),

              # Trait IDs input (only shown when "specific" is selected)
              shinyjs::hidden(
                shiny::div(
                  id = ns("trait_ids_input_panel"),
                  shiny::textInput(
                    ns("trait_ids_input"),
                    i18n()$t("Select trait IDs (comma-separated)"),
                    value = "",
                    placeholder = i18n()$t("Example: 1,2,5,10")
                  ),
                  shiny::helpText(i18n()$t("Leave empty for all traits"))
                )
              ),

              # Format selection with detailed explanation
              shiny::h6(i18n()$t("Output Format")),
              shiny::radioButtons(
                ns("individual_features_format"),
                NULL,
                choices = stats::setNames(
                  c("wide", "long", "census_pairs"),
                  c(i18n()$t("Wide format (measurements as columns)"),
                    i18n()$t("Long format (measurements as rows)"),
                    i18n()$t("Census pairs (one row per census interval)"))
                ),
                selected = "wide"
              ),
              shiny::div(
                class = "alert alert-info",
                style = "font-size: 0.85em; margin-top: -10px;",
                shiny::uiOutput(ns("format_explanation"))
              ),

              # Additional options
              shiny::checkboxInput(
                ns("include_multi_census_features"),
                i18n()$t("Include multi-census data"),
                value = FALSE
              ),

              shiny::checkboxInput(
                ns("include_metadata_features"),
                i18n()$t("Include measurement metadata"),
                value = FALSE
              ),

              # Query button
              shiny::hr(),
              shiny::actionButton(
                ns("query_individual_features"),
                i18n()$t("Query Individual Features"),
                icon = shiny::icon("search"),
                class = "btn-info btn-block"
              )
            )
          )
        )
      )
    })

    # Toggle individual features config panel visibility
    shiny::observeEvent(input$enable_individual_features_query, {
      if (isTRUE(input$enable_individual_features_query)) {
        shinyjs::show("individual_features_config_panel")
      } else {
        shinyjs::hide("individual_features_config_panel")
      }
    })

    # Toggle trait IDs input visibility based on selection mode
    shiny::observeEvent(input$trait_selection_mode, {
      if (identical(input$trait_selection_mode, "specific")) {
        shinyjs::show("trait_ids_input_panel")
      } else {
        shinyjs::hide("trait_ids_input_panel")
      }
    })

    # Format explanation for individual features
    output$format_explanation <- shiny::renderUI({
      shiny::req(input$individual_features_format)

      if (input$individual_features_format == "wide") {
        shiny::tagList(
          shiny::strong(i18n()$t("Wide format:")),
          " ",
          i18n()$t("One row per individual, measurements as columns. Values are aggregated if multiple observations exist per individual.")
        )
      } else if (input$individual_features_format == "census_pairs") {
        shiny::tagList(
          shiny::strong(i18n()$t("Census pairs format:")),
          " ",
          i18n()$t("One row per consecutive census pair per individual. Columns: dbh0, dbh1, date_census0, date_census1, time (days between censuses), stem_status at second census.")
        )
      } else {
        shiny::tagList(
          shiny::strong(i18n()$t("Long format:")),
          " ",
          i18n()$t("One row per measurement. More complete representation with no aggregation.")
        )
      }
    })

    # Style descriptions (translated)
    output$style_description <- shiny::renderUI({
      shiny::req(input$output_style)

      style_descriptions <- list(
        auto = i18n()$t("Automatically selects the best format based on plot method"),
        minimal = i18n()$t("Returns only essential columns (plot, tag, species, dbh)"),
        standard = i18n()$t("Common columns for general ecological analysis"),
        permanent_plot = i18n()$t("Structured format for permanent plot monitoring (single census)"),
        permanent_plot_multi_census = i18n()$t("Preserves all census columns for time-series analysis"),
        transect = i18n()$t("Simplified format optimized for transect walk surveys"),
        full = i18n()$t("Complete dataset with all available columns")
      )

      selected_style <- input$output_style
      description <- style_descriptions[[selected_style]]

      shiny::div(
        class = "alert alert-info",
        style = "margin-top: 10px; font-size: 0.9em;",
        shiny::icon("info-circle"),
        " ",
        description
      )
    })

    # Execute button handler
    shiny::observeEvent(input$extract_individuals, {
      cli::cli_alert_info("Extract button clicked!")

      # Check selected plots
      plots <- selected_plots()
      cli::cli_alert_info("Selected plots: {if(is.null(plots)) 'NULL' else paste(length(plots), 'plots')}")

      if (is.null(plots) || length(plots) == 0) {
        cli::cli_alert_warning("No plots selected!")
        shiny::showNotification(
          i18n()$t("Please select at least one plot before extracting individuals"),
          type = "warning",
          duration = 5
        )
        return()
      }

      cli::cli_alert_success("Incrementing execute counter to {execute_counter() + 1}")
      execute_counter(execute_counter() + 1)
      cli::cli_alert_success("Execute counter now at: {execute_counter()}")
    })

    # Individual features query button handler
    shiny::observeEvent(input$query_individual_features, {
      cli::cli_alert_info("Query individual features button clicked!")

      # Check selected plots
      plots <- selected_plots()
      cli::cli_alert_info("Selected plots: {if(is.null(plots)) 'NULL' else paste(length(plots), 'plots')}")

      if (is.null(plots) || length(plots) == 0) {
        cli::cli_alert_warning("No plots selected!")
        shiny::showNotification(
          i18n()$t("Please select at least one plot before querying individual features"),
          type = "warning",
          duration = 5
        )
        return()
      }

      cli::cli_alert_success("Incrementing individual features counter to {individual_features_counter() + 1}")
      individual_features_counter(individual_features_counter() + 1)
      cli::cli_alert_success("Individual features counter now at: {individual_features_counter()}")
    })

    # Build options list
    options <- shiny::reactive({
      list(
        # Output formatting
        output_style = input$output_style %||% "auto",

        # Census handling
        census_strategy = input$census_strategy %||% "last",
        show_multiple_census = input$show_multiple_census %||% FALSE,
        individual_features_format = input$individual_features_format %||% "wide",

        # Data organization
        concatenate_stem = input$concatenate_stem %||% FALSE,
        remove_ids = input$remove_ids %||% TRUE,
        issues = input$issues %||% "remove",

        # Additional extraction
        extract_traits = input$extract_traits %||% TRUE,
        extract_individual_features = input$extract_individual_features %||% TRUE,
        extract_subplot_features = input$extract_subplot_features %||% TRUE,
        traits_to_genera = input$traits_to_genera %||% FALSE
      )
    })

    # Build individual features options list
    individual_features_options <- shiny::reactive({
      # Parse trait IDs if specific mode selected
      trait_ids <- NULL
      if (identical(input$trait_selection_mode, "specific")) {
        trait_ids_text <- input$trait_ids_input %||% ""
        if (nzchar(trait_ids_text)) {
          # Parse comma-separated IDs
          trait_ids <- as.integer(strsplit(trait_ids_text, ",\\s*")[[1]])
          trait_ids <- trait_ids[!is.na(trait_ids)]  # Remove NAs
        }
      }

      list(
        enabled = input$enable_individual_features_query %||% FALSE,
        trait_ids = trait_ids,
        format = input$individual_features_format %||% "wide",
        include_multi_census = input$include_multi_census_features %||% FALSE,
        census_strategy = input$census_strategy %||% "last",
        include_metadata = input$include_metadata_features %||% FALSE,
        issues = input$issues %||% "remove"
      )
    })

    # Return reactive values
    return(
      list(
        options = options,
        execute_trigger = shiny::reactive(execute_counter()),
        individual_features_options = individual_features_options,
        individual_features_trigger = shiny::reactive(individual_features_counter())
      )
    )
  })
}
