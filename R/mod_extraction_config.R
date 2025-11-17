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
    shiny::h4("Individual Extraction Configuration"),
    shiny::p(
      class = "text-muted",
      "Configure how individual tree data should be extracted and formatted"
    ),

    # Output Style Selection
    shiny::wellPanel(
      shiny::h5(shiny::icon("table"), " Output Style"),
      shiny::radioButtons(
        ns("output_style"),
        NULL,
        choices = c(
          "Auto-detect from method" = "auto",
          "Minimal (essential columns only)" = "minimal",
          "Standard (common analysis)" = "standard",
          "Permanent Plot (single census)" = "permanent_plot",
          "Permanent Plot (multi-census)" = "permanent_plot_multi_census",
          "Transect (walk surveys)" = "transect",
          "Full (all columns)" = "full"
        ),
        selected = "auto"
      ),
      shiny::uiOutput(ns("style_description"))
    ),

    # Census Handling
    shiny::wellPanel(
      shiny::h5(shiny::icon("calendar"), " Census Handling"),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::radioButtons(
            ns("census_strategy"),
            "Census Strategy",
            choices = c(
              "Last census" = "last",
              "First census" = "first",
              "Mean across censuses" = "mean"
            ),
            selected = "last"
          )
        ),
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("show_multiple_census"),
            "Show multiple census data",
            value = FALSE
          ),
          shiny::helpText("Creates separate columns for each census")
        )
      )
    ),

    # Data Organization Options
    shiny::wellPanel(
      shiny::h5(shiny::icon("cog"), " Data Organization"),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("concatenate_stem"),
            "Concatenate multiple stems",
            value = FALSE
          ),
          shiny::helpText("Combine data from multiple stems per individual")
        ),
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("remove_ids"),
            "Remove database IDs",
            value = TRUE
          ),
          shiny::helpText("Remove internal ID columns for cleaner output")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("remove_obs_with_issue"),
            "Remove observations with issues",
            value = TRUE
          ),
          shiny::helpText("Exclude flagged problematic records")
        )
      ),
      shiny::checkboxInput(
        ns("include_issue"),
        "Include issue flags in output",
        value = FALSE
      )
    ),

    # Additional Extraction Options
    shiny::wellPanel(
      shiny::h5(shiny::icon("plus-circle"), " Additional Data"),
      shiny::checkboxInput(
        ns("extract_traits"),
        "Extract taxonomic traits (wood density, etc.)",
        value = TRUE
      ),
      shiny::checkboxInput(
        ns("extract_individual_features"),
        "Extract individual-level features",
        value = TRUE
      ),
      shiny::checkboxInput(
        ns("extract_subplot_features"),
        "Extract subplot-level features",
        value = TRUE
      ),
      shiny::hr(),
      shiny::checkboxInput(
        ns("traits_to_genera"),
        "Fallback to genus-level traits when species data unavailable",
        value = FALSE
      )
    ),

    # Execute Button
    shiny::hr(),
    shiny::actionButton(
      ns("extract_individuals"),
      "Extract Individuals from Selected Plots",
      icon = shiny::icon("download"),
      class = "btn-success btn-lg btn-block"
    )
  )
}

#' Extraction Configuration Module - Server
#'
#' Server logic for extraction configuration
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing selected plot IDs
#'
#' @return A reactive list containing:
#'   - options: Named list of extraction options
#'   - execute_trigger: Reactive counter that increments on execute
#'
#' @keywords internal
#' @export
mod_extraction_config_server <- function(id, selected_plots) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    execute_counter <- shiny::reactiveVal(0)

    # Style descriptions
    style_descriptions <- list(
      auto = "Automatically selects the best format based on plot method",
      minimal = "Returns only essential columns (plot, tag, species, dbh)",
      standard = "Common columns for general ecological analysis",
      permanent_plot = "Structured format for permanent plot monitoring (single census)",
      permanent_plot_multi_census = "Preserves all census columns for time-series analysis",
      transect = "Simplified format optimized for transect walk surveys",
      full = "Complete dataset with all available columns"
    )

    # Render style description
    output$style_description <- shiny::renderUI({
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
          "Please select at least one plot before extracting individuals",
          type = "warning",
          duration = 5
        )
        return()
      }

      cli::cli_alert_success("Incrementing execute counter to {execute_counter() + 1}")
      execute_counter(execute_counter() + 1)
      cli::cli_alert_success("Execute counter now at: {execute_counter()}")
    })

    # Build options list
    options <- shiny::reactive({
      list(
        # Output formatting
        output_style = input$output_style,

        # Census handling
        census_strategy = input$census_strategy,
        show_multiple_census = input$show_multiple_census,

        # Data organization
        concatenate_stem = input$concatenate_stem,
        remove_ids = input$remove_ids,
        remove_obs_with_issue = input$remove_obs_with_issue,
        include_issue = input$include_issue,

        # Additional extraction
        extract_traits = input$extract_traits,
        extract_individual_features = input$extract_individual_features,
        extract_subplot_features = input$extract_subplot_features,
        traits_to_genera = input$traits_to_genera
      )
    })

    # Return reactive values
    return(
      list(
        options = options,
        execute_trigger = shiny::reactive(execute_counter())
      )
    )
  })
}
