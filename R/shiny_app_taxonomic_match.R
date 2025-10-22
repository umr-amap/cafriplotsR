# Main Shiny App for Taxonomic Name Standardization
#
# Modular Shiny app that orchestrates all modules for taxonomic name matching

#' Taxonomic Name Standardization App
#'
#' Main Shiny application for standardizing taxonomic names against the backbone database.
#' This app provides a modern, modular interface for matching species names with
#' intelligent fuzzy matching and manual review capabilities.
#'
#' @param data Optional data.frame or reactive, pre-loaded data to standardize
#' @param name_column Optional character, pre-selected column name containing taxa
#' @param language Character, initial language ("en" or "fr"), default: "en"
#' @param min_similarity Numeric, minimum similarity for fuzzy matching (0-1), default: 0.3
#' @param max_suggestions Integer, maximum suggestions per name, default: 10
#' @param mode Character, review mode ("interactive" or "batch"), default: "interactive"
#'
#' @return Shiny app object
#'
#' @keywords internal
#' @import shiny
app_taxonomic_match <- function(
  data = NULL,
  name_column = NULL,
  language = "en",
  min_similarity = 0.3,
  max_suggestions = 10,
  mode = "interactive"
) {

  # Validate parameters
  language <- match.arg(language, c("en", "fr"))
  mode <- match.arg(mode, c("interactive", "batch"))

  # UI
  ui <- shiny::fluidPage(

    # Custom CSS
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .sidebar-panel {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
        }
        .main-tabs {
          margin-top: 20px;
        }
        .module-title {
          color: #2c3e50;
          border-bottom: 2px solid #3498db;
          padding-bottom: 10px;
          margin-bottom: 20px;
        }
      "))
    ),

    # Language toggle (top right)
    shiny::absolutePanel(
      top = 10,
      right = 20,
      fixed = TRUE,
      draggable = FALSE,
      mod_language_toggle_ui("language")
    ),

    # Title
    shiny::div(
      class = "container-fluid",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h1(
            shiny::textOutput("app_title"),
            style = "color: #2c3e50; margin-top: 20px; margin-bottom: 10px;"
          ),
          shiny::p(
            shiny::textOutput("app_subtitle"),
            style = "color: #7f8c8d; font-size: 16px; margin-bottom: 30px;"
          )
        )
      )
    ),

    # Main layout
    shiny::fluidRow(

      # Sidebar
      shiny::column(
        width = 3,
        class = "sidebar-panel",

        # Data Input
        mod_data_input_ui("data_input"),
        shiny::hr(),

        # Column Selection
        mod_column_select_ui("column_select"),
        shiny::hr(),

        # Progress Tracker
        mod_progress_tracker_ui("progress")
      ),

      # Main panel
      shiny::column(
        width = 9,
        shiny::div(
          class = "main-tabs",

          shiny::tabsetPanel(
            id = "main_tabs",
            type = "pills",

            # Auto Match Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_auto_match", inline = TRUE),
              value = "auto_match",
              icon = shiny::icon("magic"),
              shiny::br(),
              mod_auto_matching_ui("auto_match")
            ),

            # Review Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_review", inline = TRUE),
              value = "review",
              icon = shiny::icon("search"),
              shiny::br(),
              mod_name_review_ui("review")
            ),

            # Export Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_export", inline = TRUE),
              value = "export",
              icon = shiny::icon("download"),
              shiny::br(),
              mod_results_export_ui("export")
            ),

            # Traits Enrichment Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_traits", inline = TRUE),
              value = "traits",
              icon = shiny::icon("database"),
              shiny::br(),
              mod_traits_enrichment_ui("traits")
            )
          )
        )
      )
    ),

    # Footer
    shiny::hr(),
    shiny::div(
      style = "text-align: center; color: #7f8c8d; padding: 20px;",
      shiny::p(
        shiny::icon("leaf"),
        "Taxonomic Name Standardization Tool |",
        "Powered by plotsdatabase package",
        style = "font-size: 14px;"
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Stop the server when session ends
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    # Language management
    current_language <- mod_language_toggle_server("language", initial = language)

    # Get translations
    t <- shiny::reactive({
      get_translations(current_language())
    })

    # App title and subtitle
    output$app_title <- shiny::renderText({
      t()$app_title
    })

    output$app_subtitle <- shiny::renderText({
      t()$app_subtitle
    })

    # Tab labels (need to be reactive for language switching)
    output$tab_auto_match <- shiny::renderText({
      t()$tab_auto_match
    })

    output$tab_review <- shiny::renderText({
      t()$tab_review
    })

    output$tab_export <- shiny::renderText({
      t()$tab_export
    })

    output$tab_traits <- shiny::renderText({
      "Enrich with Traits"
    })

    # Data input module
    user_data <- mod_data_input_server(
      "data_input",
      provided_data = data,
      language = current_language
    )

    # Column selection module
    column_info <- mod_column_select_server(
      "column_select",
      data = user_data,
      initial_column = name_column,
      language = current_language
    )

    # Auto matching module
    match_results <- mod_auto_matching_server(
      "auto_match",
      data = user_data,
      column_name = shiny::reactive(column_info()$column),
      include_authors = shiny::reactive(column_info()$include_authors),
      min_similarity = min_similarity,
      language = current_language
    )

    # Progress tracker module
    mod_progress_tracker_server(
      "progress",
      match_results = match_results,
      language = current_language
    )

    # Manual review module
    reviewed_results <- mod_name_review_server(
      "review",
      match_results = match_results,
      mode = mode,
      max_suggestions = max_suggestions,
      min_similarity = min_similarity,
      language = current_language
    )

    # Results export module (use reviewed results instead of just matched results)
    mod_results_export_server(
      "export",
      results = reviewed_results,
      original_data = user_data,
      language = current_language
    )

    # Traits enrichment module
    mod_traits_enrichment_server(
      "traits",
      results = reviewed_results,
      language = current_language
    )
  }

  # Return Shiny app object
  shiny::shinyApp(ui, server)
}
