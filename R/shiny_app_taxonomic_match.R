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
#' @param pool_taxa Optional connection pool for taxa database (will prompt for login if NULL)
#'
#' @return Shiny app object
#'
#' @keywords internal
#' @import shiny
app_taxonomic_match <- function(
  data = NULL,
  name_column = NULL,
  language = "en",
  min_similarity = 0.6,
  max_suggestions = 10,
  mode = "interactive",
  pool_taxa = NULL
) {

  # Validate parameters
  language <- match.arg(language, c("en", "fr"))
  mode <- match.arg(mode, c("interactive", "batch"))

  # Initialize translator (must be before UI for usei18n)
  translator <- init_translator()

  # UI
  ui <- shiny::fluidPage(

    # Add shiny.i18n (required for automatic translation)
    shiny.i18n::usei18n(translator),

    # Add shinyjs for dynamic show/hide
    shinyjs::useShinyjs(),

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

    # Login panel (shown if pool_taxa not provided)
    shiny::conditionalPanel(
      condition = "!output.authenticated",
      mod_database_login_ui("login")
    ),

    # Main app interface (shown after authentication)
    shiny::conditionalPanel(
      condition = "output.authenticated",

      # Language toggle (top right)
      shiny::absolutePanel(
        top = 10,
        right = 20,
        fixed = TRUE,
        draggable = FALSE,
        style = "z-index: 1000;",
        shiny::radioButtons(
          inputId = "selected_language",
          label = NULL,
          choices = c("EN" = "en", "FR" = "fr"),
          selected = language,
          inline = TRUE
        )
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
          shiny::textOutput("footer_text", inline = TRUE),
          style = "font-size: 14px;"
        )
      )
    ) # End conditionalPanel for main app
  )

  # Server
  server <- function(input, output, session) {

    # Database authentication
    if (is.null(pool_taxa)) {
      # Use login module for authentication
      login_output <- mod_database_login_server("login")

      pool_main_reactive <- login_output$pool_main
      pool_taxa_reactive <- login_output$pool_taxa
      authenticated_reactive <- login_output$authenticated
    } else {
      # Pool provided, mark as authenticated
      pool_taxa_reactive <- shiny::reactive(pool_taxa)
      pool_main_reactive <- shiny::reactive(NULL)  # Not needed for taxonomic matching
      authenticated_reactive <- shiny::reactive(TRUE)

      # Store in global env
      .db_env$pool_taxa <- pool_taxa
      # Note: Pool cleanup is handled by cleanup_connections() below
    }

    # Stop app and quit R when browser is closed
    session$onSessionEnded(function() {
      # Clean up all connections and credentials
      tryCatch({
        cleanup_connections()
      }, error = function(e) {
        cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
      })
      shiny::stopApp()
    })

    # Output for conditional panel (needs to be suspendable=FALSE)
    output$authenticated <- shiny::reactive({
      authenticated_reactive()
    })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    # Reactive values for module initialization
    rv <- shiny::reactiveValues(
      modules_initialized = FALSE
    )

    # Create reactive translator (shiny.i18n recommended pattern)
    i18n <- shiny::reactive({
      selected <- input$selected_language
      if (length(selected) > 0 && selected %in% translator$get_languages()) {
        translator$set_translation_language(selected)
      }
      translator
    })

    # Current language reactive (for modules still using old system)
    current_language <- shiny::reactive({
      input$selected_language
    })

    # App title and subtitle
    output$app_title <- shiny::renderText({
      i18n()$t("Taxonomic Name Standardization for Tropical African Plants")
    })

    output$app_subtitle <- shiny::renderText({
      i18n()$t("Standardize species names against the taxonomic backbone")
    })

    # Tab labels (need to be reactive for language switching)
    output$tab_auto_match <- shiny::renderText({
      i18n()$t("Auto Match")
    })

    output$tab_review <- shiny::renderText({
      i18n()$t("Review")
    })

    output$tab_export <- shiny::renderText({
      i18n()$t("Export")
    })

    output$tab_traits <- shiny::renderText({
      i18n()$t("Enrich with Traits")
    })

    # Footer text
    output$footer_text <- shiny::renderText({
      paste(
        i18n()$t("Taxonomic Name Standardization Tool"),
        "|",
        i18n()$t("Powered by CafriplotsR package")
      )
    })

    # Only initialize modules after authentication (runs once)
    shiny::observe({
      shiny::req(authenticated_reactive() == TRUE)
      shiny::req(!rv$modules_initialized)  # Only run once

      cli::cli_alert_info("Initializing app modules...")

      # Data input module
      user_data <- mod_data_input_server(
        "data_input",
        provided_data = data,
        i18n = i18n
      )

      # Column selection module
      column_info <- mod_column_select_server(
        "column_select",
        data = user_data,
        initial_column = name_column,
        language = current_language
      )

      # Auto matching module
      # Use data from column_info (may be modified with combined column)
      match_results <- mod_auto_matching_server(
        "auto_match",
        data = shiny::reactive(column_info()$data),
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
        column_name = shiny::reactive(column_info()$column),
        language = current_language
      )

      # Mark modules as initialized
      rv$modules_initialized <- TRUE
      cli::cli_alert_success("All modules initialized successfully!")
    })  # Close the observe block for module initialization
  }

  # Return Shiny app object
  shiny::shinyApp(ui, server)
}
