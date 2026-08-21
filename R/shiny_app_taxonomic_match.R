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
#' @param language Character, initial language ("en" or "fr"), default: "fr"
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
  language = "fr",
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
      mod_database_login_ui("login", allow_public = TRUE, allow_offline = TRUE)
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
              style = "color: #7f8c8d; font-size: 16px; margin-bottom: 15px;"
            ),
            shiny::uiOutput("app_intro_ui")
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

          # Output Options
          shiny::uiOutput("wcvp_option_ui"),

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
      login_output <- mod_database_login_server(
        "login", allow_public = TRUE, allow_offline = TRUE
      )

      pool_main_reactive <- login_output$pool_main
      pool_taxa_reactive <- login_output$pool_taxa
      authenticated_reactive <- login_output$authenticated
      is_offline_reactive <- login_output$is_offline
    } else {
      # Pool provided, mark as authenticated
      pool_taxa_reactive <- shiny::reactive(pool_taxa)
      pool_main_reactive <- shiny::reactive(NULL)  # Not needed for taxonomic matching
      authenticated_reactive <- shiny::reactive(TRUE)
      is_offline_reactive <- shiny::reactive(FALSE)

      # Store in global env
      .db_env$pool_taxa <- pool_taxa
      # Note: Pool cleanup is handled by cleanup_connections() below
    }

    # Sync language from login module to app language selector
    shiny::observe({
      lang <- login_output$language()
      shiny::req(lang)
      shiny::updateSelectInput(session, "selected_language", selected = lang)
    })

    # On session end: locally, free connections and quit R when the browser
    # closes. When served (SSP Cloud / shiny-server) a single process handles
    # many sessions, so we must NOT cleanup global state or stop the app -
    # idle pools are reclaimed by the pool package's eviction instead.
    session$onSessionEnded(function() {
      if (.is_served()) {
        return(invisible(NULL))
      }
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

    # Cached backbone is loaded once for offline mode and passed down to
    # modules that would otherwise hit the DB (review, fuzzy suggestions).
    # In online mode this stays NULL — modules use their normal SQL paths.
    app_backbone <- shiny::reactive({
      if (isTRUE(is_offline_reactive())) {
        load_backbone_cache()
      } else {
        NULL
      }
    })

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


    # App intro (collapsible section below the subtitle)
    output$app_intro_ui <- shiny::renderUI({
      shiny::tags$details(
        style = "margin-bottom:20px; background:#eaf3fb; padding:14px 16px; border-radius:5px; border-left:4px solid #3498db;",
        shiny::tags$summary(
          style = "cursor:pointer; font-weight:bold; color:#2c3e50;",
          shiny::icon("info-circle"), " ", i18n()$t("About this app")
        ),
        shiny::tags$ul(
          style = "margin:10px 0 0 0; padding-left:20px; line-height:1.7;",
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Data input:")), " ",
            i18n()$t(paste0(
              "Import an Excel file or copy-paste a list of names to standardize."
            ))
          ),
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Column selection:")), " ",
            i18n()$t(paste0(
              "Select the column(s) containing the taxonomic information: a single column ",
              "or multiple columns (e.g. genus and specific epithet separately)."
            ))
          ),
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Automatic matching:")), " ",
            i18n()$t(paste0(
              "Names are checked automatically against the backbone. Exact matches are found first; ",
              "fuzzy matching then handles misspellings. Synonyms are resolved to accepted names. ",
              "Progress is saved and can be resumed if the session is interrupted."
            ))
          ),
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Similarity threshold:")), " ",
            i18n()$t(paste0(
              "Controls how strict the automatic matching is. A higher value reduces false matches ",
              "but leaves more names for manual review; a lower value increases automatic matching ",
              "but raises the risk of errors."
            ))
          ),
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Manual review:")), " ",
            i18n()$t(paste0(
              "Unmatched names are reviewed one by one. The app proposes ranked suggestions and ",
              "allows free search in the backbone. Match quality is shown as a percentage ",
              "(green >= 90 %, blue >= 70 %). Note: when the genus is recognised, fuzzy ",
              "matching is restricted to species within that genus."
            ))
          ),
          shiny::tags$li(
            shiny::tags$strong(i18n()$t("Traits enrichment:")), " ",
            i18n()$t(paste0(
              "Once names are standardised, the Traits tab retrieves available measurements for each ",
              "matched taxon. Numeric traits are reported as mean, sd and n; categorical traits as ",
              "the most frequent value or all values combined."
            ))
          )
        )
      )
    })

    # Only initialize modules after authentication (runs once)
    shiny::observe({
      shiny::req(authenticated_reactive() == TRUE)
      shiny::req(!rv$modules_initialized)  # Only run once

      cli::cli_alert_info("Initializing app modules...")

      # Check WCVP availability in the taxa database — skip entirely when
      # offline (no connection means no WCVP enrichment regardless).
      wcvp_avail <- shiny::reactive({
        if (isTRUE(is_offline_reactive())) return(FALSE)
        tryCatch({
          con_taxa <- call.mydb.taxa()
          status <- get_wcvp_status(con_taxa)
          !is.null(status) && !is.null(status$version) && !is.na(status$version)
        }, error = function(e) {
          FALSE
        })
      })

      # Render WCVP output option in sidebar
      output$wcvp_option_ui <- shiny::renderUI({
        ns_main <- function(id) id  # top-level inputs don't need a namespace

        if (isTRUE(wcvp_avail())) {
          shiny::tagList(
            shiny::h5(
              shiny::icon("globe"),
              i18n()$t("Output options"),
              style = "margin-bottom: 6px;"
            ),
            shiny::checkboxInput(
              inputId = "use_wcvp_names",
              label   = i18n()$t("Use World Checklist of Vascular Plants (WCVP) names in the output"),
              value   = FALSE
            ),
            shiny::helpText(
              i18n()$t(paste0(
                "By default, names are standardized against the internal taxonomic backbone. ",
                "When this box is checked, the standardized name in the output is replaced by the ",
                "accepted name from the World Checklist of Vascular Plants (WCVP), an international ",
                "reference maintained by the Royal Botanic Gardens, Kew, whenever the taxon is found ",
                "there. Taxa absent from WCVP keep their internal backbone name."
              )),
              style = "margin-top: -6px; font-size: 0.85em;"
            ),
            shiny::hr()
          )
        }
      })

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
        i18n = i18n
      )

      # Auto matching module
      # Use data from column_info (may be modified with combined column)
      use_wcvp_names <- shiny::reactive(isTRUE(input$use_wcvp_names))

      match_results <- mod_auto_matching_server(
        "auto_match",
        data = shiny::reactive(column_info()$data),
        column_name = shiny::reactive(column_info()$column),
        include_authors = shiny::reactive(column_info()$include_authors),
        min_similarity = min_similarity,
        i18n = i18n,
        use_wcvp_names = use_wcvp_names,
        is_offline = is_offline_reactive
      )

      # Manual review module — pass cached backbone for offline custom search
      reviewed_results <- mod_name_review_server(
        "review",
        match_results = match_results,
        mode = mode,
        max_suggestions = max_suggestions,
        min_similarity = min_similarity,
        i18n = i18n,
        backbone = app_backbone
      )

      # Progress tracker module (uses reviewed_results to include manual reviews)
      mod_progress_tracker_server(
        "progress",
        match_results = reviewed_results,
        i18n = i18n
      )

      # Results export module (use reviewed results instead of just matched results)
      mod_results_export_server(
        "export",
        results = reviewed_results,
        original_data = user_data,
        i18n = i18n
      )

      # Traits enrichment module
      mod_traits_enrichment_server(
        "traits",
        results = reviewed_results,
        column_name = shiny::reactive(column_info()$column),
        i18n = i18n
      )

      # Mark modules as initialized
      rv$modules_initialized <- TRUE
      cli::cli_alert_success("All modules initialized successfully!")
    })  # Close the observe block for module initialization

    # Offline mode: hide tabs that require live DB access (Traits enrichment).
    # WCVP option in the sidebar is already conditional on get_wcvp_status()
    # which silently returns FALSE without a DB, so it self-hides.
    shiny::observe({
      shiny::req(authenticated_reactive() == TRUE)
      if (isTRUE(is_offline_reactive())) {
        shiny::hideTab("main_tabs", "traits")
      } else {
        shiny::showTab("main_tabs", "traits")
      }
    })
  }

  # Return Shiny app object
  shiny::shinyApp(ui, server)
}
