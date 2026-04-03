# Shiny App: Specimen Import Wizard
#
# Interactive wizard application for importing new herbarium specimens to the database.
#
# Features:
# - Upload specimen data (Excel/CSV)
# - Map columns to database fields
# - Match collectors and taxa to database
# - Preview and import specimens
#
# Main function: launch_specimen_import_wizard()

#' Launch Specimen Import Wizard
#'
#' Launches an interactive wizard-style Shiny application for importing
#' new herbarium specimens to the database from Excel/CSV files.
#'
#' **Wizard Steps:**
#' - Step 1: Upload your data file
#' - Step 2: Map your columns to database fields
#' - Step 3: Match collector names and taxa to the database
#' - Step 4: Preview and import to the specimens table
#'
#' @param lang Character, initial language ("en" or "fr"). Default "en".
#'
#' @return Launches Shiny app (does not return until app closes)
#'
#' @details
#' The wizard guides you through the entire import process with validation
#' at each step. Collector names are matched to the `table_colnam` lookup table,
#' and taxonomic identifications are validated against the taxa database.
#'
#' For creating links between specimens and individuals, use the separate
#' `launch_individual_specimen_linking_app()` function.
#'
#' @examples
#' \dontrun{
#' launch_specimen_import_wizard()
#' launch_specimen_import_wizard(lang = "fr")
#' }
#'
#' @export
launch_specimen_import_wizard <- function(lang = "fr") {
  # Load i18n translations
  i18n <- shiny.i18n::Translator$new(
    translation_json_path = system.file(
      "translations/translation.json",
      package = "CafriplotsR"
    )
  )
  i18n$set_translation_language(lang)

  # Define UI
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(spin = "fading-circle"),

    # Custom CSS for wizard steps
    shiny::tags$style(shiny::HTML("
      .wizard-step {
        display: inline-block;
        padding: 10px 20px;
        margin: 5px;
        border-radius: 25px;
        background: #e9ecef;
        color: #6c757d;
        font-weight: 500;
      }
      .wizard-step.active {
        background: #007bff;
        color: white;
      }
      .wizard-step.completed {
        background: #28a745;
        color: white;
      }
      .section-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
      }
      .nav-pills .nav-link.active {
        background-color: #6c5ce7;
      }
    ")),

    # App title and language toggle
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::h2(shiny::textOutput("app_title"))
      ),
      shiny::column(
        width = 4,
        style = "text-align: right; padding-top: 15px;",
        mod_language_toggle_ui("lang_toggle")
      )
    ),

    shiny::hr(),

    # Login panel (shown before authentication)
    shiny::conditionalPanel(
      condition = "!output.authenticated",
      mod_database_login_ui("login")
    ),

    # Main content (shown after authentication)
    shiny::conditionalPanel(
      condition = "output.authenticated",

      shiny::tabsetPanel(
        id = "main_sections",
        type = "pills",

        # ===== SECTION 1: IMPORT SPECIMENS =====
        shiny::tabPanel(
          title = shiny::uiOutput("section_import_title"),
          value = "import",
          icon = shiny::icon("file-import"),
          shiny::br(),

          # Section header
          shiny::div(
            class = "section-header",
            shiny::h3(shiny::icon("file-import"), " ", shiny::textOutput("import_header_title", inline = TRUE)),
            shiny::p(shiny::textOutput("import_header_desc"))
          ),

          # Wizard progress indicator
          shiny::div(
            style = "text-align: center; margin-bottom: 30px;",
            shiny::uiOutput("wizard_progress")
          ),

          # Wizard steps container
          shiny::div(
            id = "wizard_container",

            # Step 1: Upload
            shiny::conditionalPanel(
              condition = "output.current_step == 1",
              mod_specimen_upload_ui("step1_upload", i18n)
            ),

            # Step 2: Mapping
            shiny::conditionalPanel(
              condition = "output.current_step == 2",
              mod_specimen_mapping_ui("step2_mapping", i18n)
            ),

            # Step 3: Lookup
            shiny::conditionalPanel(
              condition = "output.current_step == 3",
              mod_specimen_lookup_ui("step3_lookup", i18n)
            ),

            # Step 4: Import
            shiny::conditionalPanel(
              condition = "output.current_step == 4",
              mod_specimen_import_ui("step4_import", i18n)
            )
          ),

          # Navigation buttons
          shiny::div(
            style = "margin-top: 30px; padding: 20px; border-top: 1px solid #dee2e6;",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::actionButton(
                  "wizard_back",
                  shiny::tagList(shiny::icon("arrow-left"), " ", i18n$t("Back")),
                  class = "btn-secondary"
                )
              ),
              shiny::column(
                6,
                style = "text-align: right;",
                shiny::actionButton(
                  "wizard_next",
                  shiny::tagList(i18n$t("Next"), " ", shiny::icon("arrow-right")),
                  class = "btn-primary"
                )
              )
            )
          )
        )
      )
    )
  )

  # Define Server
  server <- function(input, output, session) {

    # ===== LANGUAGE MANAGEMENT =====
    current_lang <- mod_language_toggle_server("lang_toggle", initial = lang)

    # Reactive i18n that updates when language changes
    i18n_reactive <- shiny::reactive({
      selected_lang <- current_lang()
      i18n$set_translation_language(selected_lang)
      i18n
    })

    # ===== DATABASE LOGIN =====
    login_result <- mod_database_login_server("login")
    pool_main <- login_result$pool_main
    pool_taxa <- login_result$pool_taxa
    authenticated <- login_result$authenticated

    # Sync language from login module to app language toggle
    shiny::observe({
      lang <- login_result$language()
      shiny::req(lang)
      shiny::updateRadioButtons(session, "lang_toggle-language", selected = lang)
    })

    # Output for conditional panels
    output$authenticated <- shiny::reactive({
      authenticated()
    })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    # ===== WIZARD STATE =====
    current_step <- shiny::reactiveVal(1)

    output$current_step <- shiny::reactive({
      current_step()
    })
    shiny::outputOptions(output, "current_step", suspendWhenHidden = FALSE)

    # ===== TEXT OUTPUTS =====
    output$app_title <- shiny::renderText({
      i18n_reactive()$t("Specimen Management")
    })

    # Section titles
    output$section_import_title <- shiny::renderUI({
      i18n_reactive()$t("Import Specimens")
    })

    # Header titles
    output$import_header_title <- shiny::renderText({
      i18n_reactive()$t("Import New Specimens")
    })

    output$import_header_desc <- shiny::renderText({
      i18n_reactive()$t("Upload your specimen data and import it to the database in 4 easy steps.")
    })

    # ===== WIZARD PROGRESS INDICATOR =====
    output$wizard_progress <- shiny::renderUI({
      step <- current_step()

      steps <- list(
        list(num = 1, label = i18n_reactive()$t("Upload")),
        list(num = 2, label = i18n_reactive()$t("Map Columns")),
        list(num = 3, label = i18n_reactive()$t("Match Values")),
        list(num = 4, label = i18n_reactive()$t("Import"))
      )

      shiny::div(
        lapply(steps, function(s) {
          class <- "wizard-step"
          if (s$num < step) class <- paste(class, "completed")
          if (s$num == step) class <- paste(class, "active")

          icon_name <- if (s$num < step) "check" else if (s$num == 1) "upload" else if (s$num == 2) "columns" else if (s$num == 3) "link" else "database"

          shiny::span(
            class = class,
            shiny::icon(icon_name),
            " ",
            paste0(s$num, ". ", s$label)
          )
        })
      )
    })

    # ===== MODULE INITIALIZATION =====
    # Storage for module outputs
    step1_data <- shiny::reactiveVal(NULL)
    step2_mappings <- shiny::reactiveVal(NULL)
    step2_valid <- shiny::reactiveVal(FALSE)
    step3_results <- shiny::reactiveVal(NULL)
    step3_complete <- shiny::reactiveVal(FALSE)

    shiny::observe({
      shiny::req(authenticated() == TRUE)

      # Step 1: Upload
      uploaded_data <- mod_specimen_upload_server(
        "step1_upload",
        con = pool_main,
        i18n = i18n_reactive
      )

      # Track uploaded data
      shiny::observe({
        step1_data(uploaded_data())
      })

      # Step 2: Mapping
      mapping_result <- mod_specimen_mapping_server(
        "step2_mapping",
        data = uploaded_data,
        con = pool_main,
        i18n = i18n_reactive
      )

      # Track mappings
      shiny::observe({
        step2_mappings(mapping_result$mappings())
        step2_valid(mapping_result$is_valid())
      })

      # Step 3: Lookup
      lookup_result <- mod_specimen_lookup_server(
        "step3_lookup",
        data = uploaded_data,
        mappings = mapping_result$mappings,
        con_main = pool_main,
        con_taxa = pool_taxa,
        i18n = i18n_reactive
      )

      # Track lookup results
      shiny::observe({
        step3_results(lookup_result$matched_data())
        step3_complete(lookup_result$is_complete())
      })

      # Step 4: Import
      import_result <- mod_specimen_import_server(
        "step4_import",
        matched_data = lookup_result$matched_data,
        mappings = mapping_result$mappings,
        matching_complete = lookup_result$is_complete,
        con = pool_main,
        i18n = i18n_reactive
      )
    })

    # ===== WIZARD NAVIGATION =====
    shiny::observeEvent(input$wizard_back, {
      step <- current_step()
      if (step > 1) {
        current_step(step - 1)
      }
    })

    shiny::observeEvent(input$wizard_next, {
      step <- current_step()

      # Validate before proceeding
      can_proceed <- FALSE

      if (step == 1) {
        # Need data uploaded
        if (!is.null(step1_data())) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please upload a file first."),
            type = "warning"
          )
        }
      } else if (step == 2) {
        # Need valid mappings
        if (step2_valid()) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please map all required columns."),
            type = "warning"
          )
        }
      } else if (step == 3) {
        # Lookup analysis must be run (matched_data exists)
        if (!is.null(step3_results())) {
          can_proceed <- TRUE
        } else {
          shiny::showNotification(
            i18n_reactive()$t("Please run the lookup analysis first."),
            type = "warning"
          )
        }
      }

      if (can_proceed && step < 4) {
        current_step(step + 1)
      }
    })

    # Update button states based on current step
    shiny::observe({
      step <- current_step()
      shinyjs::toggleState("wizard_back", condition = step > 1)
      shinyjs::toggleState("wizard_next", condition = step < 4)

      # Update button text on last step
      if (step == 4) {
        shinyjs::hide("wizard_next")
      } else {
        shinyjs::show("wizard_next")
      }
    })

    # ===== SESSION CLEANUP =====
    session$onSessionEnded(function() {
      tryCatch({
        cleanup_connections()
      }, error = function(e) {
        cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
      })
      shiny::stopApp()
    })
  }

  # Run the app
  shiny::shinyApp(ui = ui, server = server)
}


#' Specimen Import Wizard (Internal Function)
#'
#' Internal function that creates the Shiny app object without launching it.
#' Useful for testing or embedding in other applications.
#'
#' @param lang Character, initial language
#'
#' @return Shiny app object
#' @keywords internal
shiny_app_specimen_import_wizard <- function(lang = "fr") {
  launch_specimen_import_wizard(lang = lang)
}
