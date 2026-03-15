# Import Wizard Shiny App
#
# Interactive step-by-step wizard for importing plot metadata and individual
# tree data into the CafriPlots database.

#' Launch Import Wizard Shiny App
#'
#' Opens an interactive Shiny app that guides users through the complete
#' import workflow: data upload, column mapping, validation, preview, and
#' execution. This wizard wraps the existing import functions in a
#' user-friendly graphical interface.
#'
#' @details
#' The wizard consists of 6 steps:
#' \enumerate{
#'   \item Choose import type (plots or individuals)
#'   \item Upload data or download template
#'   \item Map columns to database schema
#'   \item Validate data quality
#'   \item Preview import (dry run)
#'   \item Execute import
#' }
#'
#' All import logic reuses existing CafriplotsR functions:
#' \itemize{
#'   \item \code{\link{get_plot_metadata_template}} - Template generation
#'   \item \code{\link{map_user_columns}} - Column mapping
#'   \item \code{\link{validate_plot_metadata}} - Validation
#'   \item \code{\link{import_plot_metadata}} - Import execution
#' }
#'
#' @param launch_browser Logical: Open in external browser? (default TRUE)
#'
#' @return Invisibly returns the Shiny app object
#'
#' @examples
#' \dontrun{
#' # Launch the import wizard
#' launch_import_wizard()
#'
#' # Launch in RStudio Viewer pane
#' launch_import_wizard(launch_browser = FALSE)
#' }
#'
#' @param language Character, initial language ("en" or "fr"), default: "fr"
#'
#' @export
launch_import_wizard <- function(launch_browser = TRUE, language = "fr") {

  # Validate parameters
  language <- match.arg(language, c("en", "fr"))

  # Check required packages
  required_pkgs <- c("shiny", "shinyjs", "DT", "shiny.i18n")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(sprintf(
      "Required packages missing: %s\nInstall with: install.packages(c(%s))",
      paste(missing_pkgs, collapse = ", "),
      paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
    ))
  }

  # Initialize translator (must be before UI for usei18n)
  translator <- init_translator()

  # Launch app
  shiny::shinyApp(
    ui = import_wizard_ui(translator, language),
    server = function(input, output, session) {
      import_wizard_server(input, output, session, translator)
    },
    options = list(launch.browser = launch_browser)
  )
}


#' UI for Import Wizard
#' @keywords internal
import_wizard_ui <- function(translator, language = "fr") {

  shiny::tagList(
    # Add shiny.i18n (required for automatic translation)
    shiny.i18n::usei18n(translator),

    # Enable shinyjs for JavaScript interactions
    shinyjs::useShinyjs(),

  shiny::fluidPage(
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

    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Step indicator styling */
        .step-indicator {
          display: flex;
          justify-content: space-between;
          margin-bottom: 30px;
          padding: 20px 0;
        }
        .step {
          flex: 1;
          text-align: center;
          padding: 15px 10px;
          background: #f8f9fa;
          border-radius: 8px;
          margin: 0 5px;
          border: 2px solid #dee2e6;
          transition: all 0.3s;
          font-weight: 500;
        }
        .step.active {
          background: #007bff;
          color: white;
          border-color: #007bff;
          box-shadow: 0 4px 6px rgba(0,123,255,0.3);
        }
        .step.completed {
          background: #28a745;
          color: white;
          border-color: #28a745;
        }

        /* Column mapping styling */
        .mapping-row {
          padding: 12px;
          margin: 8px 0;
          border-left: 4px solid #6c757d;
          background: #f8f9fa;
          border-radius: 4px;
          transition: all 0.2s;
        }
        .mapping-row:hover {
          background: #e9ecef;
        }
        .confidence-10, .confidence-9 {
          border-left-color: #28a745;
        }
        .confidence-8, .confidence-7 {
          border-left-color: #ffc107;
        }
        .confidence-6, .confidence-5, .confidence-4 {
          border-left-color: #dc3545;
        }

        /* Card styling */
        .import-card {
          border: 2px solid #dee2e6;
          border-radius: 8px;
          padding: 20px;
          margin: 15px 0;
          transition: all 0.3s;
          cursor: pointer;
        }
        .import-card:hover {
          border-color: #007bff;
          box-shadow: 0 4px 12px rgba(0,123,255,0.2);
          transform: translateY(-2px);
        }
        .import-card.selected {
          border-color: #007bff;
          background: #e7f3ff;
        }
        .import-card h4 {
          margin-top: 0;
        }

        /* Alert styling improvements */
        .alert {
          border-radius: 8px;
          border-left: 4px solid;
        }

        /* Button styling */
        .btn {
          border-radius: 6px;
          font-weight: 500;
          padding: 10px 24px;
        }

        /* Navigation button container */
        .nav-buttons {
          margin-top: 30px;
          padding-top: 20px;
          border-top: 2px solid #dee2e6;
        }
      "))
    ),

    # Login panel (shown before authentication)
    shiny::conditionalPanel(
      condition = "!output.authenticated",
      mod_database_login_ui("login")
    ),

    # Main app content (shown after authentication)
    shiny::conditionalPanel(
      condition = "output.authenticated",

      # Header
      div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; margin-bottom: 30px; color: white; border-radius: 0 0 12px 12px;",
        fluidRow(
          column(12,
            h2(
              icon("cloud-upload-alt", style = "margin-right: 10px;"),
              "CafriPlots Import Wizard",
              style = "margin: 0; font-weight: bold;"
            ),
            p("Step-by-step guide for importing plot and tree data", style = "margin: 5px 0 0 0; opacity: 0.9;")
          )
        )
      ),

      # Main content container
      div(
        class = "container-fluid",
        style = "max-width: 1200px; margin: 0 auto;",

        # Step indicator
        uiOutput("step_indicator"),

        # Main content area (changes based on step)
        div(
          style = "min-height: 400px;",
          uiOutput("step_content")
        ),

        # Navigation buttons
        div(
          class = "nav-buttons",
          fluidRow(
            column(6,
              uiOutput("back_button")
            ),
            column(6,
              uiOutput("next_button"),
              align = "right"
            )
          )
        )
      ),

      # Footer
      hr(),
      div(
        style = "text-align: center; color: #6c757d; padding: 20px 0;",
        p(
          "CafriplotsR Import Wizard v1.0 |",
          tags$a(href = "https://github.com/your-repo", "Documentation", target = "_blank"),
          style = "margin: 0;"
        )
      )
    ) # End conditionalPanel for main app
  ) # End fluidPage
  ) # End tagList
}


#' Server for Import Wizard
#' @keywords internal
import_wizard_server <- function(input, output, session, translator) {

  # Database authentication using login module
  login_output <- mod_database_login_server("login")
  pool_main_reactive <- login_output$pool_main
  pool_taxa_reactive <- login_output$pool_taxa
  authenticated_reactive <- login_output$authenticated

  # Sync language from login module to app language selector
  shiny::observe({
    lang <- login_output$language()
    shiny::req(lang)
    shiny::updateSelectInput(session, "selected_language", selected = lang)
  })

  # Output for conditional panel (needs to be suspendable=FALSE)
  output$authenticated <- shiny::reactive({
    authenticated_reactive()
  })
  shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  # Create reactive translator (shiny.i18n recommended pattern)
  i18n <- shiny::reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })

  # Cleanup on session end
  session$onSessionEnded(function() {
    # Clean up all connections and credentials
    tryCatch({
      cleanup_connections()
    }, error = function(e) {
      cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
    })
    shiny::stopApp()
  })

  # Reactive values to store state
  rv <- reactiveValues(
    step = 1,
    max_step_reached = 1,  # Track progress
    import_type = NULL,
    config = NULL,
    data = NULL,
    mapping_result = NULL,  # Contains mappings and validation from Step 3
    mappings = NULL,        # Extracted mappings for later steps
    matched_data_result = NULL,  # Result from Step 4 (lookup matching)
    matched_data = NULL,    # Matched data to use in validation
    validation = NULL,
    dry_run_result = NULL,
    import_result = NULL,
    modules_initialized = FALSE
  )

  # Step indicator UI
  output$step_indicator <- renderUI({
    # Determine max steps based on import type
    max_steps <- if (!is.null(rv$import_type) && rv$import_type == "plots") 8 else 7

    step_labels <- c(
      i18n()$t("Choose Type"),
      i18n()$t("Upload Data"),
      i18n()$t("Map Columns"),
      i18n()$t("Match Lookups"),
      i18n()$t("Validate"),
      i18n()$t("Preview"),
      i18n()$t("Import"),
      i18n()$t("Census Info")  # Step 8 - only for plots
    )

    div(
      class = "step-indicator",
      lapply(1:max_steps, function(i) {
        class_name <- if (i < rv$step) {
          "step completed"
        } else if (i == rv$step) {
          "step active"
        } else {
          "step"
        }

        # Add checkmark for completed steps
        icon_html <- if (i < rv$step) {
          tags$div(icon("check-circle"), br(), step_labels[i])
        } else if (i == rv$step) {
          tags$div(icon("arrow-circle-right"), br(), step_labels[i])
        } else {
          tags$div(icon("circle"), br(), step_labels[i])
        }

        div(class = class_name, icon_html)
      })
    )
  })

  # Step content (load appropriate module)
  output$step_content <- renderUI({
    switch(
      as.character(rv$step),
      "1" = mod_step1_choose_type_ui("step1", i18n()),
      "2" = mod_step2_upload_ui("step2", i18n()),
      "3" = mod_step3_mapping_ui("step3", i18n()),
      "4" = mod_step4_lookup_matching_ui("step4", i18n()),
      "5" = mod_step5_validation_ui("step5", i18n()),
      "6" = mod_step6_preview_ui("step6", i18n()),
      "7" = mod_step7_import_ui("step7", i18n()),
      "8" = mod_census_information_ui("step8", i18n())
    )
  })

  # Navigation buttons
  output$back_button <- renderUI({
    if (rv$step > 1) {
      actionButton(
        "btn_back",
        label = tagList(icon("arrow-left"), " ", i18n()$t("Back")),
        class = "btn btn-secondary btn-lg"
      )
    }
  })

  output$next_button <- renderUI({
    # Disable if step not ready
    disabled <- !can_proceed_to_next_step()

    # Determine max steps based on import type
    max_steps <- if (!is.null(rv$import_type) && rv$import_type == "plots") 8 else 7

    # Hide next button on final step (Step 8 for plots, Step 7 for individuals)
    if (rv$step >= max_steps) {
      return(NULL)
    }

    # Change label based on step
    label <- if (rv$step == 7 && rv$import_type == "individuals") {
      tagList(icon("check"), " ", i18n()$t("Execute Import"))
    } else if (rv$step == 7 && rv$import_type == "plots") {
      tagList(i18n()$t("Next: Census Info (Optional)"), " ", icon("arrow-right"))
    } else {
      tagList(i18n()$t("Next"), " ", icon("arrow-right"))
    }

    actionButton(
      "btn_next",
      label = label,
      class = "btn btn-primary btn-lg",
      disabled = disabled
    )
  })

  # Check if can proceed to next step
  can_proceed_to_next_step <- reactive({
    switch(
      as.character(rv$step),
      "1" = !is.null(rv$import_type),
      "2" = !is.null(rv$data),
      "3" = !is.null(rv$mapping_result) && rv$mapping_result$validation$valid,
      "4" = !is.null(rv$matched_data_result),  # Lookup matching complete
      "5" = !is.null(rv$validation) && rv$validation$valid,  # Validation passed
      "6" = !is.null(rv$validation) && rv$validation$valid,  # Preview (can proceed if validated)
      "7" = {
        # For individuals, Step 7 is final (no next button)
        # For plots, can proceed to Step 8 if import was successful
        if (rv$import_type == "plots") {
          !is.null(rv$import_result) && rv$import_result$success && !rv$import_result$dry_run
        } else {
          FALSE  # Individuals end at Step 7
        }
      },
      "8" = TRUE  # Census step is always optional, can always proceed (but no next button)
    )
  })

  # Back button handler
  observeEvent(input$btn_back, {
    rv$step <- max(1, rv$step - 1)
  })

  # Next button handler
  observeEvent(input$btn_next, {
    if (can_proceed_to_next_step()) {
      rv$step <- rv$step + 1
      rv$max_step_reached <- max(rv$max_step_reached, rv$step)
    } else {
      showNotification(
        "Please complete this step before proceeding",
        type = "warning"
      )
    }
  })

  # Initialize step modules only after authentication (runs once)
  shiny::observe({
    shiny::req(authenticated_reactive() == TRUE)
    shiny::req(pool_main_reactive())
    shiny::req(!rv$modules_initialized)  # Only run once

    cli::cli_alert_info("Initializing import wizard modules...")

    # Step 1: Choose import type
    step1_result <- mod_step1_choose_type_server("step1", i18n)

    observeEvent(step1_result(), {
      req(step1_result())

      rv$import_type <- step1_result()

      # Load configuration using connection pool
      tryCatch({
        # Debug logging
        message("Loading configuration for import type: ", rv$import_type)

        # Get connection pool
        pool <- pool_main_reactive()
        message("Connection pool retrieved: ", class(pool)[1])

        # Load configuration
        rv$config <- get_import_column_routing(rv$import_type, con = pool)

        message("Configuration loaded successfully")

        showNotification(
          paste("Configuration loaded for:", rv$import_type),
          type = "message"
        )
      }, error = function(e) {
        # Detailed error logging
        message("ERROR in configuration loading:")
        message("  Import type: ", rv$import_type)
        message("  Error message: ", e$message)
        message("  Error call: ", deparse(e$call))

        # Print full traceback
        traceback_msg <- paste(capture.output(traceback()), collapse = "\n")
        message("  Traceback:\n", traceback_msg)

        showNotification(
          paste("Error loading configuration:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Step 2: Upload data
    step2_result <- mod_step2_upload_server(
      "step2",
      import_type = reactive(rv$import_type),
      config = reactive(rv$config),
      con = pool_main_reactive,
      i18n = i18n
    )

    observeEvent(step2_result(), {
      req(step2_result())

      # Reset all downstream results when new data is uploaded
      rv$mappings <- NULL
      rv$mapping_result <- NULL
      rv$validation <- NULL
      rv$lookup_result <- NULL
      rv$import_result <- NULL

      # Update data
      rv$data <- step2_result()

      showNotification(
        sprintf("Data loaded: %d rows, %d columns", nrow(rv$data), ncol(rv$data)),
        type = "message"
      )

      cli::cli_alert_info("New data uploaded - all downstream results reset")
    })

    # Step 3: Column mapping
    step3_result <- mod_step3_mapping_server(
      "step3",
      data = reactive(rv$data),
      config = reactive(rv$config),
      con = pool_main_reactive,
      i18n = i18n
    )

    observeEvent(step3_result(), {
      req(step3_result())

      rv$mapping_result <- step3_result()
      rv$mappings <- step3_result()$mappings_with_skips  # Use full mappings including NA for skipped columns

      # Check if config needs refresh (new table_colnam feature was created)
      if (!is.null(step3_result()$needs_config_refresh) && step3_result()$needs_config_refresh) {
        cli::cli_alert_info("Config refresh triggered - reloading metadata mappings for new table_colnam feature")

        # Refresh only the metadata_mappings part of config
        tryCatch({
          pool <- pool_main_reactive()
          updated_mappings <- get_metadata_mappings_plots(pool)
          rv$config$metadata_mappings <- updated_mappings

          cli::cli_alert_success("Config refreshed - new feature will be available in Step 4 lookup")

          showNotification(
            "New people feature created - it will be available for lookup matching in Step 4",
            type = "message",
            duration = 5
          )
        }, error = function(e) {
          cli::cli_alert_warning("Failed to refresh config: {e$message}")
        })
      }

      if (step3_result()$validation$valid) {
        showNotification(
          sprintf("Column mapping complete: %d columns mapped", length(rv$mappings)),
          type = "message"
        )
      }
    })

    # Step 4: Lookup matching
    step4_result <- mod_step4_lookup_matching_server(
      "step4",
      data = reactive(rv$data),
      mappings = reactive(rv$mappings),
      config = reactive(rv$config),
      con = pool_main_reactive,
      i18n = i18n
    )

    observeEvent(step4_result$complete(), {
      req(step4_result$complete() == TRUE)

      rv$matched_data_result <- step4_result
      rv$matched_data <- step4_result$data()

      showNotification(
        "Lookup matching complete!",
        type = "message",
        duration = 3
      )
    })

    # Step 5: Validation (uses matched data from Step 4)
    step5_result <- mod_step5_validation_server(
      "step5",
      data = reactive({
        # Use matched data if available, otherwise use original data
        if (!is.null(rv$matched_data)) rv$matched_data else rv$data
      }),
      mappings = reactive(rv$mappings),
      config = reactive(rv$config),
      con = pool_main_reactive,
      i18n = i18n
    )

    observeEvent(step5_result(), {
      req(step5_result())

      rv$validation <- step5_result()

      if (step5_result()$valid) {
        showNotification(
          "Data validation passed!",
          type = "message",
          duration = 3
        )
      }
    })

    # Step 6: Preview
    step6_result <- mod_step6_preview_server(
      "step6",
      validation_result = reactive(rv$validation),
      i18n = i18n
    )

    # Step 7: Import execution
    step7_result <- mod_step7_import_server(
      "step7",
      validation_result = reactive(rv$validation),
      mappings = reactive(rv$mappings),
      config = reactive(rv$config),
      con = pool_main_reactive,
      i18n = i18n
    )

    observeEvent(step7_result(), {
      req(step7_result())

      result <- step7_result()

      if (result$success && !result$dry_run) {
        rv$import_result <- result
        cli::cli_alert_success("Import completed: {result$n_plots} plots imported")

        # For plots, automatically move to census step after successful import
        if (!is.null(rv$import_type) && rv$import_type == "plots") {
          cli::cli_alert_info("Plot import successful - proceeding to optional census information step")
          # User can click Next to go to Step 8 or finish here
        }
      }
    })

    # Step 8: Census Information (conditional - only for plots)
    step8_result <- mod_census_information_server(
      "step8",
      imported_plots = reactive({
        if (!is.null(rv$import_result) && !is.null(rv$import_result$imported_plots)) {
          rv$import_result$imported_plots
        } else {
          NULL
        }
      }),
      con = pool_main_reactive,
      i18n = i18n
    )

    observeEvent(step8_result(), {
      req(step8_result())

      result <- step8_result()

      if (!is.null(result) && result$success) {
        cli::cli_alert_success("Census information added successfully")

        showNotification(
          i18n()$t("Census information has been added to your plots!"),
          type = "message",
          duration = 5
        )
      }
    })

    # Mark modules as initialized
    rv$modules_initialized <- TRUE
    cli::cli_alert_success("Import wizard modules initialized successfully!")
  })
}
