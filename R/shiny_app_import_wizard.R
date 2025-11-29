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
#' @export
launch_import_wizard <- function(launch_browser = TRUE) {

  # Check required packages
  required_pkgs <- c("shiny", "shinyjs", "DT")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(sprintf(
      "Required packages missing: %s\nInstall with: install.packages(c(%s))",
      paste(missing_pkgs, collapse = ", "),
      paste(sprintf("'%s'", missing_pkgs), collapse = ", ")
    ))
  }

  # Launch app
  shiny::shinyApp(
    ui = import_wizard_ui(),
    server = import_wizard_server,
    options = list(launch.browser = launch_browser)
  )
}


#' UI for Import Wizard
#' @keywords internal
import_wizard_ui <- function() {

  shiny::fluidPage(
    # Enable shinyjs for JavaScript interactions
    shinyjs::useShinyjs(),

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
  )
}


#' Server for Import Wizard
#' @keywords internal
import_wizard_server <- function(input, output, session) {

  # Reactive values to store state
  rv <- reactiveValues(
    step = 1,
    max_step_reached = 1,  # Track progress
    import_type = NULL,
    config = NULL,
    data = NULL,
    mappings = NULL,
    validation = NULL,
    dry_run_result = NULL,
    import_result = NULL
  )

  # Database connection (initialized in Step 1)
  con <- reactiveVal(NULL)

  # Initialize connection on app start
  observe({
    tryCatch({
      con(call.mydb())
    }, error = function(e) {
      showNotification(
        paste("Database connection failed:", e$message),
        type = "error",
        duration = NULL
      )
    })
  })

  # Cleanup on session end
  session$onSessionEnded(function() {
    if (!is.null(con())) {
      tryCatch({
        DBI::dbDisconnect(con())
      }, error = function(e) {
        message("Error disconnecting: ", e$message)
      })
    }
  })

  # Step indicator UI
  output$step_indicator <- renderUI({
    step_labels <- c(
      "Choose Type",
      "Upload Data",
      "Map Columns",
      "Validate",
      "Preview",
      "Import"
    )

    div(
      class = "step-indicator",
      lapply(1:6, function(i) {
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
      "1" = mod_step1_choose_type_ui("step1"),
      "2" = mod_step2_upload_ui("step2"),
      "3" = tagList(h3("Step 3: Map Columns"), p("Coming soon...")),
      "4" = tagList(h3("Step 4: Validate"), p("Coming soon...")),
      "5" = tagList(h3("Step 5: Preview"), p("Coming soon...")),
      "6" = tagList(h3("Step 6: Import"), p("Coming soon..."))
    )
  })

  # Navigation buttons
  output$back_button <- renderUI({
    if (rv$step > 1) {
      actionButton(
        "btn_back",
        label = tagList(icon("arrow-left"), " Back"),
        class = "btn btn-secondary btn-lg"
      )
    }
  })

  output$next_button <- renderUI({
    # Disable if step not ready
    disabled <- !can_proceed_to_next_step()

    # Change label on final step
    label <- if (rv$step == 6) {
      tagList(icon("check"), " Execute Import")
    } else {
      tagList("Next ", icon("arrow-right"))
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
      "3" = !is.null(rv$mappings),
      "4" = !is.null(rv$validation) && rv$validation$valid,
      "5" = !is.null(rv$dry_run_result),
      "6" = FALSE  # Import step - different logic
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

  # Step 1: Choose import type
  step1_result <- mod_step1_choose_type_server("step1")

  observeEvent(step1_result(), {
    req(step1_result())

    rv$import_type <- step1_result()

    # Load configuration
    tryCatch({
      rv$config <- get_import_column_routing(rv$import_type, con = con())

      showNotification(
        paste("Configuration loaded for:", rv$import_type),
        type = "message"
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading configuration:", e$message),
        type = "error"
      )
    })
  })

  # Step 2: Upload data
  step2_result <- mod_step2_upload_server("step2", config = reactive(rv$config))

  observeEvent(step2_result(), {
    req(step2_result())
    rv$data <- step2_result()

    showNotification(
      sprintf("Data loaded: %d rows, %d columns", nrow(rv$data), ncol(rv$data)),
      type = "message"
    )
  })

  # TODO: Add remaining step modules (Step 3-6)
}
