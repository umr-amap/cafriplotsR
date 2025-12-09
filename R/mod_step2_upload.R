# Import Wizard - Step 2: Upload Data or Download Template
#
# Module for uploading user data or downloading a template

#' Step 2 Module: Upload Data - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step2_upload_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  # Get translator function
  tr <- function(text) i18n$t(text)

  shiny::tagList(
    shiny::h3(
      shiny::icon("cloud-upload-alt"),
      i18n$t("Step 2: Upload Data or Download Template"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    # Two-column layout
    shiny::fluidRow(
      # Left column: Template download
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background: #f8f9fa; border: 2px solid #dee2e6; border-radius: 8px; height: 100%;",

          shiny::h4(
            shiny::icon("file-download", style = "color: #007bff;"),
            paste0(" ", i18n$t("Option 1: Download Template"))
          ),

          shiny::p(
            i18n$t("Start with a pre-formatted template to ensure your data matches the required structure."),
            style = "color: #6c757d;"
          ),

          shiny::hr(),

          # Template options (dynamic based on import type)
          shiny::uiOutput(ns("template_options")),

          shiny::downloadButton(
            ns("download_template"),
            i18n$t("Download Template"),
            class = "btn-primary btn-lg",
            style = "width: 100%; margin-top: 10px;"
          ),

          shiny::div(
            style = "margin-top: 15px; padding: 10px; background: white; border-radius: 4px;",
            shiny::icon("lightbulb", style = "color: #ffc107;"),
            shiny::tags$small(
              paste0(" ", i18n$t("Tip: The template includes column descriptions and validation rules.")),
              style = "color: #6c757d;"
            )
          )
        )
      ),

      # Right column: File upload
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background: #f8f9fa; border: 2px solid #dee2e6; border-radius: 8px; height: 100%;",

          shiny::h4(
            shiny::icon("file-upload", style = "color: #28a745;"),
            paste0(" ", i18n$t("Option 2: Upload Your Data"))
          ),

          shiny::p(
            i18n$t("Upload an existing Excel or CSV file with your data."),
            style = "color: #6c757d;"
          ),

          shiny::hr(),

          # File upload
          shiny::fileInput(
            ns("file_upload"),
            NULL,
            accept = c(".xlsx", ".xls", ".csv"),
            placeholder = i18n$t("No file selected"),
            buttonLabel = i18n$t("Browse..."),
            width = "100%"
          ),

          shiny::div(
            class = "alert alert-secondary",
            style = "font-size: 14px;",
            shiny::strong(paste0(i18n$t("Supported formats"), ":")),
            shiny::tags$ul(
              style = "margin-bottom: 0;",
              shiny::tags$li(i18n$t("Excel: .xlsx, .xls")),
              shiny::tags$li(i18n$t("CSV: .csv (UTF-8 encoding)"))
            )
          ),

          shiny::div(
            class = "alert alert-warning",
            style = "font-size: 14px;",
            shiny::icon("exclamation-triangle"),
            shiny::strong(paste0(" ", i18n$t("Maximum file size"), ": ")),
            "100 MB"
          )
        )
      )
    ),

    # Data preview section
    shiny::uiOutput(ns("data_preview_section"))
  )
}


#' Step 2 Module: Upload Data - Server
#'
#' @param id Module namespace ID
#' @param import_type Reactive value containing import type ("plots" or "individuals")
#' @param config Reactive value containing import configuration
#' @param con Reactive database connection pool
#' @param i18n Translator object from shiny.i18n
#' @return Reactive value containing uploaded data (data frame)
#' @keywords internal
mod_step2_upload_server <- function(id, import_type, config, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Store uploaded data
    uploaded_data <- shiny::reactiveVal(NULL)

    # Dynamic template options based on import type
    output$template_options <- shiny::renderUI({
      shiny::req(import_type())

      if (import_type() == "plots") {
        # Plot metadata template options
        # Get translator function from parent scope
        tr <- shiny::isolate(i18n()$t)

        shiny::tagList(
          shiny::radioButtons(
            session$ns("template_type"),
            tr("Template Type:"),
            choices = stats::setNames(
              c("minimal", "permanent_plot", "transect", "full"),
              c(
                tr("Minimal (Required fields only)"),
                tr("Permanent Plot (Recommended)"),
                tr("Transect Survey"),
                tr("Full (All optional fields)")
              )
            ),
            selected = "permanent_plot"
          ),
          shiny::checkboxInput(
            session$ns("with_examples"),
            tr("Include example data"),
            value = TRUE
          )
        )
      } else {
        # Individual tree template options
        # Get translator function from parent scope
        tr <- shiny::isolate(i18n()$t)

        shiny::tagList(
          shiny::checkboxInput(
            session$ns("with_examples"),
            tr("Include features sheet (traits/measurements)"),
            value = TRUE
          ),
          shiny::div(
            class = "alert alert-info",
            style = "font-size: 14px; margin-top: 15px;",
            shiny::icon("info-circle"),
            shiny::strong(paste0(" ", tr("Note:"), " ")),
            tr("Individual template includes all possible columns."),
            " ",
            tr("Features sheet contains trait measurements (DBH, height, etc.).")
          ),
          # Hidden field for template_type to satisfy download handler
          shiny::tags$input(
            type = "hidden",
            id = session$ns("template_type"),
            value = "full"
          )
        )
      }
    })

    # Template download handler
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

        if (import_type() == "plots") {
          type <- input$template_type
          sprintf("cafriplot_template_%s_%s.xlsx", type, timestamp)
        } else {
          sprintf("cafriplot_individuals_template_%s.xlsx", timestamp)
        }
      },
      content = function(file) {
        shiny::withProgress(message = i18n()$t('Generating template...'), value = 0, {

          shiny::incProgress(0.3, detail = i18n()$t("Creating structure..."))

          # Generate template based on import type
          template <- tryCatch({
            if (import_type() == "plots") {
              get_plot_metadata_template(
                template_type = input$template_type,
                with_examples = input$with_examples
              )
            } else {
              # For individuals, generate individual tree template
              # Get a regular connection from the pool
              db_pool <- con()
              db_con <- pool::poolCheckout(db_pool)
              on.exit(pool::poolReturn(db_con), add = TRUE)

              get_individual_template(
                method = NULL,  # NULL = all columns
                include_features = isTRUE(input$with_examples),  # Ensure boolean
                con = db_con,  # Pass checked-out connection
                return_data = TRUE
              )
            }
          }, error = function(e) {
            shiny::showNotification(
              paste(i18n()$t("Error generating template:"), e$message),
              type = "error",
              duration = NULL
            )
            return(NULL)
          })

          # Check if template was generated
          if (is.null(template)) {
            stop(i18n()$t("Template generation failed"))
          }

          shiny::incProgress(0.6, detail = i18n()$t("Writing Excel file..."))

          # Write to Excel
          writexl::write_xlsx(template, file)

          shiny::incProgress(1, detail = i18n()$t("Complete!"))
        })

        shiny::showNotification(
          i18n()$t("Template downloaded successfully!"),
          type = "message",
          duration = 3
        )
      }
    )

    # File upload handler
    shiny::observeEvent(input$file_upload, {
      shiny::req(input$file_upload)

      file_path <- input$file_upload$datapath
      file_ext <- tools::file_ext(input$file_upload$name)

      # Load data based on file type
      data <- tryCatch({
        shiny::withProgress(message = i18n()$t('Loading file...'), value = 0, {

          shiny::incProgress(0.3, detail = i18n()$t("Reading file..."))

          result <- if (file_ext %in% c("xlsx", "xls")) {
            readxl::read_excel(file_path)
          } else if (file_ext == "csv") {
            read.csv(file_path, stringsAsFactors = FALSE)
          } else {
            stop(sprintf(i18n()$t("Unsupported file format: %s"), file_ext))
          }

          shiny::incProgress(0.8, detail = i18n()$t("Validating structure..."))

          # Convert to data frame (in case it's a tibble)
          result <- as.data.frame(result)

          shiny::incProgress(1, detail = i18n()$t("Complete!"))

          result
        })
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error loading file:"), e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })

      if (!is.null(data)) {
        uploaded_data(data)

        shiny::showNotification(
          sprintf(i18n()$t("File loaded: %d rows, %d columns"), nrow(data), ncol(data)),
          type = "message",
          duration = 4
        )
      }
    })

    # Data preview section
    output$data_preview_section <- shiny::renderUI({
      shiny::req(uploaded_data())

      data <- uploaded_data()

      shiny::div(
        style = "margin-top: 30px;",

        shiny::h4(
          shiny::icon("table"),
          " Data Preview",
          style = "color: #495057;"
        ),

        # Summary statistics
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::div(
              class = "alert alert-info",
              style = "text-align: center;",
              shiny::h3(nrow(data), style = "margin: 0;"),
              shiny::p("Rows", style = "margin: 0; color: #6c757d;")
            )
          ),
          shiny::column(
            4,
            shiny::div(
              class = "alert alert-info",
              style = "text-align: center;",
              shiny::h3(ncol(data), style = "margin: 0;"),
              shiny::p("Columns", style = "margin: 0; color: #6c757d;")
            )
          ),
          shiny::column(
            4,
            shiny::div(
              class = "alert alert-info",
              style = "text-align: center;",
              shiny::h3(
                format(object.size(data), units = "auto"),
                style = "margin: 0; font-size: 24px;"
              ),
              shiny::p("Size", style = "margin: 0; color: #6c757d;")
            )
          )
        ),

        # Column names
        shiny::div(
          class = "alert alert-secondary",
          shiny::strong("Column names: "),
          shiny::tags$code(paste(names(data), collapse = ", "))
        ),

        # Interactive data table
        shiny::div(
          style = "margin-top: 15px;",
          DT::dataTableOutput(session$ns("preview_table"))
        )
      )
    })

    # Render data table
    output$preview_table <- DT::renderDataTable({
      shiny::req(uploaded_data())

      DT::datatable(
        head(uploaded_data(), 100),  # Limit to first 100 rows for preview
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = "Showing first 100 rows"
      )
    })

    # Return uploaded data
    return(uploaded_data)
  })
}
