# Import Wizard - Step 2: Upload Data or Download Template
#
# Module for uploading user data or downloading a template

#' Step 2 Module: Upload Data - UI
#'
#' @param id Module namespace ID
#' @keywords internal
mod_step2_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("cloud-upload-alt"),
      "Step 2: Upload Data or Download Template",
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
            " Option 1: Download Template"
          ),

          shiny::p(
            "Start with a pre-formatted template to ensure your data matches the required structure.",
            style = "color: #6c757d;"
          ),

          shiny::hr(),

          # Template type selection
          shiny::radioButtons(
            ns("template_type"),
            "Template Type:",
            choices = c(
              "Minimal (Required fields only)" = "minimal",
              "Permanent Plot (Recommended)" = "permanent_plot",
              "Transect Survey" = "transect",
              "Full (All optional fields)" = "full"
            ),
            selected = "permanent_plot"
          ),

          shiny::checkboxInput(
            ns("with_examples"),
            "Include example data",
            value = TRUE
          ),

          shiny::downloadButton(
            ns("download_template"),
            "Download Template",
            class = "btn-primary btn-lg",
            style = "width: 100%; margin-top: 10px;"
          ),

          shiny::div(
            style = "margin-top: 15px; padding: 10px; background: white; border-radius: 4px;",
            shiny::icon("lightbulb", style = "color: #ffc107;"),
            shiny::tags$small(
              " Tip: The template includes column descriptions and validation rules.",
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
            " Option 2: Upload Your Data"
          ),

          shiny::p(
            "Upload an existing Excel or CSV file with your data.",
            style = "color: #6c757d;"
          ),

          shiny::hr(),

          # File upload
          shiny::fileInput(
            ns("file_upload"),
            NULL,
            accept = c(".xlsx", ".xls", ".csv"),
            placeholder = "No file selected",
            buttonLabel = "Browse...",
            width = "100%"
          ),

          shiny::div(
            class = "alert alert-secondary",
            style = "font-size: 14px;",
            shiny::strong("Supported formats:"),
            shiny::tags$ul(
              style = "margin-bottom: 0;",
              shiny::tags$li("Excel: .xlsx, .xls"),
              shiny::tags$li("CSV: .csv (UTF-8 encoding)")
            )
          ),

          shiny::div(
            class = "alert alert-warning",
            style = "font-size: 14px;",
            shiny::icon("exclamation-triangle"),
            shiny::strong(" Maximum file size: "),
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
#' @param config Reactive value containing import configuration
#' @return Reactive value containing uploaded data (data frame)
#' @keywords internal
mod_step2_upload_server <- function(id, config) {
  shiny::moduleServer(id, function(input, output, session) {

    # Store uploaded data
    uploaded_data <- shiny::reactiveVal(NULL)

    # Template download handler
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        type <- input$template_type
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        sprintf("cafriplot_template_%s_%s.xlsx", type, timestamp)
      },
      content = function(file) {
        shiny::withProgress(message = 'Generating template...', value = 0, {

          shiny::incProgress(0.3, detail = "Creating structure...")

          # Generate template using existing function
          template <- get_plot_metadata_template(
            template_type = input$template_type,
            with_examples = input$with_examples
          )

          shiny::incProgress(0.6, detail = "Writing Excel file...")

          # Write to Excel
          writexl::write_xlsx(template, file)

          shiny::incProgress(1, detail = "Complete!")
        })

        shiny::showNotification(
          "Template downloaded successfully!",
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
        shiny::withProgress(message = 'Loading file...', value = 0, {

          shiny::incProgress(0.3, detail = "Reading file...")

          result <- if (file_ext %in% c("xlsx", "xls")) {
            readxl::read_excel(file_path)
          } else if (file_ext == "csv") {
            read.csv(file_path, stringsAsFactors = FALSE)
          } else {
            stop("Unsupported file format: ", file_ext)
          }

          shiny::incProgress(0.8, detail = "Validating structure...")

          # Convert to data frame (in case it's a tibble)
          result <- as.data.frame(result)

          shiny::incProgress(1, detail = "Complete!")

          result
        })
      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading file:", e$message),
          type = "error",
          duration = NULL
        )
        return(NULL)
      })

      if (!is.null(data)) {
        uploaded_data(data)

        shiny::showNotification(
          sprintf("File loaded: %d rows, %d columns", nrow(data), ncol(data)),
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
