# Specimen Import Wizard - Step 1: Upload Specimen Data
#
# Module for uploading specimen data or downloading a template

#' Specimen Upload Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_specimen_upload_ui <- function(id, i18n) {
 ns <- shiny::NS(id)

 shiny::tagList(
   shiny::h3(
     shiny::icon("cloud-upload-alt"),
     i18n$t("Step 1: Upload Specimen Data"),
     style = "color: #495057; margin-bottom: 20px;"
   ),

   # Important requirement about taxonomic standardization
   shiny::div(
     class = "alert alert-warning",
     style = "margin-bottom: 20px;",
     shiny::h5(
       shiny::icon("exclamation-triangle"),
       " ",
       i18n$t("Important Requirements")
     ),
     shiny::p(i18n$t("Please read and confirm you understand these requirements before proceeding:")),
     shiny::tags$ul(
       shiny::tags$li(
         shiny::HTML(
           paste0(
             "<strong>", i18n$t("Taxonomic standardization required for specimens data:"), "</strong> ",
             i18n$t("Before importing specimen data, you must standardize taxonomic information and obtain idtax_n values. Use the interactive Shiny app for taxonomic matching (launch_taxonomic_match_app()) to standardize your species names. Your data must include an 'idtax_n' column with the standardized taxonomic IDs.")
           )
         )
       )
     )
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
           i18n$t("Download a template with the expected column structure for specimen data."),
           style = "color: #6c757d;"
         ),

         shiny::hr(),

         shiny::div(
           class = "alert alert-info",
           style = "font-size: 14px;",
           shiny::strong(i18n$t("Template columns:")),
           shiny::tags$ul(
             style = "margin-bottom: 0; margin-top: 10px;",
             shiny::tags$li(shiny::tags$code("collector"), " - ", i18n$t("Collector name")),
             shiny::tags$li(shiny::tags$code("colnbr"), " - ", i18n$t("Specimen number")),
             shiny::tags$li(shiny::tags$code("suffix"), " - ", i18n$t("Suffix (optional: A, B, bis...)")),
             shiny::tags$li(shiny::tags$code("idtax_n"), " - ", i18n$t("Taxonomic ID (from taxonomic matching)")),
             shiny::tags$li(shiny::tags$code("det_by"), " - ", i18n$t("Determined by (optional)")),
             shiny::tags$li(shiny::tags$code("det_year"), " - ", i18n$t("Determination year (optional)")),
             shiny::tags$li(shiny::tags$code("det_month"), " - ", i18n$t("Determination month (optional)")),
             shiny::tags$li(shiny::tags$code("det_day"), " - ", i18n$t("Determination day (optional)"))
           )
         ),

         shiny::downloadButton(
           ns("download_template"),
           i18n$t("Download Template"),
           class = "btn-primary btn-lg",
           style = "width: 100%; margin-top: 10px;"
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
           i18n$t("Upload your Excel or CSV file. You'll map columns in the next step."),
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
           class = "alert alert-info",
           style = "font-size: 14px;",
           shiny::icon("info-circle"),
           shiny::strong(paste0(" ", i18n$t("No need to rename columns!"), " ")),
           i18n$t("The wizard will help you match your column names to the database.")
         ),

         shiny::div(
           class = "alert alert-secondary",
           style = "font-size: 14px; margin-top: 10px;",
           shiny::strong(paste0(i18n$t("Supported formats"), ":")),
           shiny::tags$ul(
             style = "margin-bottom: 0;",
             shiny::tags$li(i18n$t("Excel: .xlsx, .xls")),
             shiny::tags$li(i18n$t("CSV: .csv (UTF-8 encoding)"))
           )
         )
       )
     )
   ),

   # Data preview section
   shiny::uiOutput(ns("data_preview_section"))
 )
}


#' Specimen Upload Module - Server
#'
#' @param id Module namespace ID
#' @param con Reactive database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive value containing uploaded data (data frame)
#' @keywords internal
#' @export
mod_specimen_upload_server <- function(id, con, i18n) {
 shiny::moduleServer(id, function(input, output, session) {

   # Store uploaded data
   uploaded_data <- shiny::reactiveVal(NULL)

   # Template download handler
   output$download_template <- shiny::downloadHandler(
     filename = function() {
       timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
       sprintf("specimen_template_%s.xlsx", timestamp)
     },
     content = function(file) {
       shiny::withProgress(message = i18n()$t('Generating template...'), value = 0, {

         shiny::incProgress(0.3, detail = i18n()$t("Creating structure..."))

         # Create template data frame
         # Note: idtax_n values are examples and must be obtained from taxonomic matching
         template <- data.frame(
           collector = c("Dauby", "Dauby", "Smith"),
           colnbr = c(1234, 1235, 567),
           suffix = c("", "A", ""),
           idtax_n = c(12345, 67890, 11223),
           det_by = c("Dauby", "Dauby", "Smith"),
           det_year = c(2024, 2024, 2023),
           det_month = c(6, 6, 3),
           det_day = c(15, 15, 10),
           stringsAsFactors = FALSE
         )

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

         # Convert to data frame
         result <- as.data.frame(result)

         # Add row ID for tracking
         result$`_row_id` <- seq_len(nrow(result))

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
         sprintf(i18n()$t("File loaded: %d rows, %d columns"), nrow(data), ncol(data) - 1),
         type = "message",
         duration = 4
       )
     }
   })

   # Data preview section
   output$data_preview_section <- shiny::renderUI({
     shiny::req(uploaded_data())

     data <- uploaded_data()
     # Remove internal row ID for display
     display_data <- data[, !names(data) %in% "_row_id", drop = FALSE]

     shiny::div(
       style = "margin-top: 30px;",

       shiny::h4(
         shiny::icon("table"),
         paste0(" ", i18n()$t("Data Preview")),
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
             shiny::p(i18n()$t("Rows"), style = "margin: 0; color: #6c757d;")
           )
         ),
         shiny::column(
           4,
           shiny::div(
             class = "alert alert-info",
             style = "text-align: center;",
             shiny::h3(ncol(display_data), style = "margin: 0;"),
             shiny::p(i18n()$t("Columns"), style = "margin: 0; color: #6c757d;")
           )
         ),
         shiny::column(
           4,
           shiny::div(
             class = "alert alert-success",
             style = "text-align: center;",
             shiny::h3(shiny::icon("check"), style = "margin: 0;"),
             shiny::p(i18n()$t("Ready"), style = "margin: 0; color: #6c757d;")
           )
         )
       ),

       # Column names
       shiny::div(
         class = "alert alert-secondary",
         shiny::strong(paste0(i18n()$t("Your columns"), ": ")),
         shiny::tags$code(paste(names(display_data), collapse = ", "))
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

     data <- uploaded_data()
     display_data <- data[, !names(data) %in% "_row_id", drop = FALSE]

     DT::datatable(
       head(display_data, 100),
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
       caption = i18n()$t("Showing first 100 rows")
     )
   })

   # Return uploaded data
   return(uploaded_data)
 })
}
