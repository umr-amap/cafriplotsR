# Data Input Module
#
# Handles file upload or direct R data input

#' Data Input Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_data_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4(shiny::textOutput(ns("title"))),
    shiny::uiOutput(ns("input_controls")),
    shiny::uiOutput(ns("data_summary"))
  )
}


#' Data Input Module - Server
#'
#' @param id Character, module ID
#' @param provided_data Reactive or data.frame, optional pre-loaded data
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return Reactive data.frame with user data
#'
#' @keywords internal
mod_data_input_server <- function(id, provided_data = NULL, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    user_data <- shiny::reactiveVal(NULL)
    file_name <- shiny::reactiveVal(NULL)
    excel_sheets <- shiny::reactiveVal(NULL)
    uploaded_file_path <- shiny::reactiveVal(NULL)

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Data Input")
    })

    # Input controls
    output$input_controls <- shiny::renderUI({
      ns <- session$ns

      # If data is pre-provided
      if (!is.null(provided_data)) {
        # Handle both reactive and static data
        data_to_check <- if (shiny::is.reactive(provided_data)) {
          provided_data()
        } else {
          provided_data
        }

        if (!is.null(data_to_check) && nrow(data_to_check) > 0) {
          shiny::div(
            shiny::icon("check-circle", class = "fa-2x", style = "color: green;"),
            shiny::p(i18n()$t("Using R data from environment"), style = "font-weight: bold;")
          )
        } else {
          # Show file upload
          shiny::tagList(
            shiny::fileInput(
              inputId = ns("file_upload"),
              label = i18n()$t("Upload Excel file"),
              accept = c(".xlsx", ".xls", ".csv"),
              placeholder = i18n()$t("Choose file...")
            ),
            shiny::uiOutput(ns("sheet_selector"))
          )
        }
      } else {
        # Show file upload
        shiny::tagList(
          shiny::fileInput(
            inputId = ns("file_upload"),
            label = i18n()$t("Upload Excel file"),
            accept = c(".xlsx", ".xls", ".csv"),
            placeholder = i18n()$t("Choose file...")
          ),
          shiny::uiOutput(ns("sheet_selector"))
        )
      }
    })

    # Sheet selector UI (only for Excel files)
    output$sheet_selector <- shiny::renderUI({
      req(excel_sheets())

      ns <- session$ns

      shiny::div(
        style = "margin-top: -10px; margin-bottom: 10px;",
        shiny::selectInput(
          inputId = ns("excel_sheet"),
          label = "Select sheet:",
          choices = excel_sheets(),
          selected = excel_sheets()[1]
        )
      )
    })

    # Handle file upload - detect file type
    shiny::observeEvent(input$file_upload, {
      req(input$file_upload)

      file_path <- input$file_upload$datapath
      file_ext <- tools::file_ext(input$file_upload$name)

      uploaded_file_path(file_path)
      file_name(input$file_upload$name)

      tryCatch({
        # Check if Excel file
        if (file_ext %in% c("xlsx", "xls")) {
          # Get sheet names
          sheets <- readxl::excel_sheets(file_path)
          excel_sheets(sheets)

          # Don't load data yet - wait for sheet selection
          # Reset user_data to trigger sheet selector
          user_data(NULL)

        } else if (file_ext == "csv") {
          # Read CSV file directly
          shinybusy::show_spinner()

          data <- readr::read_csv(file_path, show_col_types = FALSE)

          # Add id_data column if not present
          if (!"id_data" %in% colnames(data)) {
            data <- data %>%
              dplyr::mutate(id_data = seq(1, nrow(.), 1))
          }

          user_data(data)
          excel_sheets(NULL)  # No sheet selector for CSV

          shinybusy::hide_spinner()

          shiny::showNotification(
            i18n()$t("File uploaded successfully"),
            type = "message",
            duration = 3
          )
        } else {
          shiny::showNotification(
            "Unsupported file format. Please upload .xlsx, .xls, or .csv file.",
            type = "error",
            duration = 5
          )
        }

      }, error = function(e) {
        shinybusy::hide_spinner()

        shiny::showNotification(
          paste(i18n()$t("Error:"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Handle sheet selection for Excel files
    shiny::observeEvent(input$excel_sheet, {
      req(uploaded_file_path())
      req(input$excel_sheet)

      shinybusy::show_spinner()

      tryCatch({
        # Read selected sheet from Excel file
        data <- readxl::read_xlsx(uploaded_file_path(), sheet = input$excel_sheet, guess_max = 30000)

        # Add id_data column if not present
        if (!"id_data" %in% colnames(data)) {
          data <- data %>%
            dplyr::mutate(id_data = seq(1, nrow(.), 1))
        }

        user_data(data)

        shinybusy::hide_spinner()

        shiny::showNotification(
          paste0(i18n()$t("File uploaded successfully"), " (Sheet: ", input$excel_sheet, ")"),
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        shinybusy::hide_spinner()

        shiny::showNotification(
          paste(i18n()$t("Error:"), e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Handle pre-provided data
    shiny::observe({
      if (!is.null(provided_data)) {
        data_to_use <- if (shiny::is.reactive(provided_data)) {
          provided_data()
        } else {
          provided_data
        }

        if (!is.null(data_to_use) && nrow(data_to_use) > 0) {
          # Add id_data column if not present
          if (!"id_data" %in% colnames(data_to_use)) {
            data_to_use <- data_to_use %>%
              dplyr::mutate(id_data = seq(1, nrow(.), 1))
          }

          user_data(data_to_use)
          file_name("R data")
        }
      }
    })

    # Data summary
    output$data_summary <- shiny::renderUI({
      req(user_data())

      data <- user_data()

      shiny::div(
        style = "margin-top: 10px; padding: 10px; background-color: #f0f0f0; border-radius: 5px;",
        shiny::p(
          shiny::strong(file_name()),
          shiny::br(),
          paste(nrow(data), i18n()$t("rows"), ",", ncol(data), i18n()$t("columns"))
        )
      )
    })

    # Return reactive data
    return(user_data)
  })
}
