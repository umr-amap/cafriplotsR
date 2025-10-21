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
#' @param language Reactive returning current language ("en" or "fr")
#'
#' @return Reactive data.frame with user data
#'
#' @keywords internal
mod_data_input_server <- function(id, provided_data = NULL, language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reactive values
    user_data <- shiny::reactiveVal(NULL)
    file_name <- shiny::reactiveVal(NULL)

    # Get translations
    t <- shiny::reactive({
      get_translations(language())
    })

    # Module title
    output$title <- shiny::renderText({
      t()$data_input_title
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
            shiny::p(t()$data_using_r_data, style = "font-weight: bold;")
          )
        } else {
          # Show file upload
          shiny::fileInput(
            inputId = ns("file_upload"),
            label = t()$data_upload_file,
            accept = c(".xlsx", ".xls"),
            placeholder = t()$data_choose_file
          )
        }
      } else {
        # Show file upload
        shiny::fileInput(
          inputId = ns("file_upload"),
          label = t()$data_upload_file,
          accept = c(".xlsx", ".xls"),
          placeholder = t()$data_choose_file
        )
      }
    })

    # Handle file upload
    shiny::observeEvent(input$file_upload, {
      req(input$file_upload)

      shinybusy::show_spinner()

      tryCatch({
        # Read Excel file
        data <- readxl::read_xlsx(input$file_upload$datapath, sheet = 1)

        # Add id_data column if not present
        if (!"id_data" %in% colnames(data)) {
          data <- data %>%
            dplyr::mutate(id_data = seq(1, nrow(.), 1))
        }

        # Store data
        user_data(data)
        file_name(input$file_upload$name)

        shinybusy::hide_spinner()

        shiny::showNotification(
          t()$msg_file_uploaded,
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        shinybusy::hide_spinner()

        shiny::showNotification(
          paste(t()$msg_error, e$message),
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
          paste(nrow(data), t()$data_rows, ",", ncol(data), t()$data_columns)
        )
      )
    })

    # Return reactive data
    return(user_data)
  })
}
