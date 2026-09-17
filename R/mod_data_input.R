# Data Input Module
#
# Handles file upload, text input (copy-paste), or direct R data input

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
    shiny::uiOutput(ns("input_method_selector")),
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
    input_method <- shiny::reactiveVal("file")  # "file" or "text"

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Data Input")
    })

    # Input method selector (only shown when no pre-provided data)
    output$input_method_selector <- shiny::renderUI({
      # If data is pre-provided, don't show selector
      if (!is.null(provided_data)) {
        data_to_check <- if (shiny::is.reactive(provided_data)) {
          provided_data()
        } else {
          provided_data
        }
        if (!is.null(data_to_check) && nrow(data_to_check) > 0) {
          return(NULL)
        }
      }

      ns <- session$ns

      # Build choices with translations
      method_choices <- c("file", "text")
      names(method_choices) <- c(
        i18n()$t("File upload"),
        i18n()$t("Text input (paste/type)")
      )

      shiny::div(
        style = "margin-bottom: 15px;",
        shiny::radioButtons(
          inputId = ns("input_method"),
          label = i18n()$t("Input method:"),
          choices = method_choices,
          selected = "file",
          inline = TRUE
        )
      )
    })

    # Track input method changes
    shiny::observeEvent(input$input_method, {
      input_method(input$input_method)
    })

    # Input controls (conditional on input method)
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
          return(shiny::div(
            shiny::icon("check-circle", class = "fa-2x", style = "color: green;"),
            shiny::p(i18n()$t("Using R data from environment"), style = "font-weight: bold;")
          ))
        }
      }

      # Get current input method
      current_method <- input$input_method %||% "file"

      if (current_method == "file") {
        # File upload interface
        shiny::tagList(
          shiny::fileInput(
            inputId = ns("file_upload"),
            label = i18n()$t("Upload file (Excel or CSV)"),
            accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt"),
            placeholder = i18n()$t("Choose file...")
          ),
          shiny::uiOutput(ns("sheet_selector"))
        )
      } else {
        # Text input interface
        shiny::tagList(
          shiny::textAreaInput(
            inputId = ns("text_input"),
            label = i18n()$t("Enter or paste taxonomic names:"),
            placeholder = i18n()$t("One name per line, or separated by comma/semicolon/tab"),
            rows = 8,
            width = "100%"
          ),
          shiny::tags$small(
            class = "text-muted",
            style = "display: block; margin-top: -10px; margin-bottom: 10px;",
            i18n()$t("Accepted separators: newline, comma, semicolon, tab")
          ),
          shiny::actionButton(
            inputId = ns("btn_load_text"),
            label = shiny::tagList(shiny::icon("check"), i18n()$t("Load names")),
            class = "btn-primary btn-sm"
          )
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
          label = i18n()$t("Select sheet:"),
          choices = excel_sheets(),
          selected = excel_sheets()[1]
        )
      )
    })

    # Which file and sheet user_data() currently holds, so a sheet selector
    # re-rendered for a new upload does not read the same sheet twice.
    loaded_sheet <- shiny::reactiveVal(NULL)

    # Read one sheet of the uploaded Excel file into user_data()
    load_excel_sheet <- function(path, sheet) {
      shinybusy::show_spinner()
      # A plain function, so on.exit() reliably runs when it returns.
      on.exit(shinybusy::hide_spinner(), add = TRUE)

      tryCatch({
        # read_excel, not read_xlsx: the input also accepts legacy .xls
        data <- readxl::read_excel(path, sheet = sheet, guess_max = 30000)
        user_data(.add_id_data(data))
        loaded_sheet(list(path = path, sheet = sheet))

        shiny::showNotification(
          paste0(i18n()$t("File uploaded successfully"), " (", sheet, ")"),
          type = "message",
          duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error:"), e$message),
          type = "error",
          duration = 10
        )
      })
    }

    # Handle file upload - detect file type
    shiny::observeEvent(input$file_upload, {
      req(input$file_upload)

      file_path <- input$file_upload$datapath
      # Lower-cased: files saved on Windows are often "NAMES.CSV"
      file_ext <- tolower(tools::file_ext(input$file_upload$name))

      uploaded_file_path(file_path)
      file_name(input$file_upload$name)
      loaded_sheet(NULL)

      tryCatch({
        if (file_ext %in% c("xlsx", "xls")) {
          sheets <- readxl::excel_sheets(file_path)
          excel_sheets(sheets)

          # Load the first sheet now rather than waiting for the selector to
          # report one: a second file whose first sheet has the same name as
          # the previous one leaves input$excel_sheet unchanged, so the
          # selector never fired and nothing was loaded.
          load_excel_sheet(file_path, sheets[1])

        } else if (file_ext %in% c("csv", "tsv", "txt")) {
          shinybusy::show_spinner()
          excel_sheets(NULL)  # No sheet selector for delimited text
          data <- tryCatch(
            .read_delimited_upload(file_path),
            finally = shinybusy::hide_spinner()
          )
          user_data(.add_id_data(data))

          shiny::showNotification(
            i18n()$t("File uploaded successfully"),
            type = "message",
            duration = 3
          )
        } else {
          shiny::showNotification(
            i18n()$t("Unsupported file format. Please upload .xlsx, .xls, or .csv file."),
            type = "error",
            duration = 5
          )
        }

      }, error = function(e) {
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

      current <- loaded_sheet()
      if (!is.null(current) &&
          identical(current$path, uploaded_file_path()) &&
          identical(current$sheet, input$excel_sheet)) {
        return(invisible(NULL))
      }

      load_excel_sheet(uploaded_file_path(), input$excel_sheet)
    })

    # Handle text input (paste/type names)
    shiny::observeEvent(input$btn_load_text, {
      req(input$text_input)

      text_content <- input$text_input

      # Check if text is empty or only whitespace
      if (trimws(text_content) == "") {
        shiny::showNotification(
          i18n()$t("Please enter at least one taxonomic name"),
          type = "warning",
          duration = 5
        )
        return(NULL)
      }

      tryCatch({
        shinybusy::show_spinner()

        # Parse text input: split by newline, comma, semicolon, or tab
        # First replace all separators with newline, then split
        text_normalized <- text_content %>%
          gsub(";", "\n", .) %>%
          gsub(",", "\n", .) %>%
          gsub("\t", "\n", .)

        # Split by newline and clean
        names_vector <- strsplit(text_normalized, "\n")[[1]]

        # Clean each name: trim whitespace, remove empty strings
        names_vector <- trimws(names_vector)
        names_vector <- names_vector[names_vector != ""]
        names_vector <- names_vector[!is.na(names_vector)]

        # Remove duplicates while preserving order
        names_vector <- unique(names_vector)

        if (length(names_vector) == 0) {
          shinybusy::hide_spinner()
          shiny::showNotification(
            i18n()$t("No valid names found in the input"),
            type = "warning",
            duration = 5
          )
          return(NULL)
        }

        # Create data frame with taxon_name column
        data <- dplyr::tibble(
          taxon_name = names_vector,
          id_data = seq_along(names_vector)
        )

        user_data(data)
        file_name(paste0(i18n()$t("Text input"), " (", length(names_vector), " ", i18n()$t("names"), ")"))

        shinybusy::hide_spinner()

        shiny::showNotification(
          paste0(length(names_vector), " ", i18n()$t("names loaded successfully")),
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


#' Add a row identifier column when the data has none
#'
#' @param data data.frame.
#' @return `data` with an `id_data` column.
#' @keywords internal
.add_id_data <- function(data) {
  if (!"id_data" %in% colnames(data)) data$id_data <- seq_len(nrow(data))
  data
}

#' Read an uploaded delimited text file (CSV, TSV, TXT)
#'
#' Guesses what a spreadsheet export actually contains rather than assuming
#' `read_csv()` defaults. French-locale Excel writes "CSV" with `;` between
#' fields, `,` as decimal mark and Windows-1252 encoding; read as a plain CSV
#' that came out as a single garbled column.
#'
#' @param path Character path to the file.
#' @return A tibble.
#' @keywords internal
.read_delimited_upload <- function(path) {
  bytes <- readBin(path, "raw", n = min(file.size(path), 1e6))
  # Strip a UTF-8 BOM before deciding, then fall back to Windows-1252 (a
  # superset of latin1) for anything that is not valid UTF-8.
  if (length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    bytes <- bytes[-(1:3)]
  }
  encoding <- if (validUTF8(rawToChar(bytes[bytes != as.raw(0)]))) "UTF-8" else "windows-1252"

  # The delimiter is whichever candidate splits the header line most often.
  header <- strsplit(rawToChar(bytes[bytes != as.raw(0)]), "\r?\n")[[1]][1]
  header <- gsub('"[^"]*"', "", header %||% "")  # ignore delimiters inside quotes
  candidates <- c(",", ";", "\t", "|")
  counts <- vapply(candidates, function(d) lengths(regmatches(header, gregexpr(d, header, fixed = TRUE))), integer(1))
  delim <- if (max(counts) > 0) candidates[which.max(counts)] else ","

  readr::read_delim(
    path,
    delim = delim,
    locale = readr::locale(
      encoding = encoding,
      # With ";" between fields, "," is the decimal mark (French/European Excel)
      decimal_mark = if (delim == ";") "," else ".",
      grouping_mark = if (delim == ";") " " else ","
    ),
    show_col_types = FALSE,
    guess_max = 30000
  )
}
