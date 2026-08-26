# Shared helpers for xlsx/csv uploads in the wizard modules.
#
# Every module that offers a "choose xlsx file" input needs the same three
# things: the list of sheets in the workbook, a selector so the user can pick
# one, and a reader that honours that choice. They are gathered here so the
# behaviour stays identical across modules.

#' Is an uploaded file an Excel workbook?
#'
#' @param name File name (with extension).
#' @return Logical scalar.
#' @keywords internal
.is_excel_file <- function(name) {
  tolower(tools::file_ext(name)) %in% c("xls", "xlsx")
}


#' Sheet names of a workbook, or NULL if it cannot be read
#'
#' @param path Path to the workbook.
#' @return Character vector of sheet names, or NULL.
#' @keywords internal
.excel_sheet_names <- function(path) {
  tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
}


#' Read an uploaded table, honouring the chosen sheet
#'
#' Falls back to the first sheet when `sheet` is missing or does not belong to
#' the workbook, so a stale selection left over from a previous upload cannot
#' error out.
#'
#' @param file_info One row of a `shiny::fileInput()` value (`name`,
#'   `datapath`).
#' @param sheet Sheet name to read. Ignored for csv files.
#' @param guess_max Rows readxl uses to guess column types.
#' @return A data.frame.
#' @keywords internal
.read_uploaded_table <- function(file_info, sheet = NULL, guess_max = 5000) {
  ext <- tolower(tools::file_ext(file_info$name))

  if (ext == "csv") {
    return(utils::read.csv(file_info$datapath, stringsAsFactors = FALSE,
                           check.names = FALSE))
  }

  sheets <- .excel_sheet_names(file_info$datapath)
  use <- if (!is.null(sheet) && length(sheet) == 1 && !is.na(sheet) &&
             nzchar(sheet) && sheet %in% sheets) sheet else 1

  as.data.frame(readxl::read_excel(file_info$datapath, sheet = use,
                                   guess_max = guess_max))
}


#' Placeholder for the sheet selector attached to a file input
#'
#' Put this straight after the `shiny::fileInput()` it belongs to, then wire it
#' up with [.xlsx_sheet_server()] using the same `file_id`.
#'
#' @param ns Module namespace function.
#' @param file_id Input id of the file input, without namespace.
#' @return A `shiny::uiOutput()`.
#' @keywords internal
.xlsx_sheet_ui <- function(ns, file_id = "xlsx_file") {
  shiny::uiOutput(ns(paste0(file_id, "_sheet_ui")))
}


#' Wire a sheet selector to a file input and read the selected sheet
#'
#' Renders the selector as soon as an Excel file is uploaded and returns a
#' reactive holding the parsed table. The reactive waits until the selector has
#' caught up with the newly uploaded file, so switching files reads the table
#' once, not twice.
#'
#' @param input,output,session Module server arguments.
#' @param file_id Input id of the file input, without namespace.
#' @param i18n Reactive returning a translator object, or NULL for no
#'   translation.
#' @param guess_max Rows readxl uses to guess column types.
#' @return A reactive returning a data.frame.
#' @keywords internal
.xlsx_sheet_server <- function(input, output, session, file_id = "xlsx_file",
                               i18n = NULL, guess_max = 5000) {
  ns <- session$ns
  sheet_id <- paste0(file_id, "_sheet")
  ui_id <- paste0(file_id, "_sheet_ui")

  tr <- function(txt) if (is.null(i18n)) txt else i18n()$t(txt)

  output[[ui_id]] <- shiny::renderUI({
    f <- input[[file_id]]
    if (is.null(f) || !.is_excel_file(f$name)) return(NULL)

    sheets <- .excel_sheet_names(f$datapath)
    if (is.null(sheets) || length(sheets) == 0) return(NULL)

    shiny::selectInput(
      ns(sheet_id),
      tr("Sheet to read"),
      choices = sheets,
      selected = sheets[1],
      width = "100%"
    )
  })

  # The selector often sits inside a panel that is hidden while another wizard
  # step is showing; without this the input value never arrives and the reader
  # below waits forever.
  shiny::outputOptions(output, ui_id, suspendWhenHidden = FALSE)

  shiny::reactive({
    f <- input[[file_id]]
    shiny::req(f)

    sheet <- NULL
    if (.is_excel_file(f$name)) {
      sheets <- .excel_sheet_names(f$datapath)
      shiny::req(sheets)
      # Wait for the selector to report a sheet of *this* workbook. Reading
      # before that would fire once on the stale sheet and again on the new
      # one.
      shiny::req(input[[sheet_id]] %in% sheets)
      sheet <- input[[sheet_id]]
    }

    # Caught here rather than in the callers: this reactive is used as the
    # event expression of observeEvent(), where a raised error would bypass
    # any tryCatch() in the handler.
    tryCatch(
      .read_uploaded_table(f, sheet = sheet, guess_max = guess_max),
      error = function(e) {
        shiny::showNotification(
          paste(tr("Error reading file:"), conditionMessage(e)),
          type = "error", duration = 10
        )
        shiny::req(FALSE)
      }
    )
  })
}
