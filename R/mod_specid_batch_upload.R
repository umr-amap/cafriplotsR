# Specimen Identification App - Batch Step 1: File upload

#' Batch upload UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_batch_upload_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("upload"), " ", i18n$t("Step 1: Upload file")),
    shiny::p(i18n$t("Upload an Excel (.xlsx) or CSV file. Each row will become one update."),
             style = "color: #6c757d;"),
    shiny::p(i18n$t("Reminder: taxonomic names must already be standardized to idtax_n values. Use the taxonomic match app first if needed."),
             style = "color: #6c757d; font-style: italic;"),
    shiny::fileInput(ns("file"), i18n$t("Choose file"),
                     accept = c(".xlsx", ".xls", ".csv")),
    shiny::uiOutput(ns("preview_section"))
  )
}

#' Batch upload server
#' @param id Module id
#' @param i18n Reactive translator
#' @return Reactive returning the loaded data frame (or NULL)
#' @keywords internal
#' @export
mod_specid_batch_upload_server <- function(id, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    data_rv <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      f <- input$file$datapath
      ext <- tools::file_ext(input$file$name)
      df <- tryCatch({
        if (ext %in% c("xlsx", "xls")) readxl::read_excel(f)
        else if (ext == "csv") utils::read.csv(f, stringsAsFactors = FALSE)
        else stop(sprintf(i18n()$t("Unsupported file format: %s"), ext))
      }, error = function(e) {
        shiny::showNotification(paste(i18n()$t("Error loading file:"), e$message),
                                type = "error", duration = NULL)
        NULL
      })
      if (!is.null(df)) {
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        data_rv(df)
        shiny::showNotification(
          sprintf(i18n()$t("Loaded %d rows, %d columns."), nrow(df), ncol(df)),
          type = "message", duration = 4
        )
      }
    })

    output$preview_section <- shiny::renderUI({
      d <- data_rv()
      if (is.null(d)) return(NULL)
      shiny::tagList(
        shiny::div(class = "alert alert-secondary",
                   shiny::strong(paste0(i18n()$t("Columns detected"), ": ")),
                   shiny::tags$code(paste(names(d), collapse = ", "))),
        DT::DTOutput(session$ns("preview_tbl"))
      )
    })

    output$preview_tbl <- DT::renderDT({
      d <- data_rv()
      shiny::req(d)
      DT::datatable(utils::head(d, 50),
                    options = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
                    rownames = FALSE)
    })

    data_rv
  })
}
