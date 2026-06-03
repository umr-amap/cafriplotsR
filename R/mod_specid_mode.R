# Specimen Identification App - Step 0: Mode Selector
#
# Lets the user choose between Manual (one specimen) and Batch (file upload).

#' Mode selector UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_mode_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4(i18n$t("Choose update mode")),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::div(
          id = ns("card_manual"), class = "specid-mode-card",
          shiny::actionLink(
            ns("pick_manual"),
            label = shiny::div(
              shiny::icon("edit", style = "font-size: 2.4em; color: #007bff;"),
              shiny::h4(i18n$t("Manual"), style = "margin-top: 10px;"),
              shiny::p(i18n$t("Search a single specimen and update its identification interactively."),
                       style = "color: #6c757d;")
            ),
            style = "color: inherit;"
          )
        )
      ),
      shiny::column(
        6,
        shiny::div(
          id = ns("card_batch"), class = "specid-mode-card",
          shiny::actionLink(
            ns("pick_batch"),
            label = shiny::div(
              shiny::icon("file-upload", style = "font-size: 2.4em; color: #28a745;"),
              shiny::h4(i18n$t("Batch (file upload)"), style = "margin-top: 10px;"),
              shiny::p(i18n$t("Upload an Excel/CSV file with rows of specimens to update."),
                       style = "color: #6c757d;")
            ),
            style = "color: inherit;"
          )
        )
      )
    )
  )
}

#' Mode selector server
#' @param id Module id
#' @param i18n Reactive translator
#' @return Reactive returning "manual" or "batch" (NULL until picked)
#' @keywords internal
#' @export
mod_specid_mode_server <- function(id, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    chosen <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$pick_manual, {
      chosen("manual")
      shinyjs::addClass(id = "card_manual", class = "selected", asis = FALSE)
      shinyjs::removeClass(id = "card_batch",  class = "selected", asis = FALSE)
    })
    shiny::observeEvent(input$pick_batch, {
      chosen("batch")
      shinyjs::addClass(id = "card_batch",  class = "selected", asis = FALSE)
      shinyjs::removeClass(id = "card_manual", class = "selected", asis = FALSE)
    })

    chosen
  })
}
