# Language Toggle Module
#
# Provides UI and server logic for switching between English and French

#' Language Toggle Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_language_toggle_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::radioButtons(
      inputId = ns("language"),
      label = NULL,
      choices = c("EN" = "en", "FR" = "fr"),
      selected = "en",
      inline = TRUE
    )
  )
}


#' Language Toggle Module - Server
#'
#' @param id Character, module ID
#' @param initial Character, initial language ("en" or "fr")
#'
#' @return Reactive value containing current language
#'
#' @keywords internal
mod_language_toggle_server <- function(id, initial = "en") {
  shiny::moduleServer(id, function(input, output, session) {

    # Initialize with provided language
    if (!is.null(initial) && initial %in% c("en", "fr")) {
      shiny::updateRadioButtons(
        session = session,
        inputId = "language",
        selected = initial
      )
    }

    # Return reactive language value
    return(shiny::reactive(input$language))
  })
}
