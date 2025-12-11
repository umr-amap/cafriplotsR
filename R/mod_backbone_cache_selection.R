# Backbone Cache Selection Module
#
# Shiny module for allowing users to choose between cached and fresh backbone

#' Backbone Cache Selection Module - UI
#'
#' @description
#' UI component for backbone cache selection modal.
#' The modal is created dynamically in the server, so this returns an empty div.
#'
#' @param id Character, module namespace ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_backbone_cache_selection_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div()  # Modal created in server
}


#' Backbone Cache Selection Module - Server
#'
#' @description
#' Server logic for backbone cache selection. When triggered, checks if cache exists.
#' If cache exists, shows modal dialog with cache metadata and lets user choose
#' between cached or fresh backbone. If no cache, immediately returns "download".
#'
#' @param id Character, module namespace ID
#' @param i18n Reactive returning shiny.i18n translator object
#' @param trigger Reactive that triggers cache check (e.g., button click event)
#'
#' @return Reactive character, user's choice: "cache", "download", or NULL
#'
#' @keywords internal
mod_backbone_cache_selection_server <- function(id, i18n, trigger) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    user_choice <- shiny::reactiveVal(NULL)

    shiny::observeEvent(trigger(), {
      has_cache <- cache_exists()

      if (has_cache) {
        metadata <- get_cache_metadata()

        if (!is.null(metadata)) {
          shiny::showModal(
            shiny::modalDialog(
              title = i18n()$t("Taxonomic Backbone Source"),
              size = "m",
              easyClose = FALSE,
              footer = NULL,

              shiny::div(
                style = "padding: 10px;",

                shiny::p(i18n()$t("A cached version of the taxonomic backbone is available.")),

                # Cache info box
                shiny::div(
                  style = "background-color: #f0f0f0; padding: 15px; border-radius: 5px; margin: 15px 0;",
                  shiny::tags$ul(
                    style = "margin: 0; padding-left: 20px;",
                    shiny::tags$li(
                      shiny::strong(i18n()$t("Downloaded:")),
                      " ", metadata$age_display,
                      " (", format(metadata$download_date, "%Y-%m-%d"), ")"
                    ),
                    shiny::tags$li(
                      shiny::strong(i18n()$t("Size:")),
                      " ", metadata$size_display
                    ),
                    shiny::tags$li(
                      shiny::strong(i18n()$t("Records:")),
                      " ", format(metadata$n_records, big.mark = ",")
                    )
                  )
                ),

                shiny::p(i18n()$t("Would you like to use the cached backbone or download a fresh copy?")),

                # Action buttons
                shiny::div(
                  style = "display: flex; gap: 10px; margin-top: 20px;",

                  shiny::actionButton(
                    inputId = ns("use_cache"),
                    label = i18n()$t("Use Cached Backbone"),
                    icon = shiny::icon("database"),
                    class = "btn-primary",
                    style = "flex: 1;"
                  ),

                  shiny::actionButton(
                    inputId = ns("download_fresh"),
                    label = i18n()$t("Download Fresh Backbone"),
                    icon = shiny::icon("download"),
                    class = "btn-default",
                    style = "flex: 1;"
                  )
                )
              )
            )
          )
        } else {
          # Corrupted cache
          user_choice("download")
          shiny::removeModal()
        }
      } else {
        # No cache
        user_choice("download")
      }
    })

    shiny::observeEvent(input$use_cache, {
      user_choice("cache")
      shiny::removeModal()
    })

    shiny::observeEvent(input$download_fresh, {
      user_choice("download")
      shiny::removeModal()
    })

    return(user_choice)
  })
}
