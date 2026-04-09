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
#' between cached, fresh, or WCVP backbone. If no cache, immediately returns "download".
#'
#' @param id Character, module namespace ID
#' @param i18n Reactive returning shiny.i18n translator object
#' @param trigger Reactive that triggers cache check (e.g., button click event)
#' @param wcvp_available Reactive logical, whether WCVP data is available in the database.
#'   If NULL or FALSE, the WCVP option is not shown.
#'
#' @return Reactive character, user's choice: "cache", "download", "wcvp", or NULL
#'
#' @keywords internal
mod_backbone_cache_selection_server <- function(id, i18n, trigger, wcvp_available = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    user_choice <- shiny::reactiveVal(NULL)

    shiny::observeEvent(trigger(), {
      has_cache <- cache_exists()
      has_wcvp <- if (!is.null(wcvp_available) && is.reactive(wcvp_available)) {
        wcvp_available()
      } else if (!is.null(wcvp_available)) {
        wcvp_available
      } else {
        FALSE
      }

      if (has_cache || has_wcvp) {
        metadata <- if (has_cache) get_cache_metadata() else NULL

        shiny::showModal(
          shiny::modalDialog(
            title = i18n()$t("Taxonomic Backbone Source"),
            size = "m",
            easyClose = FALSE,
            footer = NULL,

            shiny::div(
              style = "padding: 10px;",

              # Cache info (only if cache exists)
              if (has_cache && !is.null(metadata)) {
                shiny::tagList(
                  shiny::p(i18n()$t("A cached version of the taxonomic backbone is available.")),
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
                  )
                )
              },

              shiny::p(i18n()$t("Choose which taxonomic backbone to use for matching:")),

              # Action buttons
              shiny::div(
                style = "display: flex; flex-direction: column; gap: 10px; margin-top: 20px;",

                # Row 1: Cache + Download
                shiny::div(
                  style = "display: flex; gap: 10px;",

                  if (has_cache && !is.null(metadata)) {
                    shiny::actionButton(
                      inputId = ns("use_cache"),
                      label = i18n()$t("Use Cached Backbone"),
                      icon = shiny::icon("database"),
                      class = "btn-primary",
                      style = "flex: 1;"
                    )
                  },

                  shiny::actionButton(
                    inputId = ns("download_fresh"),
                    label = i18n()$t("Download Fresh Backbone"),
                    icon = shiny::icon("download"),
                    class = "btn-default",
                    style = "flex: 1;"
                  )
                ),

                # Row 2: WCVP option (only if available)
                if (has_wcvp) {
                  shiny::div(
                    style = "border-top: 1px solid #ddd; padding-top: 10px;",
                    shiny::actionButton(
                      inputId = ns("use_wcvp"),
                      label = i18n()$t("Use WCVP Backbone"),
                      icon = shiny::icon("globe"),
                      class = "btn-success",
                      style = "width: 100%;"
                    ),
                    shiny::helpText(
                      i18n()$t("World Checklist of Vascular Plants - global taxonomic reference")
                    )
                  )
                }
              )
            )
          )
        )
      } else {
        # No cache and no WCVP
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

    shiny::observeEvent(input$use_wcvp, {
      user_choice("wcvp")
      shiny::removeModal()
    })

    return(user_choice)
  })
}
