# Progress Tracker Module
#
# Displays matching progress and summary statistics

#' Progress Tracker Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_progress_tracker_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4(shiny::textOutput(ns("title"))),
    shiny::uiOutput(ns("progress_display"))
  )
}


#' Progress Tracker Module - Server
#'
#' @param id Character, module ID
#' @param match_results Reactive list containing matching results
#' @param language Reactive returning current language ("en" or "fr")
#'
#' @return NULL (display only module)
#'
#' @keywords internal
mod_progress_tracker_server <- function(id, match_results, language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    # Get translations
    t <- shiny::reactive({
      get_translations(language())
    })

    # Module title
    output$title <- shiny::renderText({
      t()$progress_title
    })

    # Progress display
    output$progress_display <- shiny::renderUI({
      req(match_results())

      results <- match_results()

      # Extract stats
      total_names <- results$stats$total_names %||% 0
      n_exact <- results$stats$n_exact %||% 0
      n_genus <- results$stats$n_genus %||% 0
      n_fuzzy <- results$stats$n_fuzzy %||% 0
      n_unmatched <- results$stats$n_unmatched %||% 0

      matched <- n_exact + n_genus + n_fuzzy
      percent_complete <- if (total_names > 0) {
        round((matched / total_names) * 100)
      } else {
        0
      }

      shiny::div(
        style = "padding: 10px; background-color: #f8f9fa; border-radius: 5px;",

        # Total names
        shiny::p(
          shiny::strong(t()$progress_total),
          total_names
        ),

        # Breakdown
        shiny::tags$ul(
          shiny::tags$li(
            paste(t()$auto_match_exact, n_exact,
                  paste0("(", round(n_exact/total_names*100, 1), "%)"))
          ),
          shiny::tags$li(
            paste(t()$auto_match_genus, n_genus,
                  paste0("(", round(n_genus/total_names*100, 1), "%)"))
          ),
          shiny::tags$li(
            paste(t()$auto_match_fuzzy, n_fuzzy,
                  paste0("(", round(n_fuzzy/total_names*100, 1), "%)"))
          ),
          shiny::tags$li(
            style = if (n_unmatched > 0) "color: orange; font-weight: bold;" else "",
            paste(t()$auto_match_unmatched, n_unmatched,
                  paste0("(", round(n_unmatched/total_names*100, 1), "%)"))
          )
        ),

        # Progress bar
        shiny::hr(),
        shiny::p(shiny::strong(paste0(t()$progress_percent, " ", percent_complete, "%"))),
        shiny::div(
          class = "progress",
          shiny::div(
            class = paste0("progress-bar ",
                          if (percent_complete == 100) "bg-success" else "bg-info"),
            role = "progressbar",
            style = paste0("width: ", percent_complete, "%"),
            `aria-valuenow` = percent_complete,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100
          )
        )
      )
    })

    return(invisible(NULL))
  })
}
