#' Citation Panel Module - UI
#'
#' @param id Module namespace ID
#' @return shiny tagList
#' @keywords internal
#' @export
mod_citation_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("panel"))
}

#' Citation Panel Module - Server
#'
#' Renders a panel listing data sources and citations for trait measurements.
#'
#' @param id Module namespace ID
#' @param citation_data Reactive returning a citation summary data.frame (columns:
#'   id_citation, citation_key, citation_authors, citation_year, citation_title,
#'   citation_journal, citation_doi, citation_dataset_name, n_measurements,
#'   n_taxa, n_traits) or NULL when no data is available.
#' @param i18n Reactive returning a shiny.i18n translator object
#' @return NULL
#' @keywords internal
#' @export
mod_citation_panel_server <- function(id, citation_data, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    output$panel <- shiny::renderUI({
      cit <- citation_data()

      if (is.null(cit) || nrow(cit) == 0) {
        return(shiny::div(
          style = "padding: 20px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
          shiny::icon("exclamation-triangle", style = "color: #856404; font-size: 20px;"),
          shiny::p(
            i18n()$t("No citation information available for these trait measurements. Citations may not have been assigned yet."),
            style = "color: #856404; margin: 8px 0 0 0;"
          )
        ))
      }

      ack_banner <- shiny::div(
        style = "padding: 20px; background: #d4edda; border-left: 5px solid #28a745; border-radius: 4px; margin-bottom: 20px;",
        shiny::h4(
          shiny::icon("exclamation-circle", style = "color: #155724;"),
          paste0(" ", i18n()$t("Please cite or acknowledge data sources")),
          style = "color: #155724; margin-top: 0;"
        ),
        shiny::p(
          i18n()$t("The trait data used to enrich your dataset comes from the databases and publications listed below. If you use these data in a publication, you must cite or acknowledge each source accordingly. Proper attribution ensures the sustainability of open data initiatives and recognizes the work of data collectors and curators."),
          style = "color: #155724; margin-bottom: 0;"
        )
      )

      total_measurements <- sum(cit$n_measurements)
      n_sources <- nrow(cit)
      n_uncited <- sum(is.na(cit$citation_key))

      stats_row <- shiny::fluidRow(
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = "border-color: #007bff;",
          shiny::h3(n_sources, style = "color: #007bff; margin: 0;"),
          shiny::tags$small(i18n()$t("Data sources"))
        )),
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = "border-color: #28a745;",
          shiny::h3(total_measurements, style = "color: #28a745; margin: 0;"),
          shiny::tags$small(i18n()$t("Total measurements"))
        )),
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = if (n_uncited > 0) "border-color: #ffc107;" else "border-color: #6c757d;",
          shiny::h3(n_uncited, style = paste0("color: ", if (n_uncited > 0) "#ffc107" else "#6c757d", "; margin: 0;")),
          shiny::tags$small(i18n()$t("Unassigned sources"))
        ))
      )

      citation_cards <- lapply(seq_len(nrow(cit)), function(i) {
        r <- cit[i, ]
        is_unknown <- is.na(r$citation_key)

        if (is_unknown) {
          shiny::div(
            style = "padding: 15px; margin: 10px 0; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
            shiny::fluidRow(
              shiny::column(9,
                shiny::tags$strong(
                  shiny::icon("question-circle", style = "color: #856404;"),
                  paste0(" ", i18n()$t("Unassigned source")),
                  style = "color: #856404;"
                ),
                shiny::p(
                  i18n()$t("These measurements do not have a citation assigned yet."),
                  style = "color: #856404; margin: 4px 0 0 0; font-size: 13px;"
                )
              ),
              shiny::column(3,
                shiny::div(
                  style = "text-align: right; padding-top: 5px;",
                  shiny::tags$span(
                    paste0(r$n_measurements, " ", i18n()$t("measurements")),
                    style = "font-weight: bold; color: #856404;"
                  ),
                  shiny::br(),
                  shiny::tags$small(
                    paste0(r$n_taxa, " ", i18n()$t("taxa"), " | ",
                           r$n_traits, " ", i18n()$t("traits")),
                    style = "color: #856404;"
                  )
                )
              )
            )
          )
        } else {
          authors_str <- if (!is.na(r$citation_authors) && nchar(r$citation_authors) > 0) r$citation_authors else ""
          year_str    <- if (!is.na(r$citation_year)) paste0(" (", r$citation_year, ")") else ""
          title_str   <- if (!is.na(r$citation_title) && nchar(r$citation_title) > 0) r$citation_title else ""
          journal_str <- if (!is.na(r$citation_journal) && nchar(r$citation_journal) > 0) {
            paste0(". ", shiny::tags$em(r$citation_journal))
          } else ""
          doi_str <- if (!is.na(r$citation_doi) && nchar(r$citation_doi) > 0) {
            paste0(" DOI: ", r$citation_doi)
          } else ""

          shiny::div(
            style = "padding: 15px; margin: 10px 0; background: #f8f9fa; border-left: 4px solid #007bff; border-radius: 4px;",
            shiny::fluidRow(
              shiny::column(9,
                shiny::tags$strong(r$citation_key, style = "color: #007bff; font-size: 14px;"),
                if (!is.na(r$citation_dataset_name) && nchar(r$citation_dataset_name) > 0) {
                  shiny::tags$span(
                    paste0(" [", r$citation_dataset_name, "]"),
                    style = "color: #6c757d; font-size: 12px;"
                  )
                },
                shiny::div(
                  style = "margin-top: 6px; color: #495057; font-size: 13px;",
                  shiny::HTML(paste0(
                    authors_str, year_str, ". ",
                    title_str, journal_str, doi_str
                  ))
                )
              ),
              shiny::column(3,
                shiny::div(
                  style = "text-align: right; padding-top: 5px;",
                  shiny::tags$span(
                    paste0(r$n_measurements, " ", i18n()$t("measurements")),
                    style = "font-weight: bold; color: #007bff;"
                  ),
                  shiny::br(),
                  shiny::tags$small(
                    paste0(r$n_taxa, " ", i18n()$t("taxa"), " | ",
                           r$n_traits, " ", i18n()$t("traits")),
                    style = "color: #6c757d;"
                  )
                )
              )
            )
          )
        }
      })

      shiny::tagList(
        ack_banner,
        stats_row,
        shiny::br(),
        shiny::h4(
          shiny::icon("list-alt"),
          paste0(" ", i18n()$t("Sources contributing to your enriched dataset"))
        ),
        do.call(shiny::tagList, citation_cards)
      )
    })

    return(invisible(NULL))
  })
}
