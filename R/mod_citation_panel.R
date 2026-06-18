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
#' @param citation_data Reactive returning a citations x traits pivot table from
#'   \code{build_data_sources_table()} with columns: \code{citation_key},
#'   optionally \code{citation_authors}, \code{citation_year},
#'   \code{citation_title}, \code{citation_dataset_name}, \code{n_taxa}, and
#'   one column per trait containing measurement counts. Returns NULL when no
#'   data is available.
#' @param i18n Reactive returning a shiny.i18n translator object
#' @return NULL
#' @keywords internal
#' @export
mod_citation_panel_server <- function(id, citation_data, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    meta_cols <- c("citation_key", "citation_authors", "citation_year",
                   "citation_title", "citation_dataset_name")

    trait_columns <- shiny::reactive({
      cit <- citation_data()
      if (is.null(cit) || !is.data.frame(cit)) return(character(0))
      setdiff(names(cit), c(meta_cols, "n_taxa"))
    })

    output$dt_sources <- DT::renderDT({
      cit <- citation_data()
      if (is.null(cit) || nrow(cit) == 0) return(NULL)

      DT::datatable(
        cit,
        rownames  = FALSE,
        filter    = "top",
        extension = "Buttons",
        options   = list(
          scrollX    = TRUE,
          pageLength = 25,
          dom        = "Bfrtip",
          buttons    = list("csv", "excel")
        )
      )
    })

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

      trait_cols  <- trait_columns()
      n_sources   <- nrow(cit)
      n_uncited   <- sum(cit$citation_key == "(no citation)", na.rm = TRUE)

      total_measurements <- if (length(trait_cols) > 0) {
        sum(as.matrix(cit[, trait_cols, drop = FALSE]), na.rm = TRUE)
      } else {
        0L
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

      shiny::tagList(
        ack_banner,
        stats_row,
        shiny::br(),
        shiny::h4(
          shiny::icon("list-alt"),
          paste0(" ", i18n()$t("Sources contributing to your enriched dataset"))
        ),
        DT::dataTableOutput(ns("dt_sources"))
      )
    })

    return(invisible(NULL))
  })
}
