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
#' @param citation_data Reactive returning a citations x traits (or, for
#'   plots, citations x country) pivot table from
#'   \code{build_data_sources_table()} / \code{build_plot_data_sources_table()}
#'   with columns: \code{citation_key}, optionally \code{citation_authors},
#'   \code{citation_year}, \code{citation_title}, \code{citation_dataset_name},
#'   a count column (\code{n_taxa} or \code{n_plots}, see \code{count_col}),
#'   and one column per trait/country containing counts. Returns NULL when no
#'   data is available.
#' @param i18n Reactive returning a shiny.i18n translator object
#' @param count_col Name of the summary count column to exclude, alongside the
#'   citation metadata columns, when computing the per-column breakdown total
#'   (\code{"n_taxa"} for trait citations, \code{"n_plots"} for plot
#'   citations). Defaults to \code{"n_taxa"}.
#' @param context Either \code{"traits"} (default) or \code{"plots"} - selects
#'   the wording used in the banner, warning message and stat labels, since
#'   the same module backs both the trait-measurement "Data Sources" tab and
#'   the plot-level "Plot Data Sources" tab.
#' @return NULL
#' @keywords internal
#' @export
mod_citation_panel_server <- function(id, citation_data, i18n, count_col = "n_taxa",
                                       context = c("traits", "plots")) {
  context <- match.arg(context)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    meta_cols <- c("citation_key", "citation_authors", "citation_year",
                   "citation_title", "citation_dataset_name")

    trait_columns <- shiny::reactive({
      cit <- citation_data()
      if (is.null(cit) || !is.data.frame(cit)) return(character(0))
      setdiff(names(cit), c(meta_cols, count_col))
    })

    output$dt_sources <- DT::renderDT({
      cit <- citation_data()
      if (is.null(cit) || nrow(cit) == 0) return(NULL)

      DT::datatable(
        cit,
        rownames  = FALSE,
        filter    = "top",
        extensions = "Buttons",
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
        no_cit_msg <- if (context == "plots") {
          i18n()$t("No citation information available for these plots. Citations may not have been assigned yet.")
        } else {
          i18n()$t("No citation information available for these trait measurements. Citations may not have been assigned yet.")
        }
        return(shiny::div(
          style = "padding: 20px; background: #fff3cd; border-left: 4px solid #ffc107; border-radius: 4px;",
          shiny::icon("exclamation-triangle", style = "color: #856404; font-size: 20px;"),
          shiny::p(
            no_cit_msg,
            style = "color: #856404; margin: 8px 0 0 0;"
          )
        ))
      }

      trait_cols  <- trait_columns()
      n_sources   <- nrow(cit)
      n_uncited   <- sum(cit$citation_key == "(no citation)", na.rm = TRUE)

      total_count <- if (length(trait_cols) > 0) {
        sum(as.matrix(cit[, trait_cols, drop = FALSE]), na.rm = TRUE)
      } else {
        0L
      }

      banner_text <- if (context == "plots") {
        i18n()$t("The plot data in your dataset comes from the databases and publications listed below. If you use these data in a publication, you must cite or acknowledge each source accordingly. Proper attribution ensures the sustainability of open data initiatives and recognizes the work of data collectors and curators.")
      } else {
        i18n()$t("The trait data used to enrich your dataset comes from the databases and publications listed below. If you use these data in a publication, you must cite or acknowledge each source accordingly. Proper attribution ensures the sustainability of open data initiatives and recognizes the work of data collectors and curators.")
      }

      ack_banner <- shiny::div(
        style = "padding: 20px; background: #d4edda; border-left: 5px solid #28a745; border-radius: 4px; margin-bottom: 20px;",
        shiny::h4(
          shiny::icon("exclamation-circle", style = "color: #155724;"),
          paste0(" ", i18n()$t("Please cite or acknowledge data sources")),
          style = "color: #155724; margin-top: 0;"
        ),
        shiny::p(
          banner_text,
          style = "color: #155724; margin-bottom: 0;"
        )
      )

      total_label <- if (context == "plots") i18n()$t("Total plots") else i18n()$t("Total measurements")

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
          shiny::h3(total_count, style = "color: #28a745; margin: 0;"),
          shiny::tags$small(total_label)
        )),
        shiny::column(4, shiny::div(
          class = "card text-center p-3",
          style = if (n_uncited > 0) "border-color: #ffc107;" else "border-color: #6c757d;",
          shiny::h3(n_uncited, style = paste0("color: ", if (n_uncited > 0) "#ffc107" else "#6c757d", "; margin: 0;")),
          shiny::tags$small(i18n()$t("Unassigned sources"))
        ))
      )

      section_header <- if (context == "plots") {
        i18n()$t("Sources contributing to your plots")
      } else {
        i18n()$t("Sources contributing to your enriched dataset")
      }

      shiny::tagList(
        ack_banner,
        stats_row,
        shiny::br(),
        shiny::h4(
          shiny::icon("list-alt"),
          paste0(" ", section_header)
        ),
        DT::dataTableOutput(ns("dt_sources"))
      )
    })

    return(invisible(NULL))
  })
}
