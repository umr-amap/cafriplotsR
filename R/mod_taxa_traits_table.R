#' Taxa Traits Table Module - UI
#'
#' Displays taxa-level traits as formatted wide/long tables with citation info,
#' for a single selected taxon. Used inside mod_taxa_search.
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
mod_taxa_traits_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("table_section"))
}


#' Taxa Traits Table Module - Server
#'
#' Fetches and displays taxa-level traits in wide/long format with citation
#' information for a selected taxon.
#'
#' @param id Module namespace ID
#' @param selected_taxon Reactive returning a single-row data frame with at
#'   least `idtax_n` (from `table_taxa`). Reset to NULL clears results.
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL (invisible)
#' @keywords internal
mod_taxa_traits_table_server <- function(id, selected_taxon, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    traits_wide    <- shiny::reactiveVal(NULL)
    traits_long    <- shiny::reactiveVal(NULL)
    citation_data  <- shiny::reactiveVal(NULL)

    # Reset results whenever the selected taxon changes
    shiny::observeEvent(selected_taxon(), {
      traits_wide(NULL)
      traits_long(NULL)
      citation_data(NULL)
    }, ignoreNULL = FALSE)

    # Section UI: show button only when a taxon is selected
    output$table_section <- shiny::renderUI({
      shiny::req(selected_taxon())

      shiny::tagList(
        shiny::hr(),
        shiny::h5(
          shiny::icon("table"),
          " ",
          i18n()$t("Trait Table")
        ),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em;",
          i18n()$t("Extract full wide/long trait tables with citation information for this taxon.")
        ),
        shiny::actionButton(
          ns("btn_fetch"),
          label = shiny::tagList(
            shiny::icon("download"),
            i18n()$t("Extract as Table")
          ),
          class = "btn-info"
        ),
        shiny::br(),
        shiny::br(),
        shiny::uiOutput(ns("results_tabs"))
      )
    })

    # Fetch traits on button click
    shiny::observeEvent(input$btn_fetch, {
      shiny::req(selected_taxon())

      idtax <- selected_taxon()$idtax_n

      shinybusy::show_spinner()

      tryCatch({

        # --- Wide format ---
        res_wide <- query_taxa_traits(
          idtax        = idtax,
          format       = "wide",
          add_taxa_info = FALSE,
          include_synonyms = TRUE,
          categorical_mode = "mode",
          include_remarks  = FALSE,
          include_measurement_features = FALSE,
          include_citation = TRUE,
          con_taxa     = NULL
        )

        # --- Long format ---
        res_long <- query_taxa_traits(
          idtax        = idtax,
          format       = "long",
          add_taxa_info = FALSE,
          include_synonyms = TRUE,
          include_remarks  = TRUE,
          include_measurement_features = TRUE,
          include_citation = TRUE,
          con_taxa     = NULL
        )

        # Build wide data frame (merge numeric + categorical)
        wide_df <- NULL
        has_numeric <- !is.null(res_wide$traits_numeric) &&
                       !inherits(res_wide$traits_numeric, "logical") &&
                       nrow(res_wide$traits_numeric) > 0

        has_categorical <- !is.null(res_wide$traits_categorical) &&
                           !inherits(res_wide$traits_categorical, "logical") &&
                           nrow(res_wide$traits_categorical) > 0

        if (has_numeric) {
          wide_df <- res_wide$traits_numeric %>%
            dplyr::select(-dplyr::starts_with("id_trait_measures"))
        }
        if (has_categorical) {
          cat_df <- res_wide$traits_categorical %>%
            dplyr::select(-dplyr::starts_with("id_trait_measures"))
          wide_df <- if (is.null(wide_df)) {
            cat_df
          } else {
            dplyr::full_join(wide_df, cat_df, by = "idtax")
          }
        }
        traits_wide(wide_df)

        # Long format raw data
        long_df <- if (!is.null(res_long$traits_raw) &&
                       nrow(res_long$traits_raw) > 0) {
          res_long$traits_raw
        } else {
          NULL
        }
        traits_long(long_df)

        # Build citation summary from long data
        if (!is.null(long_df) && "citation_key" %in% names(long_df)) {
          cit <- long_df %>%
            dplyr::group_by(
              id_citation, citation_key, citation_authors,
              citation_year, citation_title, citation_journal,
              citation_doi, citation_dataset_name
            ) %>%
            dplyr::summarise(
              n_measurements = dplyr::n(),
              n_taxa         = dplyr::n_distinct(idtax),
              n_traits       = dplyr::n_distinct(trait),
              .groups = "drop"
            ) %>%
            dplyr::arrange(dplyr::desc(n_measurements))
          citation_data(cit)
        } else {
          citation_data(NULL)
        }

        if (is.null(wide_df) && is.null(long_df)) {
          shiny::showNotification(
            i18n()$t("No trait data found for this taxon"),
            type = "warning",
            duration = 5
          )
        }

      }, error = function(e) {
        shiny::showNotification(
          paste(i18n()$t("Error fetching trait table:"), e$message),
          type = "error",
          duration = 10
        )
      })

      shinybusy::hide_spinner()
    })

    # Results tabset
    output$results_tabs <- shiny::renderUI({
      shiny::req(!is.null(traits_wide()) || !is.null(traits_long()))

      shiny::tabsetPanel(
        type = "tabs",

        # Wide format tab
        shiny::tabPanel(
          title = i18n()$t("Wide Format (Aggregated)"),
          icon  = shiny::icon("table"),
          shiny::br(),
          shiny::div(
            style = "margin-bottom: 12px;",
            shiny::downloadButton(
              ns("download_wide"),
              label = i18n()$t("Download Wide Format"),
              class = "btn-success"
            )
          ),
          shiny::uiOutput(ns("preview_wide"))
        ),

        # Long format tab
        shiny::tabPanel(
          title = i18n()$t("Long Format (Detailed)"),
          icon  = shiny::icon("list"),
          shiny::br(),
          shiny::div(
            style = "margin-bottom: 12px;",
            shiny::conditionalPanel(
              condition = paste0("output['", ns("has_long"), "']"),
              shiny::downloadButton(
                ns("download_long"),
                label = i18n()$t("Download Long Format"),
                class = "btn-success"
              )
            )
          ),
          shiny::uiOutput(ns("preview_long"))
        ),

        # Data Sources tab
        shiny::tabPanel(
          title = i18n()$t("Data Sources"),
          icon  = shiny::icon("book"),
          shiny::br(),
          shiny::uiOutput(ns("citation_panel"))
        )
      )
    })

    output$has_long <- shiny::reactive({ !is.null(traits_long()) })
    shiny::outputOptions(output, "has_long", suspendWhenHidden = FALSE)

    # ---- Previews ----

    output$preview_wide <- shiny::renderUI({
      if (is.null(traits_wide())) {
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          " ",
          i18n()$t("No aggregated trait data available for this taxon.")
        )
      } else {
        data <- traits_wide()
        shiny::div(
          shiny::h5(paste0(
            i18n()$t("Wide Format"),
            " (", nrow(data), " ", i18n()$t("taxa"), ", ",
            ncol(data), " ", i18n()$t("columns"), ")"
          )),
          DT::renderDataTable({
            DT::datatable(
              data,
              options = list(
                scrollX    = TRUE,
                scrollY    = "350px",
                pageLength = 25
              )
            )
          })
        )
      }
    })

    output$preview_long <- shiny::renderUI({
      if (is.null(traits_long())) {
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("exclamation-triangle"),
          " ",
          i18n()$t("No detailed measurements available for this taxon.")
        )
      } else {
        data <- traits_long()
        shiny::div(
          shiny::h5(paste0(
            i18n()$t("Long Format"),
            " (", nrow(data), " ", i18n()$t("measurements"), ", ",
            ncol(data), " ", i18n()$t("columns"), ")"
          )),
          shiny::p(
            shiny::icon("info-circle"),
            " ",
            i18n()$t("This view shows individual trait measurements with remarks and measurement features included."),
            style = "color: #6c757d; font-style: italic;"
          ),
          DT::renderDataTable({
            DT::datatable(
              data,
              options = list(
                scrollX    = TRUE,
                scrollY    = "350px",
                pageLength = 25
              )
            )
          })
        )
      }
    })

    # ---- Citation panel ----

    output$citation_panel <- shiny::renderUI({
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

      # Acknowledgement banner
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

      # Summary stats
      total_measurements <- sum(cit$n_measurements)
      n_sources  <- nrow(cit)
      n_uncited  <- sum(is.na(cit$citation_key))

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
          shiny::h3(
            n_uncited,
            style = paste0("color: ", if (n_uncited > 0) "#ffc107" else "#6c757d", "; margin: 0;")
          ),
          shiny::tags$small(i18n()$t("Unassigned sources"))
        ))
      )

      # Citation cards
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
          year_str    <- if (!is.na(r$citation_year))    paste0(" (", r$citation_year, ")") else ""
          title_str   <- if (!is.na(r$citation_title)   && nchar(r$citation_title) > 0) r$citation_title else ""
          journal_str <- if (!is.na(r$citation_journal) && nchar(r$citation_journal) > 0) {
            paste0(". ", shiny::tags$em(r$citation_journal))
          } else ""
          doi_str     <- if (!is.na(r$citation_doi) && nchar(r$citation_doi) > 0) {
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

    # ---- Download handlers ----

    output$download_wide <- shiny::downloadHandler(
      filename = function() {
        paste0("taxa_traits_wide_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        shiny::req(traits_wide())
        sheets <- list(traits = traits_wide())
        cit <- citation_data()
        if (!is.null(cit) && nrow(cit) > 0) sheets$citations <- cit
        writexl::write_xlsx(sheets, path = file)
      }
    )

    output$download_long <- shiny::downloadHandler(
      filename = function() {
        paste0("taxa_traits_long_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        shiny::req(traits_long())
        sheets <- list(traits = traits_long())
        cit <- citation_data()
        if (!is.null(cit) && nrow(cit) > 0) sheets$citations <- cit
        writexl::write_xlsx(sheets, path = file)
      }
    )

    return(list(
      traits_fetched = shiny::reactive(
        !is.null(traits_wide()) || !is.null(traits_long())
      )
    ))
  })
}
