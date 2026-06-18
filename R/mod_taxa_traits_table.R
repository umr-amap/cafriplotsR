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

    mod_citation_panel_server("cit_panel", citation_data = citation_data, i18n = i18n)

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

        # Build citations x traits pivot table
        if (!is.null(long_df) && "citation_key" %in% names(long_df)) {
          citation_data(build_data_sources_table(long_df))
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
          mod_citation_panel_ui(ns("cit_panel"))
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
