# Feature Wizard - Standardize Observations step
#
# Module for parsing the free-text 'observations' trait into standardized
# rows for 'mortality_risk_flag' and 'dawkins_index'. Wraps
# standardize_observations() and shows results for user validation before
# any database write. Mirrors the structure of mod_feat_step3_stem_status.

#' Feature Wizard Step 3: Standardize Observations - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step3_standardize_obs_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("magnifying-glass-chart"),
      i18n$t("Step 3: Standardize Observations"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::div(
      class = "alert alert-warning",
      style = "font-size: 15px; border-left: 5px solid #ffc107;",
      shiny::icon("exclamation-triangle"), " ",
      shiny::strong(i18n$t("Prerequisite: This step parses existing 'observations' (trait id 13) for the stems of the selected plots.")),
      shiny::br(), shiny::br(),
      i18n$t("Run AFTER importing new census observations. Free-text phrases are matched against an editable regex ontology and turned into rows for the 'mortality_risk_flag' and 'dawkins_index' traits. Original 'observations' rows are not modified.")
    ),

    shiny::div(
      class = "alert alert-info",
      style = "margin-bottom: 20px;",
      shiny::icon("info-circle"), " ",
      shiny::strong(i18n$t("What this step does:")),
      shiny::tags$ul(
        style = "margin-top: 8px; margin-bottom: 0;",
        shiny::tags$li(i18n$t("Fetches free-text 'observations' for every stem of the selected plots")),
        shiny::tags$li(i18n$t("Splits multi-observation entries (separators ';' or ', ') into atomic phrases")),
        shiny::tags$li(i18n$t("Classifies phrases via the ontology CSV (mortality risk: 20 tokens; dawkins: 5 classes)")),
        shiny::tags$li(i18n$t("NEVER overwrites existing dawkins_index measurements")),
        shiny::tags$li(i18n$t("Lets you review derived rows and unresolved phrases before any DB write"))
      )
    ),

    shiny::actionButton(
      ns("btn_compute"),
      shiny::tagList(shiny::icon("calculator"), " ", i18n$t("Run Standardization")),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 25px;"
    ),

    shiny::uiOutput(ns("std_summary")),

    shiny::uiOutput(ns("review_header")),
    DT::DTOutput(ns("std_table")),

    shiny::uiOutput(ns("unresolved_header")),
    DT::DTOutput(ns("unresolved_table")),

    shiny::uiOutput(ns("confirm_ui"))
  )
}


#' Feature Wizard Step 3: Standardize Observations - Server
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing selected plots data frame
#' @param con Reactive returning the database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive returning list with \code{data} (standardized tibble)
#'   and \code{config} (mode + individual_ids), or NULL if not yet confirmed
#' @keywords internal
#' @export
mod_feat_step3_standardize_obs_server <- function(id, selected_plots, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    std_result     <- shiny::reactiveVal(NULL)
    unresolved     <- shiny::reactiveVal(NULL)
    individual_ids <- shiny::reactiveVal(NULL)
    confirmed      <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$btn_compute, {
      shiny::req(selected_plots(), con())
      confirmed(FALSE)
      std_result(NULL)
      unresolved(NULL)

      plots <- selected_plots()

      shiny::withProgress({
        shiny::setProgress(0.1, message = i18n()$t("Fetching individual IDs..."))

        plot_ids <- plots$id_liste_plots

        ind_ids <- tryCatch({
          DBI::dbGetQuery(con(), glue::glue_sql(
            "SELECT id_n FROM data_individuals
             WHERE id_table_liste_plots_n IN ({plot_ids*})",
            plot_ids = as.integer(plot_ids),
            .con = con()
          ))$id_n
        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error fetching individuals:"), e$message),
            type = "error", duration = 10
          )
          NULL
        })

        if (is.null(ind_ids) || length(ind_ids) == 0) {
          shiny::showNotification(
            i18n()$t("No individuals found for the selected plots."),
            type = "warning", duration = 8
          )
          return()
        }

        shiny::setProgress(0.3, message = sprintf(
          i18n()$t("Standardizing observations for %d stems..."), length(ind_ids)
        ))

        result <- tryCatch({
          standardize_observations(
            individual_ids = ind_ids,
            add_data       = FALSE,
            con            = con()
          )
        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error standardizing observations:"), e$message),
            type = "error", duration = 10
          )
          NULL
        })

        shiny::setProgress(1, message = i18n()$t("Done!"))

        if (!is.null(result)) {
          individual_ids(as.integer(ind_ids))
          std_result(result)
          unresolved(attr(result, "unresolved"))
          if (nrow(result) > 0) {
            shiny::showNotification(
              sprintf(i18n()$t("Standardized %d row(s)."), nrow(result)),
              type = "message", duration = 5
            )
          } else {
            shiny::showNotification(
              i18n()$t("No phrases matched the ontology."),
              type = "warning", duration = 6
            )
          }
        }
      }, message = i18n()$t("Standardizing observations..."))
    })

    output$std_summary <- shiny::renderUI({
      res <- std_result()
      if (is.null(res)) return(NULL)

      n_total     <- nrow(res)
      n_mort      <- sum(res$trait == "mortality_risk_flag")
      n_daw       <- sum(res$trait == "dawkins_index")
      n_daw_skip  <- sum(res$trait == "dawkins_index" & res$skip_existing)
      n_unres     <- if (is.null(unresolved())) 0 else nrow(unresolved())

      shiny::tagList(
        shiny::h4(i18n()$t("Standardization Summary"),
                  style = "margin-top: 20px; margin-bottom: 15px;"),
        shiny::fluidRow(
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(n_total, style = "margin: 0; color: #007bff;"),
            shiny::p(i18n()$t("Total derived rows"),
                     style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #dc3545; text-align: center;",
            shiny::h3(n_mort, style = "margin: 0; color: #dc3545;"),
            shiny::p(i18n()$t("mortality_risk_flag tokens"),
                     style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(n_daw, style = "margin: 0; color: #28a745;"),
            shiny::p(i18n()$t("dawkins_index values"),
                     style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #fd7e14; text-align: center;",
            shiny::h3(n_daw_skip, style = "margin: 0; color: #fd7e14;"),
            shiny::p(i18n()$t("dawkins skipped (already exist)"),
                     style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #6c757d; text-align: center;",
            shiny::h3(n_unres, style = "margin: 0; color: #6c757d;"),
            shiny::p(i18n()$t("Unresolved phrase patterns"),
                     style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          ))
        )
      )
    })

    output$review_header <- shiny::renderUI({
      if (is.null(std_result()) || nrow(std_result()) == 0) return(NULL)
      shiny::h4(i18n()$t("Review Standardized Rows"),
                style = "margin-top: 25px; margin-bottom: 10px;")
    })

    output$std_table <- DT::renderDT({
      res <- std_result()
      shiny::req(res, nrow(res) > 0)

      display <- res %>%
        dplyr::select(id_n, plot_name, tag, census_name, census_date,
                      trait, std_value, source_phrases, skip_existing)

      DT::datatable(
        display,
        options  = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class    = "display cell-border stripe",
        colnames = c("id_n", "Plot", "Tag", "Census", "Date",
                     "Target trait", "Std value",
                     "Source phrase(s)", "Skip (existing)")
      ) %>%
        DT::formatStyle(
          "trait",
          backgroundColor = DT::styleEqual(
            c("mortality_risk_flag", "dawkins_index"),
            c("#fde2e4",              "#d4edda")
          )
        ) %>%
        DT::formatStyle(
          "skip_existing",
          backgroundColor = DT::styleEqual(TRUE, "#fff3cd")
        )
    })

    output$unresolved_header <- shiny::renderUI({
      un <- unresolved()
      if (is.null(un) || nrow(un) == 0) return(NULL)
      shiny::tagList(
        shiny::h4(i18n()$t("Unresolved Phrases (consider extending the ontology)"),
                  style = "margin-top: 25px; margin-bottom: 8px;"),
        shiny::p(
          i18n()$t("Phrases below matched no ontology pattern. Edit inst/ontology/observations_ontology.csv to capture recurring ones, then re-run the step."),
          style = "color: #6c757d;"
        )
      )
    })

    output$unresolved_table <- DT::renderDT({
      un <- unresolved()
      shiny::req(un)
      if (nrow(un) == 0) return(NULL)
      DT::datatable(
        un,
        options  = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class    = "display cell-border stripe",
        colnames = c("Phrase", "Count")
      )
    })

    output$confirm_ui <- shiny::renderUI({
      if (is.null(std_result()) || nrow(std_result()) == 0) return(NULL)

      if (isTRUE(confirmed())) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 20px;",
          shiny::icon("check-circle"), " ",
          shiny::strong(i18n()$t("Table confirmed. Click Next to proceed to the import step."))
        )
      } else {
        shiny::div(
          style = "margin-top: 20px;",
          shiny::p(
            i18n()$t("Review the table above. If the derived values look correct, confirm to proceed."),
            style = "color: #6c757d;"
          ),
          shiny::actionButton(
            ns("btn_confirm"),
            shiny::tagList(shiny::icon("check"), " ",
                           i18n()$t("Confirm — Derived Rows Look Correct")),
            class = "btn-success btn-lg"
          )
        )
      }
    })

    shiny::observeEvent(input$btn_confirm, {
      shiny::req(std_result())
      confirmed(TRUE)
      shiny::showNotification(
        i18n()$t("Standardized table confirmed. You can now proceed to the import step."),
        type = "message", duration = 5
      )
    })

    return(shiny::reactive({
      shiny::req(confirmed() == TRUE, std_result(), individual_ids())
      list(
        data   = std_result(),
        config = list(
          mode           = "standardize_observations",
          individual_ids = individual_ids()
        )
      )
    }))
  })
}
