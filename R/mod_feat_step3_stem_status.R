# Feature Wizard - Step 3: Compute Stem Status
#
# Module for computing and reviewing stem vital status for selected plots.
# Wraps compute_stem_vital_status() and shows results for user validation
# before any database write.

#' Feature Wizard Step 3: Compute Stem Status - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_feat_step3_stem_status_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("heartbeat"),
      i18n$t("Step 3: Compute Stem Vital Status"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    # Prerequisite warning banner
    shiny::div(
      class = "alert alert-warning",
      style = "font-size: 15px; border-left: 5px solid #ffc107;",
      shiny::icon("exclamation-triangle"), " ",
      shiny::strong(i18n$t("Prerequisite: This step must be run AFTER adding new measurements and recruits for the census.")),
      shiny::br(), shiny::br(),
      i18n$t("Stem vital status is computed from all available evidence (stem diameters, observation notes, RainFor flags). Running this step before new measurements are entered will produce incomplete results.")
    ),

    # Informational panel
    shiny::div(
      class = "alert alert-info",
      style = "margin-bottom: 20px;",
      shiny::icon("info-circle"), " ",
      shiny::strong(i18n$t("What this step does:")),
      shiny::tags$ul(
        style = "margin-top: 8px; margin-bottom: 0;",
        shiny::tags$li(i18n$t("Fetches all evidence traits (diameter, observations, RainFor flags) for every stem of the selected plots")),
        shiny::tags$li(i18n$t("Applies the evidence hierarchy: observations > flag2_rainfor > diameter pattern > flag1_rainfor")),
        shiny::tags$li(i18n$t("Retroactively corrects 'presumed_dead' to 'alive' when a stem is remeasured at a later census")),
        shiny::tags$li(i18n$t("Shows the full status table for your review before any database write"))
      )
    ),

    # Compute button
    shiny::actionButton(
      ns("btn_compute"),
      shiny::tagList(shiny::icon("calculator"), " ", i18n$t("Compute Stem Status")),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 25px;"
    ),

    # Summary cards (shown after compute)
    shiny::uiOutput(ns("status_summary")),

    # Review table
    shiny::uiOutput(ns("review_header")),
    DT::DTOutput(ns("status_table")),

    # Confirm / proceed button (shown after compute)
    shiny::uiOutput(ns("confirm_ui"))
  )
}


#' Feature Wizard Step 3: Compute Stem Status - Server
#'
#' @param id Module namespace ID
#' @param selected_plots Reactive containing selected plots data frame
#' @param con Reactive containing database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive returning list with \code{data} (status tibble) and
#'   \code{config} (list with mode and individual_ids), or NULL if not yet confirmed
#' @keywords internal
#' @export
mod_feat_step3_stem_status_server <- function(id, selected_plots, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    status_result  <- shiny::reactiveVal(NULL)   # tibble from compute_stem_vital_status
    individual_ids <- shiny::reactiveVal(NULL)   # integer vector
    confirmed      <- shiny::reactiveVal(FALSE)

    # ---- Compute button ----
    shiny::observeEvent(input$btn_compute, {
      shiny::req(selected_plots(), con())
      confirmed(FALSE)
      status_result(NULL)

      plots <- selected_plots()

      shiny::withProgress({
        shiny::setProgress(0.1, message = i18n()$t("Fetching individual IDs..."))

        # Retrieve all individual IDs for the selected plots
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
          i18n()$t("Computing status for %d stems..."), length(ind_ids)
        ))

        result <- tryCatch({
          compute_stem_vital_status(
            individual_ids = ind_ids,
            add_data       = FALSE,
            con            = con()
          )
        }, error = function(e) {
          shiny::showNotification(
            paste(i18n()$t("Error computing stem status:"), e$message),
            type = "error", duration = 10
          )
          NULL
        })

        shiny::setProgress(1, message = i18n()$t("Done!"))

        if (!is.null(result) && nrow(result) > 0) {
          individual_ids(as.integer(ind_ids))
          status_result(result)
          shiny::showNotification(
            sprintf(i18n()$t("Status computed for %d stem × census rows."), nrow(result)),
            type = "message", duration = 5
          )
        }

      }, message = i18n()$t("Computing stem vital status..."))
    })

    # ---- Summary cards ----
    output$status_summary <- shiny::renderUI({
      res <- status_result()
      if (is.null(res)) return(NULL)

      n_alive         <- sum(res$stem_vital_status == "alive",         na.rm = TRUE)
      n_dead          <- sum(res$stem_vital_status == "dead",          na.rm = TRUE)
      n_presumed_dead <- sum(res$stem_vital_status == "presumed_dead", na.rm = TRUE)
      n_na            <- sum(is.na(res$stem_vital_status))
      n_corrected     <- sum(stringr::str_detect(
        res$evidence_source, "corrected: remeasured at later census"
      ), na.rm = TRUE)

      shiny::tagList(
        shiny::h4(i18n()$t("Status Summary"), style = "margin-top: 20px; margin-bottom: 15px;"),
        shiny::fluidRow(
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(nrow(res), style = "margin: 0; color: #007bff;"),
            shiny::p(i18n()$t("Total rows"), style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(n_alive, style = "margin: 0; color: #28a745;"),
            shiny::p(i18n()$t("Alive"), style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #dc3545; text-align: center;",
            shiny::h3(n_dead, style = "margin: 0; color: #dc3545;"),
            shiny::p(i18n()$t("Dead"), style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #fd7e14; text-align: center;",
            shiny::h3(n_presumed_dead, style = "margin: 0; color: #fd7e14;"),
            shiny::p(i18n()$t("Presumed dead"), style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = "padding: 15px; background-color: #f8f9fa; border-left: 4px solid #6c757d; text-align: center;",
            shiny::h3(n_na, style = "margin: 0; color: #6c757d;"),
            shiny::p(i18n()$t("Not yet measured"), style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          )),
          shiny::column(2, shiny::div(
            class = "card",
            style = sprintf(
              "padding: 15px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
              if (n_corrected > 0) "#6f42c1" else "#dee2e6"
            ),
            shiny::h3(n_corrected,
              style = sprintf("margin: 0; color: %s;", if (n_corrected > 0) "#6f42c1" else "#adb5bd")),
            shiny::p(i18n()$t("Retroactive corrections"),
              style = "margin: 4px 0 0 0; color: #6c757d; font-size: 13px;")
          ))
        ),

        if (n_corrected > 0) {
          shiny::div(
            class = "alert alert-info",
            style = "margin-top: 15px;",
            shiny::icon("undo"), " ",
            shiny::strong(sprintf(
              i18n()$t("%d retroactive correction(s) applied:"), n_corrected
            )), " ",
            i18n()$t("stems previously classified as 'presumed_dead' were corrected to 'alive' because they were remeasured at a later census.")
          )
        }
      )
    })

    # ---- Review table header ----
    output$review_header <- shiny::renderUI({
      if (is.null(status_result())) return(NULL)
      shiny::h4(i18n()$t("Review Status Table"), style = "margin-top: 25px; margin-bottom: 10px;")
    })

    # ---- Status DT ----
    output$status_table <- DT::renderDT({
      res <- status_result()
      shiny::req(res)

      display <- res %>%
        dplyr::select(
          id_n, plot_name, tag, census_name, census_date,
          stem_vital_status, missing, evidence_source
        ) %>%
        dplyr::mutate(
          retroactive = stringr::str_detect(evidence_source, "corrected: remeasured at later census")
        )

      dt <- DT::datatable(
        display,
        options  = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class    = "display cell-border stripe",
        colnames = c(
          "id_n", "Plot", "Tag", "Census", "Date",
          "Status", "Missing", "Evidence", "Retroactive correction"
        )
      ) %>%
        DT::formatStyle(
          "stem_vital_status",
          backgroundColor = DT::styleEqual(
            c("alive", "dead", "presumed_dead", NA),
            c("#d4edda",  "#f8d7da",  "#fff3cd",     "#f8f9fa")
          )
        ) %>%
        DT::formatStyle(
          "retroactive",
          backgroundColor = DT::styleEqual(TRUE, "#ede0f7")
        )

      dt
    })

    # ---- Confirm UI ----
    output$confirm_ui <- shiny::renderUI({
      if (is.null(status_result())) return(NULL)

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
            i18n()$t("Review the table above. If the status values look correct, confirm to proceed."),
            style = "color: #6c757d;"
          ),
          shiny::actionButton(
            ns("btn_confirm"),
            shiny::tagList(shiny::icon("check"), " ", i18n()$t("Confirm — Table Looks Correct")),
            class = "btn-success btn-lg"
          )
        )
      }
    })

    shiny::observeEvent(input$btn_confirm, {
      shiny::req(status_result())
      confirmed(TRUE)
      shiny::showNotification(
        i18n()$t("Status table confirmed. You can now proceed to the import step."),
        type = "message", duration = 5
      )
    })

    # ---- Return value ----
    # Returns a reactive list matching the pattern used by other step3 modules:
    #   $data   — the status tibble
    #   $config — mode + individual_ids for step 6 import
    return(shiny::reactive({
      shiny::req(confirmed() == TRUE, status_result(), individual_ids())
      list(
        data   = status_result(),
        config = list(
          mode           = "compute_stem_status",
          individual_ids = individual_ids()
        )
      )
    }))
  })
}
