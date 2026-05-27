# Specimen Identification App - Batch Step 2: Column mapping
#
# Only flat columns are supported (no features). Required mapping:
#   - either `id_specimen`, OR (`collector` + `colnbr`)
#   - one of: `idtax_n`, OR det/colnbr/suffix updates - i.e. at least one
#     change column must be mapped, else update is meaningless.
# Optional:
#   - suffix, detd, detm, dety, detby, detvalue

#' Batch mapping UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_batch_mapping_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("columns"), " ", i18n$t("Step 2: Map columns")),
    shiny::p(i18n$t("Map your file columns to specimen fields. Provide either id_specimen, or both collector + collector number. Map at least one update target (e.g. idtax_n)."),
             style = "color: #6c757d;"),

    shiny::actionButton(ns("autodetect"),
                        shiny::tagList(shiny::icon("magic"), " ", i18n$t("Auto-detect")),
                        class = "btn-info"),
    shiny::br(), shiny::br(),

    shiny::wellPanel(
      shiny::h4(i18n$t("Specimen identification (which row to update)")),
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("col_id_specimen"),
                                            "id_specimen", choices = NULL)),
        shiny::column(4, shiny::selectInput(ns("col_collector"),
                                            i18n$t("collector"), choices = NULL)),
        shiny::column(4, shiny::selectInput(ns("col_colnbr"),
                                            i18n$t("colnbr (collector number)"), choices = NULL))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::selectInput(ns("col_suffix"),
                                            i18n$t("suffix (optional)"), choices = NULL))
      )
    ),

    shiny::wellPanel(
      shiny::h4(i18n$t("New identification")),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("col_idtax_n"),
                                            "idtax_n", choices = NULL))
      )
    ),

    shiny::wellPanel(
      shiny::h4(i18n$t("Determination metadata (optional)")),
      shiny::fluidRow(
        shiny::column(3, shiny::selectInput(ns("col_detd"), "detd",   choices = NULL)),
        shiny::column(3, shiny::selectInput(ns("col_detm"), "detm",   choices = NULL)),
        shiny::column(3, shiny::selectInput(ns("col_dety"), "dety",   choices = NULL))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("col_detby"),    "detby",    choices = NULL)),
        shiny::column(6, shiny::selectInput(ns("col_detvalue"), "detvalue", choices = NULL))
      )
    ),

    shiny::wellPanel(
      shiny::h4(i18n$t("Update collector number / suffix (optional)")),
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("col_new_colnbr"),
                                            i18n$t("new collector number"), choices = NULL)),
        shiny::column(6, shiny::selectInput(ns("col_new_suffix"),
                                            i18n$t("new suffix"), choices = NULL))
      )
    ),

    shiny::uiOutput(ns("validation_msg"))
  )
}

#' Batch mapping server
#' @param id Module id
#' @param data Reactive uploaded data frame
#' @param i18n Reactive translator
#' @return list(mappings = reactive(named list), is_valid = reactive(logical))
#' @keywords internal
#' @export
mod_specid_batch_mapping_server <- function(id, data, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    mappings_rv <- shiny::reactiveVal(NULL)
    valid_rv    <- shiny::reactiveVal(FALSE)

    select_ids <- c(
      "col_id_specimen", "col_collector", "col_colnbr", "col_suffix",
      "col_idtax_n",
      "col_detd", "col_detm", "col_dety", "col_detby", "col_detvalue",
      "col_new_colnbr", "col_new_suffix"
    )

    # Populate dropdowns when data loaded
    shiny::observe({
      shiny::req(data())
      cols <- names(data())
      choices <- c(setNames("", paste0("-- ", i18n()$t("Not mapped"), " --")),
                   setNames(cols, cols))
      for (sid in select_ids) {
        shiny::updateSelectInput(session, sid, choices = choices)
      }
    })

    # Auto-detect by common names
    shiny::observeEvent(input$autodetect, {
      shiny::req(data())
      cols <- tolower(names(data()))
      orig <- names(data())
      pick <- function(...) {
        for (p in c(...)) {
          idx <- which(cols == p)
          if (length(idx) > 0) return(orig[idx[1]])
        }
        ""
      }
      shiny::updateSelectInput(session, "col_id_specimen",
                               selected = pick("id_specimen", "idspecimen"))
      shiny::updateSelectInput(session, "col_collector",
                               selected = pick("collector", "colnam", "collector_name"))
      shiny::updateSelectInput(session, "col_colnbr",
                               selected = pick("colnbr", "number", "collector_number"))
      shiny::updateSelectInput(session, "col_suffix",
                               selected = pick("suffix"))
      shiny::updateSelectInput(session, "col_idtax_n",
                               selected = pick("idtax_n", "idtax", "id_new_taxa"))
      shiny::updateSelectInput(session, "col_detd", selected = pick("detd", "det_day"))
      shiny::updateSelectInput(session, "col_detm", selected = pick("detm", "det_month"))
      shiny::updateSelectInput(session, "col_dety", selected = pick("dety", "det_year"))
      shiny::updateSelectInput(session, "col_detby", selected = pick("detby", "det_by"))
      shiny::updateSelectInput(session, "col_detvalue",
                               selected = pick("detvalue", "det_value"))
      shiny::updateSelectInput(session, "col_new_colnbr", selected = pick("new_colnbr"))
      shiny::updateSelectInput(session, "col_new_suffix", selected = pick("new_suffix"))
    })

    # Validate continuously
    shiny::observe({
      d <- data()
      if (is.null(d)) {
        valid_rv(FALSE); mappings_rv(NULL); return()
      }

      pick <- function(x) if (!is.null(x) && nzchar(x)) x else NULL

      maps <- list(
        id_specimen = pick(input$col_id_specimen),
        collector   = pick(input$col_collector),
        colnbr      = pick(input$col_colnbr),
        suffix      = pick(input$col_suffix),
        idtax_n     = pick(input$col_idtax_n),
        detd        = pick(input$col_detd),
        detm        = pick(input$col_detm),
        dety        = pick(input$col_dety),
        detby       = pick(input$col_detby),
        detvalue    = pick(input$col_detvalue),
        new_colnbr  = pick(input$col_new_colnbr),
        new_suffix  = pick(input$col_new_suffix)
      )
      mappings_rv(maps)

      problems <- character(0)
      has_id_spec <- !is.null(maps$id_specimen)
      has_coll_pair <- !is.null(maps$collector) && !is.null(maps$colnbr)
      if (!has_id_spec && !has_coll_pair) {
        problems <- c(problems,
                      i18n()$t("Map id_specimen, or both collector and colnbr."))
      }
      change_cols <- c(maps$idtax_n, maps$detd, maps$detm, maps$dety,
                       maps$detby, maps$detvalue, maps$new_colnbr, maps$new_suffix)
      if (length(change_cols) == 0) {
        problems <- c(problems,
                      i18n()$t("Map at least one column to update (idtax_n, det*, new_colnbr, new_suffix)."))
      }
      valid_rv(length(problems) == 0)

      output$validation_msg <- shiny::renderUI({
        if (length(problems) == 0) {
          shiny::div(class = "alert alert-success",
                     shiny::icon("check-circle"), " ",
                     i18n()$t("Mapping looks good. You can proceed."))
        } else {
          shiny::div(class = "alert alert-warning",
                     shiny::h5(shiny::icon("exclamation-triangle"), " ",
                               i18n()$t("Mapping issues")),
                     shiny::tags$ul(lapply(problems, shiny::tags$li)))
        }
      })
    })

    list(
      mappings = mappings_rv,
      is_valid = valid_rv
    )
  })
}
