# Specimen Identification App - Batch Step 5: Preview & Apply
#
# Iterates rows of validated_data and calls update_ident_specimens()
# with ask_before_update=FALSE. Optional Dry-run only previews.

#' Batch update UI
#' @param id Module id
#' @param i18n Translator
#' @keywords internal
#' @export
mod_specid_batch_update_ui <- function(id, i18n) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("database"), " ", i18n$t("Step 5: Preview & apply updates")),
    shiny::p(i18n$t("Review the table of updates to apply. Each row will call update_ident_specimens()."),
             style = "color: #6c757d;"),
    DT::DTOutput(ns("preview_tbl")),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::checkboxInput(ns("dry_run"),
                             i18n$t("Dry run (no DB changes)"),
                             value = TRUE)
      ),
      shiny::column(
        6, style = "text-align: right;",
        shiny::actionButton(ns("apply"),
                            shiny::tagList(shiny::icon("upload"), " ", i18n$t("Apply updates")),
                            class = "btn-success btn-lg")
      )
    ),
    shiny::uiOutput(ns("result_ui"))
  )
}

#' Batch update server
#' @param id Module id
#' @param validated_data Reactive validated rows
#' @param mappings Reactive mappings list
#' @param pool_main Reactive main DB pool
#' @param i18n Reactive translator
#' @keywords internal
#' @export
mod_specid_batch_update_server <- function(id, validated_data, mappings,
                                           pool_main, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    result_rv <- shiny::reactiveVal(NULL)

    output$preview_tbl <- DT::renderDT({
      v <- validated_data(); m <- mappings()
      shiny::req(v, m)
      valid <- v[v$.row_valid, , drop = FALSE]
      if (nrow(valid) == 0) {
        return(DT::datatable(data.frame(message = i18n()$t("No valid rows to apply.")),
                             options = list(dom = "t"), rownames = FALSE))
      }
      keep <- c(".resolved_id_specimen",
                m$idtax_n, m$detd, m$detm, m$dety, m$detby, m$detvalue,
                m$new_colnbr, m$new_suffix)
      keep <- keep[!sapply(keep, is.null)]
      keep <- intersect(unlist(keep), names(valid))
      DT::datatable(valid[, c(".resolved_id_specimen", keep[keep != ".resolved_id_specimen"]),
                          drop = FALSE],
                    options = list(pageLength = 15, scrollX = TRUE),
                    rownames = FALSE)
    })

    shiny::observeEvent(input$apply, {
      v <- validated_data(); m <- mappings()
      shiny::req(v, m, pool_main())

      valid <- v[v$.row_valid, , drop = FALSE]
      if (nrow(valid) == 0) {
        shiny::showNotification(i18n()$t("No valid rows to apply."), type = "warning")
        return()
      }

      dry <- isTRUE(input$dry_run)

      opt_val <- function(x) {
        if (is.null(x) || length(x) == 0) return(NULL)
        if (is.na(x)) return(NULL)
        if (is.character(x) && !nzchar(x)) return(NULL)
        x
      }

      get_col <- function(row, key) {
        col <- m[[key]]
        if (is.null(col)) return(NULL)
        opt_val(row[[col]])
      }

      n <- nrow(valid)
      ok    <- logical(n)
      msgs  <- character(n)

      shiny::withProgress(message = i18n()$t("Applying updates..."), value = 0, {
        for (i in seq_len(n)) {
          shiny::incProgress(1 / n, detail = sprintf("%d / %d", i, n))
          row <- valid[i, , drop = FALSE]
          id_sp <- as.integer(row$.resolved_id_specimen)

          new_idtax  <- get_col(row, "idtax_n")
          new_idtax  <- if (!is.null(new_idtax)) suppressWarnings(as.integer(new_idtax)) else NULL

          if (dry) {
            ok[i] <- TRUE
            msgs[i] <- sprintf("DRY: id_specimen=%s -> idtax_n=%s", id_sp,
                               ifelse(is.null(new_idtax), "(unchanged)", new_idtax))
            next
          }

          res <- tryCatch({
            update_ident_specimens(
              id_speci      = id_sp,
              id_new_taxa   = new_idtax,
              new_detd      = get_col(row, "detd"),
              new_detm      = get_col(row, "detm"),
              new_dety      = get_col(row, "dety"),
              new_detby     = get_col(row, "detby"),
              new_detvalue  = get_col(row, "detvalue"),
              new_colnbr    = get_col(row, "new_colnbr"),
              new_suffix    = get_col(row, "new_suffix"),
              add_backup    = TRUE,
              show_results  = FALSE,
              only_new_ident = FALSE,
              ask_before_update = FALSE
            )
            TRUE
          }, error = function(e) e$message)

          if (isTRUE(res)) {
            ok[i] <- TRUE; msgs[i] <- "OK"
          } else {
            ok[i] <- FALSE; msgs[i] <- as.character(res)
          }
        }
      })

      result_rv(data.frame(
        id_specimen = valid$.resolved_id_specimen,
        success     = ok,
        message     = msgs,
        stringsAsFactors = FALSE
      ))

      shiny::showNotification(
        sprintf(i18n()$t("Done. %d succeeded, %d failed."),
                sum(ok), sum(!ok)),
        type = if (all(ok)) "message" else "warning",
        duration = 6
      )
    })

    output$result_ui <- shiny::renderUI({
      r <- result_rv()
      if (is.null(r)) return(NULL)
      shiny::tagList(
        shiny::h5(i18n()$t("Results")),
        DT::DTOutput(session$ns("results_tbl"))
      )
    })

    output$results_tbl <- DT::renderDT({
      r <- result_rv()
      shiny::req(r)
      DT::datatable(r, options = list(pageLength = 20, scrollX = TRUE),
                    rownames = FALSE) %>%
        DT::formatStyle("success",
                        backgroundColor = DT::styleEqual(c(TRUE, FALSE),
                                                         c("#d4edda", "#f8d7da")))
    })

    invisible(NULL)
  })
}
