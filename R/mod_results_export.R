# Results Export Module
#
# Handles exporting standardized results in various formats

#' Results Export Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_results_export_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(shiny::textOutput(ns("title"))),

    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::uiOutput(ns("format_selection"))
      ),
      shiny::column(
        width = 6,
        shiny::uiOutput(ns("column_selection"))
      )
    ),

    shiny::hr(),

    shiny::uiOutput(ns("export_button")),
    shiny::uiOutput(ns("preview"))
  )
}


#' Results Export Module - Server
#'
#' @param id Character, module ID
#' @param results Reactive list from auto matching module
#' @param original_data Reactive data.frame, original user data
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return NULL (handles download only)
#'
#' @keywords internal
mod_results_export_server <- function(id, results, original_data, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Module title
    output$title <- shiny::renderText({
      i18n()$t("Export Results")
    })

    # Format selection
    output$format_selection <- shiny::renderUI({
      ns <- session$ns

      # Build choices with translations
      format_choices <- c("xlsx", "csv", "rds")
      names(format_choices) <- c(
        i18n()$t("Excel (.xlsx) - recommended"),
        i18n()$t("CSV (.csv)"),
        i18n()$t("R data (.rds)")
      )

      shiny::tagList(
        shiny::h4(i18n()$t("Export format:")),
        shiny::radioButtons(
          inputId = ns("export_format"),
          label = NULL,
          choices = format_choices,
          selected = "xlsx"
        )
      )
    })

    # Column selection
    output$column_selection <- shiny::renderUI({
      ns <- session$ns

      # Build choices with translations
      column_choices <- c("original", "ids", "corrected", "metadata")
      names(column_choices) <- c(
        i18n()$t("Original data"),
        i18n()$t("Matched IDs (idtax_n, idtax_good_n)"),
        i18n()$t("Corrected names"),
        i18n()$t("Match metadata (method, score)")
      )

      shiny::tagList(
        shiny::h4(i18n()$t("Include columns:")),
        shiny::checkboxGroupInput(
          inputId = ns("include_columns"),
          label = NULL,
          choices = column_choices,
          selected = c("original", "ids", "corrected", "metadata")
        )
      )
    })

    # Export button
    output$export_button <- shiny::renderUI({
      ns <- session$ns

      if (!is.null(results()) && !is.null(results()$data)) {
        shiny::div(
          shiny::downloadButton(
            outputId = ns("download_data"),
            label = i18n()$t("Download"),
            class = "btn-success"
          ),
          style = "margin-bottom: 20px;"
        )
      } else {
        shiny::div(
          shiny::p(
            shiny::icon("exclamation-triangle"),
            i18n()$t("Complete matching and review before exporting"),
            style = "color: orange;"
          )
        )
      }
    })

    # Download handler
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$export_format %||% "xlsx",
                     xlsx = ".xlsx",
                     csv = ".csv",
                     rds = ".rds")

        paste0(i18n()$t("standardized_taxonomy"), "_", format(Sys.Date(), "%Y%m%d"), ext)
      },

      content = function(file) {
        req(results())

        # Get data
        export_data <- results()$data

        # Filter columns based on selection
        include <- input$include_columns %||% c("original", "ids", "corrected")

        # Start with all columns if "original" selected
        if ("original" %in% include) {
          # Keep all original columns
        } else {
          # Keep only id_data for joining
          orig_cols <- colnames(original_data())
          export_data <- export_data %>%
            dplyr::select(-dplyr::any_of(setdiff(orig_cols, "id_data")))
        }

        # Add matched IDs
        if (!("ids" %in% include)) {
          export_data <- export_data %>%
            dplyr::select(-dplyr::any_of(c("idtax_n", "idtax_good_n")))
        }

        # Add corrected names
        if (!("corrected" %in% include)) {
          export_data <- export_data %>%
            dplyr::select(-dplyr::any_of(c("corrected_name", "matched_name")))
        }

        # Add metadata
        if (!("metadata" %in% include)) {
          export_data <- export_data %>%
            dplyr::select(-dplyr::any_of(c("match_method", "match_score",
                                          "is_synonym", "accepted_name")))
        }

        # Export based on format
        if (input$export_format == "xlsx") {
          writexl::write_xlsx(export_data, path = file)
        } else if (input$export_format == "csv") {
          readr::write_csv(export_data, file = file)
        } else if (input$export_format == "rds") {
          saveRDS(export_data, file = file)
        }
      }
    )

    # Preview
    output$preview <- shiny::renderUI({
      req(results())

      data <- results()$data
      if ("match_score" %in% names(data)) {
        data$match_score <- round(data$match_score, 2)
      }
      n_rows <- nrow(data)

      shiny::div(
        shiny::h4(paste0("Data Preview (", n_rows, " row", if(n_rows != 1) "s" else "", ")")),
        DT::renderDataTable({
          DT::datatable(
            data,  # Show ALL rows, not just head(data, 10)
            options = list(
              scrollX = TRUE,
              pageLength = 25,  # Show 25 rows per page by default
              lengthMenu = c(10, 25, 50, 100, -1),  # Allow user to choose
              lengthChange = TRUE
            )
          )
        })
      )
    })

    return(invisible(NULL))
  })
}
