# Import Wizard - Step 5: Data Validation
#
# Module for validating user data before import

#' Step 5 Module: Data Validation - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step5_validation_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("check-circle"),
      i18n$t("Step 5: Validate Your Data"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Run validation checks on your data to identify any errors or warnings before importing."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Run validation button
    shiny::actionButton(
      ns("run_validation"),
      shiny::tagList(shiny::icon("play"), paste0(" ", i18n$t("Run Validation"))),
      class = "btn-primary btn-lg",
      style = "margin-bottom: 30px;"
    ),

    # Validation results
    shiny::uiOutput(ns("validation_results"))
  )
}


#' Step 5 Module: Data Validation - Server
#'
#' @param id Module namespace ID
#' @param data Reactive containing uploaded user data (should be matched data from Step 4)
#' @param mappings Reactive containing column mappings
#' @param config Reactive containing import configuration
#' @param con Reactive containing database connection pool
#' @return Reactive containing validation results
#' @keywords internal
mod_step5_validation_server <- function(id, data, mappings, config, con) {
  shiny::moduleServer(id, function(input, output, session) {

    # Validation results storage
    validation_result <- shiny::reactiveVal(NULL)

    # Run validation when button clicked
    shiny::observeEvent(input$run_validation, {
      shiny::req(data(), mappings(), config(), con())

      shiny::withProgress({
        shiny::setProgress(0.3, message = "Validating data...")

        cli::cli_alert_info("Running validation on {nrow(data())} rows...")

        # Run validation (non-interactive for Shiny)
        result <- tryCatch({
          validate_plot_metadata(
            data = data(),
            column_mappings = mappings(),
            config = config(),
            con = con(),
            strict = FALSE,
            interactive = FALSE,
            fix_on_fly = TRUE
          )
        }, error = function(e) {
          cli::cli_alert_danger("Validation failed: {e$message}")
          shiny::showNotification(
            paste("Validation error:", e$message),
            type = "error",
            duration = 10
          )
          return(NULL)
        })

        shiny::setProgress(1, message = "Validation complete!")

        if (!is.null(result)) {
          validation_result(result)

          cli::cli_alert_success("Validation complete: {result$summary$errors} errors, {result$summary$warnings} warnings")

          if (result$valid) {
            shiny::showNotification(
              "Validation passed! Your data is ready to import.",
              type = "message",
              duration = 5
            )
          } else {
            shiny::showNotification(
              sprintf("Validation found %d error(s). Please review and fix.", result$summary$errors),
              type = "warning",
              duration = 10
            )
          }
        }

      }, message = "Running validation...")
    })

    # Render validation results
    output$validation_results <- shiny::renderUI({
      shiny::req(validation_result())

      result <- validation_result()

      shiny::tagList(
        # Summary cards
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
              shiny::h3(result$summary$total_rows, style = "margin: 0; color: #007bff;"),
              shiny::p("Total Rows", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = sprintf(
                "padding: 20px; background-color: #f8f9fa; border-left: 4px solid %s; text-align: center;",
                if (result$summary$errors == 0) "#28a745" else "#dc3545"
              ),
              shiny::h3(result$summary$errors, style = sprintf("margin: 0; color: %s;", if (result$summary$errors == 0) "#28a745" else "#dc3545")),
              shiny::p("Errors", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #ffc107; text-align: center;",
              shiny::h3(result$summary$warnings, style = "margin: 0; color: #ffc107;"),
              shiny::p("Warnings", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          ),
          shiny::column(
            3,
            shiny::div(
              class = "card",
              style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #17a2b8; text-align: center;",
              shiny::h3(result$summary$changes_applied, style = "margin: 0; color: #17a2b8;"),
              shiny::p("Auto-Fixed", style = "margin: 5px 0 0 0; color: #6c757d;")
            )
          )
        ),

        shiny::hr(),

        # Overall status
        if (result$valid) {
          shiny::div(
            class = "alert alert-success",
            style = "font-size: 16px;",
            shiny::icon("check-circle", style = "font-size: 24px;"),
            shiny::strong(" Validation Passed! "),
            "Your data meets all requirements and is ready to import.",
            if (result$summary$warnings > 0) {
              shiny::tagList(
                shiny::br(),
                shiny::tags$small(
                  sprintf("Note: %d warning(s) were found but do not prevent import.", result$summary$warnings),
                  style = "color: #856404;"
                )
              )
            }
          )
        } else {
          shiny::div(
            class = "alert alert-danger",
            style = "font-size: 16px;",
            shiny::icon("exclamation-circle", style = "font-size: 24px;"),
            shiny::strong(" Validation Failed "),
            sprintf("Found %d error(s) that must be fixed before import.", result$summary$errors)
          )
        },

        # Errors table
        if (nrow(result$errors) > 0) {
          shiny::tagList(
            shiny::h4(
              shiny::icon("times-circle", style = "color: #dc3545;"),
              " Errors",
              style = "color: #dc3545; margin-top: 30px;"
            ),
            DT::DTOutput(session$ns("errors_table"))
          )
        },

        # Warnings table
        if (nrow(result$warnings) > 0) {
          shiny::tagList(
            shiny::h4(
              shiny::icon("exclamation-triangle", style = "color: #ffc107;"),
              " Warnings",
              style = "color: #ffc107; margin-top: 30px;"
            ),
            DT::DTOutput(session$ns("warnings_table"))
          )
        },

        # Changes made table
        if (nrow(result$changes_made) > 0) {
          shiny::tagList(
            shiny::h4(
              shiny::icon("wrench", style = "color: #17a2b8;"),
              " Auto-Applied Fixes",
              style = "color: #17a2b8; margin-top: 30px;"
            ),
            shiny::p(
              "The following values were automatically corrected during validation:",
              style = "color: #6c757d;"
            ),
            DT::DTOutput(session$ns("changes_table"))
          )
        }
      )
    })

    # Errors table
    output$errors_table <- DT::renderDT({
      shiny::req(validation_result())
      DT::datatable(
        validation_result()$errors,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = "display cell-border stripe"
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(validation_result()$errors),
          backgroundColor = "#fff5f5"
        )
    })

    # Warnings table
    output$warnings_table <- DT::renderDT({
      shiny::req(validation_result())
      DT::datatable(
        validation_result()$warnings,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = "display cell-border stripe"
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(validation_result()$warnings),
          backgroundColor = "#fffbf0"
        )
    })

    # Changes table
    output$changes_table <- DT::renderDT({
      shiny::req(validation_result())
      DT::datatable(
        validation_result()$changes_made,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = "display cell-border stripe"
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(validation_result()$changes_made),
          backgroundColor = "#f0f8ff"
        )
    })

    # Return validation result
    return(shiny::reactive(validation_result()))
  })
}
