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

        # Check import type and call appropriate validation function
        is_individuals <- "idtax_n" %in% config()$import_config$required_columns

        # Run validation (non-interactive for Shiny)
        result <- tryCatch({
          if (is_individuals) {
            # For individuals: need to rename columns and separate direct from features
            cli::cli_alert_info("Validating individuals data...")

            # Step 1: Rename columns according to mappings (user col -> db col)
            # Filter out NA mappings (skipped columns)
            valid_mappings <- mappings()[!is.na(mappings())]

            renamed_data <- data()
            reverse_mappings <- setNames(names(valid_mappings), unlist(valid_mappings))

            for (db_col in names(reverse_mappings)) {
              user_col <- reverse_mappings[[db_col]]
              if (user_col %in% names(renamed_data) && db_col != user_col) {
                names(renamed_data)[names(renamed_data) == user_col] <- db_col
              }
            }

            # Remove any columns that were not mapped (skipped columns)
            skipped_cols <- names(mappings())[is.na(mappings())]
            if (length(skipped_cols) > 0) {
              renamed_data <- renamed_data[, !(names(renamed_data) %in% skipped_cols), drop = FALSE]
              cli::cli_alert_info("Removed {length(skipped_cols)} skipped column(s) from individuals data")
            }

            # Step 2: Add missing required/recommended columns as NA
            all_expected_cols <- c(config()$direct_columns, config()$feature_columns)
            missing_cols <- setdiff(all_expected_cols, names(renamed_data))

            for (col in missing_cols) {
              renamed_data[[col]] <- NA
            }

            # Step 3: Separate direct columns from features
            direct_cols <- config()$direct_columns
            individuals_cols <- intersect(names(renamed_data), direct_cols)
            individuals_data <- renamed_data[, individuals_cols, drop = FALSE]

            # Step 4: Build features_data if there are any feature columns
            feature_cols_all <- config()$feature_columns
            feature_cols <- intersect(names(renamed_data), feature_cols_all)

            features_data <- if (length(feature_cols) > 0) {
              # Features need linking columns (plot_name, tag) plus feature values
              linking_cols <- c("plot_name", "tag")
              linking_present <- intersect(linking_cols, names(renamed_data))
              feature_data_cols <- unique(c(linking_present, feature_cols))
              renamed_data[, feature_data_cols, drop = FALSE]
            } else {
              NULL
            }

            validate_individual_data(
              individuals_data = individuals_data,
              features_data = features_data,
              method = NULL,
              con = con(),
              strict = FALSE,
              interactive = FALSE,
              fix_on_fly = TRUE
            )
          } else {
            # For plots: use plot validation
            cli::cli_alert_info("Validating plot metadata...")
            validate_plot_metadata(
              data = data(),
              column_mappings = mappings(),
              config = config(),
              con = con(),
              strict = FALSE,
              interactive = FALSE,
              fix_on_fly = TRUE
            )
          }
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
