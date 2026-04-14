# Import Wizard - Step 7: Execute Import
#
# Module for executing the final import using import_plot_metadata()

#' Step 7 Module: Execute Import - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step7_import_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("database"),
      i18n$t("Step 7: Execute Import"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      paste0(i18n$t("Ready to import your data into the database. This operation will"), ":"),
      style = "color: #6c757d; font-size: 16px;"
    ),

    shiny::tags$ul(
      style = "color: #6c757d; font-size: 15px; margin-bottom: 30px;",
      shiny::tags$li(i18n$t("Validate data integrity and constraints")),
      shiny::tags$li(i18n$t("Link to existing database references (plots, taxonomy)")),
      shiny::tags$li(i18n$t("Insert data into database tables")),
      shiny::tags$li(i18n$t("Use database transactions (rollback on error)")),
      shiny::tags$li(i18n$t("Automatic access to imported data"))
    ),

    # Import controls
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background-color: #fff3cd; border-left: 4px solid #ffc107;",
          shiny::h5(shiny::icon("exclamation-triangle"), paste0(" ", i18n$t("Dry Run (Preview)")), style = "margin-top: 0; color: #856404;"),
          shiny::p(i18n$t("Preview the import without making changes to the database."), style = "color: #856404; margin-bottom: 15px;"),
          shiny::actionButton(
            ns("btn_dry_run"),
            shiny::tagList(shiny::icon("eye"), paste0(" ", i18n$t("Run Dry Run"))),
            class = "btn-warning btn-lg btn-block"
          )
        )
      ),
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background-color: #d4edda; border-left: 4px solid #28a745;",
          shiny::h5(shiny::icon("database"), paste0(" ", i18n$t("Live Import")), style = "margin-top: 0; color: #155724;"),
          shiny::p(i18n$t("Execute the actual import to the database."), style = "color: #155724; margin-bottom: 15px;"),
          shiny::actionButton(
            ns("btn_import"),
            shiny::tagList(shiny::icon("check"), paste0(" ", i18n$t("Execute Import"))),
            class = "btn-success btn-lg btn-block"
          )
        )
      )
    ),

    shiny::hr(),

    # Import result
    shiny::uiOutput(ns("import_result"))
  )
}


#' Step 7 Module: Execute Import - Server
#'
#' @param id Module namespace ID
#' @param validation_result Reactive containing validation results
#' @param mappings Reactive containing column mappings
#' @param config Reactive containing import configuration
#' @param con Reactive containing database connection pool
#' @param i18n Reactive translator object from shiny.i18n
#' @return Reactive containing import result
#' @keywords internal
mod_step7_import_server <- function(id, validation_result, mappings, config, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for import result
    import_result <- shiny::reactiveVal(NULL)

    # Dry run button
    shiny::observeEvent(input$btn_dry_run, {
      shiny::req(validation_result(), mappings(), config(), con())

      shiny::withProgress({
        shiny::setProgress(0.3, message = "Running dry run...")

        cli::cli_h1("Starting Dry Run Import")

        # Detect import type from cleaned_data structure
        cleaned_data <- validation_result()$cleaned_data
        is_individuals <- is.list(cleaned_data) && !is.data.frame(cleaned_data) &&
                         "individuals" %in% names(cleaned_data)

        result <- tryCatch({
          if (is_individuals) {
            # Import individuals with features
            import_individual_data(
              individuals_data = cleaned_data$individuals,
              features_data = cleaned_data$features,
              validation = validation_result(),
              method = NULL,
              con = con(),
              dry_run = TRUE,
              progress = TRUE,
              ask_confirmation = FALSE
            )
          } else {
            # Import plot metadata
            import_plot_metadata(
              data = cleaned_data,
              column_mappings = mappings(),
              validation = validation_result(),
              config = config(),
              con = con(),
              dry_run = TRUE,
              interactive = FALSE,
              progress = TRUE
            )
          }
        }, error = function(e) {
          cli::cli_alert_danger("Dry run failed: {e$message}")
          list(
            success = FALSE,
            message = paste("Dry run failed:", e$message),
            error = e
          )
        })

        shiny::setProgress(1, message = "Dry run complete!")

        import_result(result)

        if (result$success) {
          shiny::showNotification(
            "Dry run completed successfully! Review the results below.",
            type = "message",
            duration = 5
          )
        } else {
          shiny::showNotification(
            paste("Dry run failed:", result$message),
            type = "error",
            duration = 10
          )
        }

      }, message = "Running dry run...")
    })

    # Import button
    shiny::observeEvent(input$btn_import, {
      shiny::req(validation_result(), mappings(), config(), con())

      # Confirm with user
      cleaned_data <- validation_result()$cleaned_data
      is_individuals <- is.list(cleaned_data) && !is.data.frame(cleaned_data) &&
                       "individuals" %in% names(cleaned_data)
      row_count <- if (is_individuals) nrow(cleaned_data$individuals) else nrow(cleaned_data)

      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tagList(shiny::icon("exclamation-triangle"), " Confirm Import"),
          shiny::p(
            "Are you sure you want to import this data to the database?",
            style = "font-size: 16px;"
          ),
          shiny::p(
            sprintf("This will insert %d rows into the database.", row_count),
            style = "color: #6c757d;"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              session$ns("confirm_import"),
              "Yes, Import Now",
              class = "btn-success"
            )
          )
        )
      )
    })

    # Confirmed import
    shiny::observeEvent(input$confirm_import, {
      shiny::removeModal()

      shiny::withProgress({
        shiny::setProgress(0.3, message = "Importing data...")

        cli::cli_h1("Starting Live Import")

        # Detect import type from cleaned_data structure
        cleaned_data <- validation_result()$cleaned_data
        is_individuals <- is.list(cleaned_data) && !is.data.frame(cleaned_data) &&
                         "individuals" %in% names(cleaned_data)

        result <- tryCatch({
          if (is_individuals) {
            # Import individuals with features
            import_individual_data(
              individuals_data = cleaned_data$individuals,
              features_data = cleaned_data$features,
              validation = validation_result(),
              method = NULL,
              con = con(),
              dry_run = FALSE,
              progress = TRUE,
              ask_confirmation = FALSE
            )
          } else {
            # Import plot metadata
            import_plot_metadata(
              data = cleaned_data,
              column_mappings = mappings(),
              validation = validation_result(),
              config = config(),
              con = con(),
              dry_run = FALSE,
              interactive = FALSE,
              progress = TRUE
            )
          }
        }, error = function(e) {
          cli::cli_alert_danger("Import failed: {e$message}")
          list(
            success = FALSE,
            message = paste("Import failed:", e$message),
            error = e
          )
        })

        shiny::setProgress(1, message = "Import complete!")

        import_result(result)

        if (result$success) {
          shiny::showNotification(
            sprintf("Successfully imported %d plots!", result$n_plots),
            type = "message",
            duration = 10
          )
        } else {
          shiny::showNotification(
            paste("Import failed:", result$message),
            type = "error",
            duration = 10
          )
        }

      }, message = "Importing data...")
    })

    # Render import result
    output$import_result <- shiny::renderUI({
      shiny::req(import_result())

      result <- import_result()

      if (result$success) {
        # Detect import type and get count
        is_individuals <- !is.null(result$n_individuals)
        count <- if (is_individuals) result$n_individuals else result$n_plots
        type_label <- if (is_individuals) "individuals" else "plots"

        # Success message
        shiny::tagList(
          shiny::div(
            class = if (result$dry_run) "alert alert-info" else "alert alert-success",
            shiny::icon(if (result$dry_run) "info-circle" else "check-circle"),
            shiny::strong(
              if (result$dry_run) {
                sprintf(" Dry Run Completed: %d %s would be imported", count, type_label)
              } else {
                sprintf(" Import Successful: %d %s imported!", count, type_label)
              }
            ),
            if (is_individuals && !is.null(result$n_features)) {
              shiny::tagList(
                shiny::br(),
                sprintf("(%d stem attributes)", result$n_features)
              )
            }
          ),

          if (!result$dry_run && !is.null(result$admin_code)) {
            # Get i18n translator
            tr <- shiny::isolate(i18n()$t)

            # Show admin code for row-level security (plots only)
            shiny::tagList(
              shiny::hr(),

              shiny::div(
                class = "alert alert-success",
                shiny::icon("check-circle"),
                shiny::strong(paste0(" ", tr("You Have Access!"))),
                shiny::br(),
                shiny::p(
                  tr("You now have automatic access to the plots you imported."),
                  style = "margin-top: 10px; margin-bottom: 5px;"
                ),
                shiny::p(
                  sprintf("%s %s", paste0(tr("Imported plots:"), ":"), paste(result$plot_names, collapse = ", ")),
                  style = "font-weight: bold; margin-bottom: 0;"
                )
              ),

              shiny::div(
                class = "alert alert-info",
                style = "margin-top: 15px;",
                shiny::icon("info-circle"),
                shiny::strong(paste0(" ", tr("Granting Access to Other Users"))),
                shiny::br(),
                shiny::p(
                  tr("To grant access to OTHER users, send the admin code below to the database administrator."),
                  style = "margin-top: 10px; margin-bottom: 10px;"
                ),
                shiny::p(
                  shiny::icon("user-shield"),
                  " ",
                  shiny::strong(tr("Database Administrator:")),
                  shiny::br(),
                  "Gilles Dauby - gilles.dauby@ird.fr",
                  style = "margin-bottom: 0; font-family: monospace; font-size: 14px;"
                )
              ),

              shiny::h4(
                shiny::icon("code"),
                paste0(" ", tr("Admin Code to Grant Access")),
                style = "margin-top: 30px; margin-bottom: 15px;"
              ),

              shiny::div(
                style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6; margin-bottom: 15px;",
                shiny::tags$pre(
                  style = "margin: 0; white-space: pre-wrap; font-family: 'Courier New', monospace; font-size: 12px;",
                  result$admin_code
                )
              ),

              shiny::fluidRow(
                shiny::column(
                  6,
                  shiny::downloadButton(
                    session$ns("download_admin_code"),
                    "Download Admin Code (.R)",
                    class = "btn-primary btn-block"
                  )
                ),
                shiny::column(
                  6,
                  shiny::actionButton(
                    session$ns("copy_admin_code"),
                    shiny::tagList(shiny::icon("copy"), " Copy to Clipboard"),
                    class = "btn-secondary btn-block",
                    onclick = sprintf(
                      "navigator.clipboard.writeText(`%s`); alert('Admin code copied to clipboard!');",
                      gsub("`", "\\\\`", result$admin_code)
                    )
                  )
                )
              )
            )
          }
        )

      } else {
        # Error message
        shiny::div(
          class = "alert alert-danger",
          shiny::icon("times-circle"),
          shiny::strong(" Import Failed"),
          shiny::br(),
          shiny::p(result$message, style = "margin-top: 10px;")
        )
      }
    })

    # Download admin code
    output$download_admin_code <- shiny::downloadHandler(
      filename = function() {
        paste0("admin_access_request_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        shiny::req(import_result())
        writeLines(import_result()$admin_code, file)

        shiny::showNotification(
          "Admin code downloaded successfully!",
          type = "message",
          duration = 3
        )
      }
    )

    # Return import result
    return(shiny::reactive(import_result()))
  })
}
