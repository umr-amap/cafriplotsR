# Import Wizard - Step 7: Execute Import
#
# Module for executing the final import using import_plot_metadata()

#' Step 7 Module: Execute Import - UI
#'
#' @param id Module namespace ID
#' @keywords internal
mod_step7_import_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("database"),
      "Step 7: Execute Import",
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      "Ready to import your data into the database. This operation will:",
      style = "color: #6c757d; font-size: 16px;"
    ),

    shiny::tags$ul(
      style = "color: #6c757d; font-size: 15px; margin-bottom: 30px;",
      shiny::tags$li("Link methods and countries to database references"),
      shiny::tags$li("Insert plot metadata into data_liste_plots"),
      shiny::tags$li("Insert subplot features (people, census info, etc.)"),
      shiny::tags$li("Use database transactions (rollback on error)"),
      shiny::tags$li("Generate admin code for row-level security access")
    ),

    # Import controls
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background-color: #fff3cd; border-left: 4px solid #ffc107;",
          shiny::h5(shiny::icon("exclamation-triangle"), " Dry Run (Preview)", style = "margin-top: 0; color: #856404;"),
          shiny::p("Preview the import without making changes to the database.", style = "color: #856404; margin-bottom: 15px;"),
          shiny::actionButton(
            ns("btn_dry_run"),
            shiny::tagList(shiny::icon("eye"), " Run Dry Run"),
            class = "btn-warning btn-lg btn-block"
          )
        )
      ),
      shiny::column(
        6,
        shiny::div(
          class = "card",
          style = "padding: 20px; background-color: #d4edda; border-left: 4px solid #28a745;",
          shiny::h5(shiny::icon("database"), " Live Import", style = "margin-top: 0; color: #155724;"),
          shiny::p("Execute the actual import to the database.", style = "color: #155724; margin-bottom: 15px;"),
          shiny::actionButton(
            ns("btn_import"),
            shiny::tagList(shiny::icon("check"), " Execute Import"),
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
#' @return Reactive containing import result
#' @keywords internal
mod_step7_import_server <- function(id, validation_result, mappings, config, con) {
  shiny::moduleServer(id, function(input, output, session) {

    # Storage for import result
    import_result <- shiny::reactiveVal(NULL)

    # Dry run button
    shiny::observeEvent(input$btn_dry_run, {
      shiny::req(validation_result(), mappings(), config(), con())

      shiny::withProgress({
        shiny::setProgress(0.3, message = "Running dry run...")

        cli::cli_h1("Starting Dry Run Import")

        result <- tryCatch({
          import_plot_metadata(
            data = validation_result()$cleaned_data,
            column_mappings = mappings(),
            validation = validation_result(),
            config = config(),
            con = con(),
            dry_run = TRUE,
            interactive = FALSE,
            progress = TRUE
          )
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
      shiny::showModal(
        shiny::modalDialog(
          title = shiny::tagList(shiny::icon("exclamation-triangle"), " Confirm Import"),
          shiny::p(
            "Are you sure you want to import this data to the database?",
            style = "font-size: 16px;"
          ),
          shiny::p(
            sprintf("This will insert %d rows into the database.", nrow(validation_result()$cleaned_data)),
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

        result <- tryCatch({
          import_plot_metadata(
            data = validation_result()$cleaned_data,
            column_mappings = mappings(),
            validation = validation_result(),
            config = config(),
            con = con(),
            dry_run = FALSE,
            interactive = FALSE,
            progress = TRUE
          )
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
        # Success message
        shiny::tagList(
          shiny::div(
            class = if (result$dry_run) "alert alert-info" else "alert alert-success",
            shiny::icon(if (result$dry_run) "info-circle" else "check-circle"),
            shiny::strong(
              if (result$dry_run) {
                sprintf(" Dry Run Completed: %d plots would be imported", result$n_plots)
              } else {
                sprintf(" Import Successful: %d plots imported!", result$n_plots)
              }
            )
          ),

          if (!result$dry_run) {
            # Show admin code for row-level security
            shiny::tagList(
              shiny::hr(),

              shiny::div(
                class = "alert alert-warning",
                shiny::icon("exclamation-triangle"),
                shiny::strong(" Important: Row-Level Security"),
                shiny::br(),
                shiny::p(
                  "You may not have access to these plots yet due to row-level security policies.",
                  style = "margin-top: 10px; margin-bottom: 10px;"
                ),
                shiny::p(
                  sprintf("Imported plots: %s", paste(result$plot_names, collapse = ", ")),
                  style = "font-weight: bold;"
                )
              ),

              shiny::h4(
                shiny::icon("code"),
                " Admin Access Code",
                style = "margin-top: 30px; margin-bottom: 15px;"
              ),

              shiny::p(
                "Send the following R code to your database administrator to grant you access:",
                style = "color: #6c757d;"
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
