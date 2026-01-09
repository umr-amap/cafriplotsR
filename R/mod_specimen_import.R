# Specimen Import Wizard - Step 4: Preview & Import
#
# Module for previewing matched specimens and importing to database

#' Specimen Import Module - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
#' @export
mod_specimen_import_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("database"),
      i18n$t("Step 4: Preview & Import"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Review the specimens to be imported. All required fields must be matched before importing."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Import summary
    shiny::uiOutput(ns("import_summary")),

    # Preview table
    shiny::div(
      style = "margin-top: 20px;",
      shiny::h4(i18n$t("Preview"), style = "color: #495057;"),
      DT::dataTableOutput(ns("preview_table"))
    ),

    # Validation messages
    shiny::uiOutput(ns("validation_messages")),

    # Import controls
    shiny::div(
      style = "margin-top: 30px; padding: 20px; background: #f8f9fa; border-radius: 8px;",

      shiny::fluidRow(
        shiny::column(
          6,
          shiny::checkboxInput(
            ns("dry_run"),
            shiny::tagList(
              shiny::icon("flask"),
              " ",
              i18n$t("Dry run (preview only, no database changes)")
            ),
            value = TRUE
          )
        ),
        shiny::column(
          6,
          style = "text-align: right;",
          shiny::actionButton(
            ns("import_btn"),
            shiny::tagList(shiny::icon("upload"), " ", i18n$t("Import Specimens")),
            class = "btn-success btn-lg"
          )
        )
      )
    ),

    # Import results
    shiny::uiOutput(ns("import_results"))
  )
}


#' Specimen Import Module - Server
#'
#' @param id Module namespace ID
#' @param matched_data Reactive containing matched data with IDs
#' @param mappings Reactive containing column mappings
#' @param matching_complete Reactive boolean indicating if matching is complete
#' @param con Reactive database connection pool
#' @param i18n Reactive returning translator object
#' @return Reactive list containing import results
#' @keywords internal
#' @export
mod_specimen_import_server <- function(id, matched_data, mappings, matching_complete, con, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Import results storage
    import_results <- shiny::reactiveVal(NULL)
    import_complete <- shiny::reactiveVal(FALSE)

    # Import summary
    output$import_summary <- shiny::renderUI({
      data <- matched_data()
      complete <- matching_complete()

      if (is.null(data)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ",
            i18n()$t("Complete the previous steps to see the import preview.")
          )
        )
      }

      n_rows <- nrow(data)
      n_valid <- sum(!is.na(data$id_colnam) & !is.na(data$idtax_n))
      n_invalid <- n_rows - n_valid

      shiny::fluidRow(
        shiny::column(
          4,
          shiny::div(
            class = "card",
            style = "padding: 15px; background: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(n_rows, style = "margin: 0; color: #007bff;"),
            shiny::p(i18n()$t("Total Specimens"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          4,
          shiny::div(
            class = "card",
            style = "padding: 15px; background: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(n_valid, style = "margin: 0; color: #28a745;"),
            shiny::p(i18n()$t("Ready to Import"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          4,
          shiny::div(
            class = "card",
            style = sprintf("padding: 15px; background: #f8f9fa; border-left: 4px solid %s; text-align: center;",
                            if (n_invalid == 0) "#28a745" else "#dc3545"),
            shiny::h3(n_invalid, style = sprintf("margin: 0; color: %s;",
                                                  if (n_invalid == 0) "#28a745" else "#dc3545")),
            shiny::p(i18n()$t("Missing Data"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        )
      )
    })

    # Preview table
    output$preview_table <- DT::renderDataTable({
      data <- matched_data()
      maps <- mappings()
      shiny::req(data, maps)

      # Build display data
      display_cols <- c(
        maps$collector, maps$colnbr,
        if (!is.null(maps$suffix)) maps$suffix,
        maps$idtax_n,
        "id_colnam", "idtax_n"
      )
      display_cols <- display_cols[!is.null(display_cols)]
      display_cols <- display_cols[display_cols %in% names(data)]
      # Remove duplicate idtax_n if user's column was already named idtax_n
      display_cols <- unique(display_cols)

      display_data <- data[, display_cols, drop = FALSE]

      # Add status column
      display_data$status <- ifelse(
        !is.na(data$id_colnam) & !is.na(data$idtax_n),
        "Ready",
        "Missing IDs"
      )

      DT::datatable(
        display_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        DT::formatStyle(
          "status",
          backgroundColor = DT::styleEqual(
            c("Ready", "Missing IDs"),
            c("#d4edda", "#f8d7da")
          )
        )
    })

    # Validation messages
    output$validation_messages <- shiny::renderUI({
      data <- matched_data()
      maps <- mappings()
      shiny::req(data, maps)

      issues <- c()

      # Check for missing collector IDs
      missing_coll <- sum(is.na(data$id_colnam))
      if (missing_coll > 0) {
        issues <- c(issues, sprintf(i18n()$t("%d specimens have unmatched collector names"), missing_coll))
      }

      # Check for missing or invalid taxonomic IDs
      missing_taxa <- sum(is.na(data$idtax_n))
      if (missing_taxa > 0) {
        issues <- c(issues, sprintf(i18n()$t("%d specimens have missing or invalid taxonomic IDs"), missing_taxa))
      }

      # Check for duplicate specimens
      if (!is.null(maps$suffix)) {
        dup_key <- paste(data$id_colnam, data[[maps$colnbr]], data[[maps$suffix]], sep = "_")
      } else {
        dup_key <- paste(data$id_colnam, data[[maps$colnbr]], sep = "_")
      }
      n_dups <- sum(duplicated(dup_key))
      if (n_dups > 0) {
        issues <- c(issues, sprintf(i18n()$t("%d potential duplicate specimens detected"), n_dups))
      }

      if (length(issues) > 0) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 20px;",
          shiny::h5(shiny::icon("exclamation-triangle"), " ", i18n()$t("Validation Warnings")),
          shiny::tags$ul(
            lapply(issues, function(issue) shiny::tags$li(issue))
          )
        )
      } else {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 20px;",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("All specimens are valid and ready to import.")
        )
      }
    })

    # Import handler
    shiny::observeEvent(input$import_btn, {
      data <- matched_data()
      maps <- mappings()
      shiny::req(data, maps, con())

      dry_run <- input$dry_run

      # Filter to only valid rows
      valid_data <- data[!is.na(data$id_colnam) & !is.na(data$idtax_n), ]

      if (nrow(valid_data) == 0) {
        shiny::showNotification(
          i18n()$t("No valid specimens to import. Please complete the lookup matching step."),
          type = "error",
          duration = 5
        )
        return()
      }

      shiny::withProgress(message = i18n()$t("Importing specimens..."), value = 0, {

        # Prepare specimen records
        shiny::incProgress(0.2, detail = i18n()$t("Preparing data..."))

        specimens <- data.frame(
          id_table_colnam = valid_data$id_colnam,
          colnbr = valid_data[[maps$colnbr]],
          suffix = if (!is.null(maps$suffix)) valid_data[[maps$suffix]] else NA_character_,
          idtax_n = valid_data$idtax_n,
          detby = if (!is.null(valid_data$id_detby)) valid_data$id_detby else NA_integer_,
          dety = if (!is.null(maps$det_year)) valid_data[[maps$det_year]] else NA_integer_,
          detm = if (!is.null(maps$det_month)) valid_data[[maps$det_month]] else NA_integer_,
          detd = if (!is.null(maps$det_day)) valid_data[[maps$det_day]] else NA_integer_,
          stringsAsFactors = FALSE
        )

        # Clean up NA values for character columns
        specimens$suffix[is.na(specimens$suffix)] <- ""

        if (dry_run) {
          # Dry run - just show what would be imported
          shiny::incProgress(1, detail = i18n()$t("Dry run complete"))

          import_results(list(
            success = TRUE,
            dry_run = TRUE,
            n_specimens = nrow(specimens),
            preview = specimens
          ))

          shiny::showNotification(
            sprintf(i18n()$t("Dry run: %d specimens would be imported"), nrow(specimens)),
            type = "message",
            duration = 5
          )

        } else {
          # Actual import
          shiny::incProgress(0.4, detail = i18n()$t("Connecting to database..."))

          db_con <- con()
          actual_con <- if (inherits(db_con, "Pool")) pool::poolCheckout(db_con) else db_con

          tryCatch({
            # Begin transaction
            DBI::dbBegin(actual_con)

            shiny::incProgress(0.6, detail = i18n()$t("Inserting specimens..."))

            # Insert specimens one by one to get IDs back
            inserted_ids <- c()
            for (i in seq_len(nrow(specimens))) {
              sql <- sprintf(
                "INSERT INTO specimens (id_table_colnam, colnbr, suffix, idtax_n, detby, dety, detm, detd)
                 VALUES (%s, %s, %s, %s, %s, %s, %s, %s)
                 RETURNING id_specimen",
                specimens$id_table_colnam[i],
                specimens$colnbr[i],
                DBI::dbQuoteString(actual_con, specimens$suffix[i]),
                specimens$idtax_n[i],
                ifelse(is.na(specimens$detby[i]), "NULL", specimens$detby[i]),
                ifelse(is.na(specimens$dety[i]), "NULL", specimens$dety[i]),
                ifelse(is.na(specimens$detm[i]), "NULL", specimens$detm[i]),
                ifelse(is.na(specimens$detd[i]), "NULL", specimens$detd[i])
              )

              result <- DBI::dbGetQuery(actual_con, sql)
              inserted_ids <- c(inserted_ids, result$id_specimen)

              # Update progress
              if (i %% 10 == 0) {
                shiny::incProgress(0.3 * (i / nrow(specimens)),
                                   detail = sprintf(i18n()$t("Inserted %d of %d"), i, nrow(specimens)))
              }
            }

            # Commit transaction
            DBI::dbCommit(actual_con)

            shiny::incProgress(1, detail = i18n()$t("Complete!"))

            import_results(list(
              success = TRUE,
              dry_run = FALSE,
              n_specimens = length(inserted_ids),
              inserted_ids = inserted_ids
            ))

            import_complete(TRUE)

            shiny::showNotification(
              sprintf(i18n()$t("Successfully imported %d specimens"), length(inserted_ids)),
              type = "message",
              duration = 5
            )

          }, error = function(e) {
            # Rollback on error
            DBI::dbRollback(actual_con)

            import_results(list(
              success = FALSE,
              dry_run = FALSE,
              error = e$message
            ))

            shiny::showNotification(
              paste(i18n()$t("Import failed:"), e$message),
              type = "error",
              duration = NULL
            )
          }, finally = {
            if (inherits(db_con, "Pool") && !is.null(actual_con)) {
              pool::poolReturn(actual_con)
            }
          })
        }
      })
    })

    # Import results display
    output$import_results <- shiny::renderUI({
      results <- import_results()
      shiny::req(results)

      if (results$dry_run) {
        shiny::div(
          style = "margin-top: 30px;",
          shiny::div(
            class = "alert alert-info",
            shiny::h5(shiny::icon("flask"), " ", i18n()$t("Dry Run Results")),
            shiny::p(sprintf(i18n()$t("%d specimens would be imported to the database."), results$n_specimens)),
            shiny::p(
              class = "text-muted",
              i18n()$t("Uncheck 'Dry run' and click Import again to perform the actual import.")
            )
          ),
          shiny::h5(i18n()$t("Preview of data to import:")),
          DT::dataTableOutput(session$ns("dry_run_preview"))
        )
      } else if (results$success) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 30px;",
          shiny::h4(shiny::icon("check-circle"), " ", i18n()$t("Import Successful!")),
          shiny::p(sprintf(i18n()$t("%d specimens have been added to the database."), results$n_specimens)),
          if (!is.null(results$inserted_ids)) {
            shiny::p(
              i18n()$t("Specimen IDs:"),
              " ",
              paste(head(results$inserted_ids, 10), collapse = ", "),
              if (length(results$inserted_ids) > 10) paste0("... (", length(results$inserted_ids) - 10, " more)")
            )
          }
        )
      } else {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 30px;",
          shiny::h4(shiny::icon("times-circle"), " ", i18n()$t("Import Failed")),
          shiny::p(results$error)
        )
      }
    })

    # Dry run preview table
    output$dry_run_preview <- DT::renderDataTable({
      results <- import_results()
      shiny::req(results, results$dry_run, !is.null(results$preview))

      DT::datatable(
        results$preview,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    # Return results
    return(list(
      results = import_results,
      is_complete = import_complete
    ))
  })
}
