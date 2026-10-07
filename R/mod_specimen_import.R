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
    # Bumped after each real import so the duplicate check sees the new rows
    db_refresh <- shiny::reactiveVal(0)

    # Flag rows already in the database or repeated in the file.
    # Returns list(data = flagged data, error = message or NULL).
    run_dup_check <- function(data, maps) {
      db_con <- con()
      actual_con <- if (inherits(db_con, "Pool")) pool::poolCheckout(db_con) else db_con
      on.exit({
        if (inherits(db_con, "Pool") && !is.null(actual_con)) pool::poolReturn(actual_con)
      }, add = TRUE)

      tryCatch(
        list(
          data = .check_specimen_duplicates(data, maps$colnbr, maps$suffix, actual_con),
          error = NULL
        ),
        error = function(e) list(data = NULL, error = conditionMessage(e))
      )
    }

    dup_check <- shiny::reactive({
      data <- matched_data()
      maps <- mappings()
      shiny::req(data, maps, con())
      db_refresh()
      run_dup_check(data, maps)
    })

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
      has_ids <- !is.na(data$id_colnam) & !is.na(data$idtax_n)
      flagged <- dup_check()$data
      is_dup <- if (!is.null(flagged)) flagged$is_duplicate else rep(FALSE, n_rows)
      n_valid <- sum(has_ids & !is_dup)
      n_dup <- sum(is_dup)
      n_invalid <- sum(!has_ids & !is_dup)

      shiny::fluidRow(
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 15px; background: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(n_rows, style = "margin: 0; color: #007bff;"),
            shiny::p(i18n()$t("Total Specimens"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 15px; background: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(n_valid, style = "margin: 0; color: #28a745;"),
            shiny::p(i18n()$t("Ready to Import"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = sprintf("padding: 15px; background: #f8f9fa; border-left: 4px solid %s; text-align: center;",
                            if (n_dup == 0) "#28a745" else "#fd7e14"),
            shiny::h3(n_dup, style = sprintf("margin: 0; color: %s;",
                                              if (n_dup == 0) "#28a745" else "#fd7e14")),
            shiny::p(i18n()$t("Duplicates (not imported)"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
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
      flagged <- dup_check()$data
      if (!is.null(flagged)) {
        display_data$status[flagged$dup_in_file] <- "Duplicate in file"
        display_data$status[flagged$in_db] <- "Already in database"
      }
      display_data <- cbind(line = seq_len(nrow(data)) + 1L, display_data)

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
            c("Ready", "Missing IDs", "Duplicate in file", "Already in database"),
            c("#d4edda", "#f8d7da", "#ffe5d0", "#ffe5d0")
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

      issues_ui <- if (length(issues) > 0) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 20px;",
          shiny::h5(shiny::icon("exclamation-triangle"), " ", i18n()$t("Validation Warnings")),
          shiny::tags$ul(
            lapply(issues, function(issue) shiny::tags$li(issue))
          )
        )
      }

      # Duplicate specimens (collector + number + suffix)
      check <- dup_check()
      dup_ui <- if (!is.null(check$error)) {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-top: 20px;",
          shiny::h5(shiny::icon("times-circle"), " ",
                    i18n()$t("Could not check the database for existing specimens")),
          shiny::p(check$error),
          shiny::p(i18n()$t("Import is blocked until this check succeeds."))
        )
      } else if (any(check$data$is_duplicate)) {
        flagged <- check$data
        n_db <- sum(flagged$in_db)
        n_file <- sum(flagged$dup_in_file)
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 20px; border-left: 4px solid #fd7e14;",
          shiny::h5(shiny::icon("clone"), " ", i18n()$t("Duplicate specimens")),
          shiny::p(i18n()$t("A specimen is identified by its collector, number and suffix. The lines below will NOT be imported:")),
          shiny::tags$ul(
            if (n_db > 0) shiny::tags$li(sprintf(
              i18n()$t("%d line(s) are already in the database (see id_specimen)"), n_db
            )),
            if (n_file > 0) shiny::tags$li(sprintf(
              i18n()$t("%d line(s) share the same collector, number and suffix within your file. Keep one line per specimen, then upload the file again."),
              n_file
            ))
          ),
          shiny::p(
            class = "text-muted",
            i18n()$t("Line numbers refer to your spreadsheet, the header being line 1.")
          ),
          DT::dataTableOutput(session$ns("duplicates_table")),
          shiny::div(
            style = "margin-top: 10px;",
            shiny::downloadButton(
              session$ns("download_duplicates"),
              i18n()$t("Download the list of duplicates (CSV)"),
              class = "btn-outline-secondary btn-sm"
            )
          )
        )
      }

      if (is.null(issues_ui) && is.null(dup_ui)) {
        shiny::div(
          class = "alert alert-success",
          style = "margin-top: 20px;",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("All specimens are valid and ready to import.")
        )
      } else {
        shiny::tagList(issues_ui, dup_ui)
      }
    })

    # Duplicate lines, as the user typed them, so they can be found in the file
    duplicates_report <- shiny::reactive({
      flagged <- dup_check()$data
      maps <- mappings()
      shiny::req(flagged, maps)
      flagged <- flagged[flagged$is_duplicate, , drop = FALSE]

      user_cols <- unique(c(maps$collector, maps$colnbr, maps$suffix))
      user_cols <- user_cols[user_cols %in% names(flagged)]

      reason <- ifelse(
        flagged$in_db,
        sprintf(i18n()$t("Already in database (id_specimen %s)"), flagged$existing_id_specimen),
        ""
      )
      in_file <- ifelse(
        flagged$dup_in_file,
        sprintf(i18n()$t("Repeated in file (lines %s)"), flagged$dup_file_lines),
        ""
      )
      reason <- ifelse(reason != "" & in_file != "", paste(reason, in_file, sep = "; "),
                       paste0(reason, in_file))

      out <- data.frame(line = flagged$file_line, stringsAsFactors = FALSE)
      out <- cbind(out, flagged[, user_cols, drop = FALSE])
      out$problem <- reason
      out
    })

    output$duplicates_table <- DT::renderDataTable({
      DT::datatable(
        duplicates_report(),
        options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        class = "cell-border stripe"
      )
    })

    output$download_duplicates <- shiny::downloadHandler(
      filename = function() paste0("specimen_duplicates_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(duplicates_report(), file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    # Import handler
    shiny::observeEvent(input$import_btn, {
      data <- matched_data()
      maps <- mappings()
      shiny::req(data, maps, con())

      dry_run <- input$dry_run

      # Re-check duplicates against the database now, not a cached result
      check <- run_dup_check(data, maps)
      if (!is.null(check$error)) {
        shiny::showNotification(
          paste(i18n()$t("Could not check the database for existing specimens"), check$error),
          type = "error",
          duration = NULL
        )
        return()
      }
      flagged <- check$data

      # Filter to only valid, non-duplicate rows
      keep <- !is.na(data$id_colnam) & !is.na(data$idtax_n) & !flagged$is_duplicate
      valid_data <- data[keep, ]
      n_skipped_dup <- sum(flagged$is_duplicate)

      if (nrow(valid_data) == 0) {
        shiny::showNotification(
          if (n_skipped_dup > 0) {
            i18n()$t("No specimens to import: the remaining lines are duplicates. See the list of duplicates.")
          } else {
            i18n()$t("No valid specimens to import. Please complete the lookup matching step.")
          },
          type = "error",
          duration = 5
        )
        return()
      }

      shiny::withProgress(message = i18n()$t("Importing specimens..."), value = 0, {

        # Prepare specimen records
        shiny::incProgress(0.2, detail = i18n()$t("Preparing data..."))

        # Start with required fields
        specimens <- data.frame(
          id_colnam = valid_data$id_colnam,
          colnbr = valid_data[[maps$colnbr]],
          suffix = if (!is.null(maps$suffix)) valid_data[[maps$suffix]] else NA_character_,
          idtax_n = valid_data$idtax_n,
          stringsAsFactors = FALSE
        )

        # Add determination fields
        # Note: detby is stored as free text (not as ID reference to table_colnam)
        specimens$detby <- if (!is.null(maps$det_by)) as.character(valid_data[[maps$det_by]]) else NA_character_
        specimens$dety <- if (!is.null(maps$det_year)) as.integer(valid_data[[maps$det_year]]) else NA_integer_
        specimens$detm <- if (!is.null(maps$det_month)) as.integer(valid_data[[maps$det_month]]) else NA_integer_
        specimens$detd <- if (!is.null(maps$det_day)) as.integer(valid_data[[maps$det_day]]) else NA_integer_
        specimens$original_tax_name <- if (!is.null(maps$original_tax_name)) as.character(valid_data[[maps$original_tax_name]]) else NA_character_

        # Add collection date fields
        specimens$coly <- if (!is.null(maps$col_year)) as.integer(valid_data[[maps$col_year]]) else NA_integer_
        specimens$colm <- if (!is.null(maps$col_month)) as.integer(valid_data[[maps$col_month]]) else NA_integer_
        specimens$cold <- if (!is.null(maps$col_day)) as.integer(valid_data[[maps$col_day]]) else NA_integer_

        # Add location fields
        specimens$locality <- if (!is.null(maps$locality)) as.character(valid_data[[maps$locality]]) else NA_character_
        specimens$country <- if (!is.null(maps$country)) as.character(valid_data[[maps$country]]) else NA_character_
        specimens$ddlat <- if (!is.null(maps$ddlat)) as.numeric(valid_data[[maps$ddlat]]) else NA_real_
        specimens$ddlon <- if (!is.null(maps$ddlon)) as.numeric(valid_data[[maps$ddlon]]) else NA_real_

        # Add other fields
        specimens$add_col <- if (!is.null(maps$add_col)) as.character(valid_data[[maps$add_col]]) else NA_character_
        specimens$description <- if (!is.null(maps$description)) as.character(valid_data[[maps$description]]) else NA_character_

        # Add modification timestamp (as per add_specimens function)
        specimens$data_modif_y <- as.integer(format(Sys.Date(), "%Y"))
        specimens$data_modif_m <- as.integer(format(Sys.Date(), "%m"))
        specimens$data_modif_d <- as.integer(format(Sys.Date(), "%d"))

        # Note: Leave suffix, detby, and other optional text fields as NA (not empty strings)
        # Only convert empty strings to NA if needed for database consistency
        # The database accepts NULL/NA values for these optional fields

        if (dry_run) {
          # Dry run - just show what would be imported
          shiny::incProgress(1, detail = i18n()$t("Dry run complete"))

          import_results(list(
            success = TRUE,
            dry_run = TRUE,
            n_specimens = nrow(specimens),
            n_skipped_dup = n_skipped_dup,
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

            # Use DBI::dbWriteTable for safe batch insert (prevents SQL injection)
            # This is the same approach used in mod_specimen_add.R
            DBI::dbWriteTable(
              actual_con,
              "specimens",
              specimens,
              append = TRUE,
              row.names = FALSE
            )

            # Commit transaction
            DBI::dbCommit(actual_con)

            shiny::incProgress(1, detail = i18n()$t("Complete!"))

            import_results(list(
              success = TRUE,
              dry_run = FALSE,
              n_specimens = nrow(specimens),
              n_skipped_dup = n_skipped_dup
            ))

            import_complete(TRUE)
            db_refresh(db_refresh() + 1)

            shiny::showNotification(
              sprintf(i18n()$t("Successfully imported %d specimens"), nrow(specimens)),
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
            if (isTRUE(results$n_skipped_dup > 0)) shiny::p(sprintf(
              i18n()$t("%d duplicate specimens would be skipped."), results$n_skipped_dup
            )),
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
          if (isTRUE(results$n_skipped_dup > 0)) shiny::p(sprintf(
            i18n()$t("%d duplicate specimens were skipped."), results$n_skipped_dup
          ))
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
