# Import Wizard - Step 6: Preview Data
#
# Module for previewing cleaned data before import

#' Step 6 Module: Preview Data - UI
#'
#' @param id Module namespace ID
#' @keywords internal
mod_step6_preview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("eye"),
      "Step 6: Preview Your Data",
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      "Review your cleaned and validated data before importing to the database.",
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Summary cards
    shiny::uiOutput(ns("summary_cards")),

    shiny::hr(),

    # Changes summary (if any)
    shiny::uiOutput(ns("changes_summary")),

    # Data preview
    shiny::h4(
      shiny::icon("table"),
      " Data Preview",
      style = "margin-top: 30px; margin-bottom: 15px;"
    ),
    shiny::p(
      "Preview of your cleaned data (showing first 100 rows):",
      style = "color: #6c757d;"
    ),
    DT::DTOutput(ns("data_preview")),

    # Download options
    shiny::hr(),
    shiny::h4(
      shiny::icon("download"),
      " Download Cleaned Data",
      style = "margin-top: 30px; margin-bottom: 15px;"
    ),
    shiny::p(
      "Download your cleaned data for review or backup:",
      style = "color: #6c757d;"
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::downloadButton(
          ns("download_excel"),
          "Download Excel",
          class = "btn-primary btn-block"
        )
      ),
      shiny::column(
        3,
        shiny::downloadButton(
          ns("download_csv"),
          "Download CSV",
          class = "btn-secondary btn-block"
        )
      )
    )
  )
}


#' Step 6 Module: Preview Data - Server
#'
#' @param id Module namespace ID
#' @param validation_result Reactive containing validation results
#' @return Reactive indicating preview confirmed
#' @keywords internal
mod_step6_preview_server <- function(id, validation_result) {
  shiny::moduleServer(id, function(input, output, session) {

    # Get cleaned data
    cleaned_data <- shiny::reactive({
      shiny::req(validation_result())
      validation_result()$cleaned_data
    })

    # Summary cards
    output$summary_cards <- shiny::renderUI({
      shiny::req(validation_result())

      result <- validation_result()

      shiny::fluidRow(
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #007bff; text-align: center;",
            shiny::h3(result$summary$total_rows, style = "margin: 0; color: #007bff;"),
            shiny::p("Rows to Import", style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(result$summary$mapped_columns, style = "margin: 0; color: #28a745;"),
            shiny::p("Columns Mapped", style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #17a2b8; text-align: center;",
            shiny::h3(result$summary$changes_applied, style = "margin: 0; color: #17a2b8;"),
            shiny::p("Values Auto-Fixed", style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(
              shiny::icon("check-circle", style = "color: #28a745;"),
              style = "margin: 0;"
            ),
            shiny::p("Ready to Import", style = "margin: 5px 0 0 0; color: #28a745; font-weight: bold;")
          )
        )
      )
    })

    # Changes summary
    output$changes_summary <- shiny::renderUI({
      shiny::req(validation_result())

      changes <- validation_result()$changes_made

      if (nrow(changes) == 0) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " No automatic changes were made to your data. All values were valid."
          )
        )
      }

      shiny::tagList(
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("wrench"),
          shiny::strong(sprintf(" %d automatic change(s) were applied:", nrow(changes))),
          shiny::br(),
          shiny::tags$small(
            "Your original data remains unchanged. Only the imported data will reflect these corrections.",
            style = "color: #856404;"
          )
        ),

        shiny::h5("Changes Applied:", style = "margin-top: 20px;"),
        DT::DTOutput(session$ns("changes_table"))
      )
    })

    # Changes table
    output$changes_table <- DT::renderDT({
      shiny::req(validation_result())

      DT::datatable(
        validation_result()$changes_made,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE,
        class = "display cell-border stripe"
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(validation_result()$changes_made),
          backgroundColor = "#fff3cd"
        )
    })

    # Data preview table
    output$data_preview <- DT::renderDT({
      shiny::req(cleaned_data())

      # Show first 100 rows
      preview_data <- head(cleaned_data(), 100)

      # Enrich lookup columns with readable names (replace IDs with names for display)
      preview_data_enriched <- .enrich_preview_with_lookup_names(preview_data)

      DT::datatable(
        preview_data_enriched,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display cell-border stripe hover",
        caption = sprintf(
          "Showing %d of %d total rows",
          nrow(preview_data_enriched),
          nrow(cleaned_data())
        )
      ) %>%
        DT::formatStyle(
          columns = 1:ncol(preview_data_enriched),
          backgroundColor = "#f8f9fa"
        )
    })

    # Download Excel
    output$download_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("cleaned_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        shiny::req(cleaned_data())

        # Enrich data with lookup names (same as preview display)
        enriched_data <- .enrich_preview_with_lookup_names(cleaned_data())

        writexl::write_xlsx(enriched_data, file)

        shiny::showNotification(
          "Excel file downloaded successfully! (with readable lookup values)",
          type = "message",
          duration = 3
        )
      }
    )

    # Download CSV
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("cleaned_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        shiny::req(cleaned_data())

        # Enrich data with lookup names (same as preview display)
        enriched_data <- .enrich_preview_with_lookup_names(cleaned_data())

        write.csv(enriched_data, file, row.names = FALSE)

        shiny::showNotification(
          "CSV file downloaded successfully! (with readable lookup values)",
          type = "message",
          duration = 3
        )
      }
    )

    # Return preview confirmed (always TRUE if user reaches this step)
    return(shiny::reactive(TRUE))
  })
}


#' Enrich Preview Data with Lookup Names
#'
#' Replaces IDs with readable names for lookup columns (method, country, people)
#' for better user experience in preview
#'
#' @param data Data frame with schema column names
#' @return Data frame with IDs replaced by names
#' @keywords internal
.enrich_preview_with_lookup_names <- function(data) {

  enriched_data <- data

  # Method: replace IDs with names
  if ("method" %in% names(enriched_data)) {
    tryCatch({
      # Check if values are numeric (IDs)
      method_values <- enriched_data$method[!is.na(enriched_data$method) & trimws(enriched_data$method) != ""]
      are_numeric <- suppressWarnings(!any(is.na(as.numeric(method_values))))

      if (are_numeric && length(method_values) > 0) {
        # Get method lookup table
        method_lookup <- method_list()

        # Create ID to name mapping
        id_to_name <- setNames(method_lookup$method, method_lookup$id_method)

        # Replace IDs with names
        enriched_data$method <- sapply(enriched_data$method, function(id) {
          if (is.na(id) || trimws(id) == "") return(id)
          name <- id_to_name[[as.character(id)]]
          if (!is.null(name)) name else id
        })

        cli::cli_alert_info("Preview: Enriched method column (IDs → names)")
      }
    }, error = function(e) {
      cli::cli_alert_warning("Could not enrich method column: {e$message}")
    })
  }

  # Country: replace IDs with names
  if ("country" %in% names(enriched_data)) {
    tryCatch({
      # Check if values are numeric (IDs)
      country_values <- enriched_data$country[!is.na(enriched_data$country) & trimws(enriched_data$country) != ""]
      are_numeric <- suppressWarnings(!any(is.na(as.numeric(country_values))))

      if (are_numeric && length(country_values) > 0) {
        # Get country lookup table
        country_lookup <- country_list()

        # Create ID to name mapping
        id_to_name <- setNames(country_lookup$country, country_lookup$id_country)

        # Replace IDs with names
        enriched_data$country <- sapply(enriched_data$country, function(id) {
          if (is.na(id) || trimws(id) == "") return(id)
          name <- id_to_name[[as.character(id)]]
          if (!is.null(name)) name else id
        })

        cli::cli_alert_info("Preview: Enriched country column (IDs → names)")
      }
    }, error = function(e) {
      cli::cli_alert_warning("Could not enrich country column: {e$message}")
    })
  }

  # People columns: replace IDs with names
  tryCatch({
    con <- call.mydb()
    subplot_info <- subplot_list(con)

    if (!is.null(subplot_info) && "type" %in% names(subplot_info) && "valuetype" %in% names(subplot_info)) {
      # Get people column names
      people_cols <- subplot_info$type[!is.na(subplot_info$valuetype) & subplot_info$valuetype == "table_colnam"]
      people_cols <- people_cols[!is.na(people_cols)]
      people_cols <- as.character(people_cols)

      # Get people lookup table
      people_lookup <- DBI::dbGetQuery(con, "
        SELECT id_table_colnam, colnam
        FROM table_colnam
      ")

      # Create ID to name mapping
      id_to_name <- setNames(people_lookup$colnam, people_lookup$id_table_colnam)

      # Process each people column
      for (col in people_cols) {
        if (col %in% names(enriched_data)) {
          # People columns can have comma-separated values
          enriched_data[[col]] <- sapply(enriched_data[[col]], function(cell_value) {
            if (is.na(cell_value) || trimws(cell_value) == "") return(cell_value)

            # Split by comma
            ids_list <- strsplit(as.character(cell_value), ",")[[1]]
            ids_list <- trimws(ids_list)

            # Check if values are numeric (IDs)
            are_numeric <- suppressWarnings(!any(is.na(as.numeric(ids_list))))

            if (are_numeric) {
              # Replace each ID with its name
              names_list <- sapply(ids_list, function(id) {
                name <- id_to_name[[as.character(id)]]
                if (!is.null(name)) name else id
              })

              # Join back with commas
              return(paste(names_list, collapse = ", "))
            } else {
              # Values are already names, keep as is
              return(cell_value)
            }
          })

          cli::cli_alert_info("Preview: Enriched {col} column (IDs → names, comma-separated)")
        }
      }
    }
  }, error = function(e) {
    cli::cli_alert_warning("Could not enrich people columns: {e$message}")
  })

  return(enriched_data)
}
