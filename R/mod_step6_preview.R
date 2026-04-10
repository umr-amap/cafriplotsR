# Import Wizard - Step 6: Preview Data
#
# Module for previewing cleaned data before import

#' Step 6 Module: Preview Data - UI
#'
#' @param id Module namespace ID
#' @param i18n Translator object from shiny.i18n
#' @keywords internal
mod_step6_preview_ui <- function(id, i18n) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h3(
      shiny::icon("eye"),
      i18n$t("Step 6: Preview Your Data"),
      style = "color: #495057; margin-bottom: 20px;"
    ),

    shiny::p(
      i18n$t("Review your cleaned and validated data before importing to the database."),
      style = "color: #6c757d; font-size: 16px; margin-bottom: 30px;"
    ),

    # Summary cards
    shiny::uiOutput(ns("summary_cards")),

    shiny::hr(),

    # Changes summary (if any)
    shiny::uiOutput(ns("changes_summary")),

    # Map preview (if coordinates available)
    shiny::uiOutput(ns("map_preview_ui")),

    # Data preview
    shiny::h4(
      shiny::icon("table"),
      paste0(" ", i18n$t("Data Preview")),
      style = "margin-top: 30px; margin-bottom: 15px;"
    ),
    shiny::p(
      i18n$t("Preview of your cleaned data (showing first 100 rows):"),
      style = "color: #6c757d;"
    ),
    DT::DTOutput(ns("data_preview")),

    # Download options
    shiny::hr(),
    shiny::h4(
      shiny::icon("download"),
      paste0(" ", i18n$t("Download Cleaned Data")),
      style = "margin-top: 30px; margin-bottom: 15px;"
    ),
    shiny::p(
      i18n$t("Download your cleaned data for review or backup:"),
      style = "color: #6c757d;"
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::downloadButton(
          ns("download_excel"),
          i18n$t("Download Excel"),
          class = "btn-primary btn-block"
        )
      ),
      shiny::column(
        3,
        shiny::downloadButton(
          ns("download_csv"),
          i18n$t("Download CSV"),
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
#' @param i18n Reactive returning translator object from shiny.i18n
#' @return Reactive indicating preview confirmed
#' @keywords internal
mod_step6_preview_server <- function(id, validation_result, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Get cleaned data (reactiveVal to allow modification)
    cleaned_data <- shiny::reactiveVal(NULL)

    # Initialize cleaned data from validation result
    shiny::observe({
      shiny::req(validation_result())
      if (is.null(cleaned_data())) {
        cleaned_data(validation_result()$cleaned_data)
      }
    })

    # Helper: Extract displayable data frame (handles both plots and individuals import)
    display_data <- shiny::reactive({
      shiny::req(cleaned_data())
      data <- cleaned_data()

      # For individuals import (list with individuals + features)
      if (is.list(data) && !is.data.frame(data) && "individuals" %in% names(data)) {
        return(data$individuals)
      }

      # For plots import (single data frame)
      return(data)
    })

    # UTM to Geographic conversion
    shiny::observeEvent(input$convert_utm, {
      shiny::req(input$utm_zone, input$utm_hemisphere, cleaned_data())

      tryCatch({
        data <- cleaned_data()

        # Check if sf package is available
        if (!requireNamespace("sf", quietly = TRUE)) {
          shiny::showNotification(
            "The 'sf' package is required for UTM conversion. Please install it with: install.packages('sf')",
            type = "error",
            duration = 10
          )
          return(NULL)
        }

        # Filter rows with coordinates
        has_coords <- !is.na(data$ddlat) & !is.na(data$ddlon)

        if (!any(has_coords)) {
          shiny::showNotification(
            "No valid coordinates to convert.",
            type = "warning",
            duration = 5
          )
          return(NULL)
        }

        # Create UTM CRS string
        utm_zone <- input$utm_zone
        hemisphere <- input$utm_hemisphere
        utm_epsg <- if (hemisphere == "N") {
          32600 + utm_zone  # Northern hemisphere
        } else {
          32700 + utm_zone  # Southern hemisphere
        }

        cli::cli_alert_info("Converting UTM Zone {utm_zone}{hemisphere} (EPSG:{utm_epsg}) to WGS84...")

        # Convert coordinates
        coords_to_convert <- data[has_coords, c("ddlon", "ddlat")]  # Note: ddlon=easting, ddlat=northing

        # Create sf object with UTM coordinates
        utm_points <- sf::st_as_sf(
          coords_to_convert,
          coords = c("ddlon", "ddlat"),  # x=easting, y=northing
          crs = utm_epsg
        )

        # Transform to WGS84 (EPSG:4326)
        wgs84_points <- sf::st_transform(utm_points, crs = 4326)

        # Extract converted coordinates
        coords_matrix <- sf::st_coordinates(wgs84_points)

        # Update data with converted coordinates
        data$ddlon[has_coords] <- coords_matrix[, "X"]  # Longitude
        data$ddlat[has_coords] <- coords_matrix[, "Y"]  # Latitude

        # Update cleaned data
        cleaned_data(data)

        # Also update validation result to persist conversion
        val_result <- validation_result()
        val_result$cleaned_data <- data
        # Note: validation_result is passed from parent, we can't modify it directly
        # The conversion will be applied to the downloaded data and import

        shiny::showNotification(
          sprintf("Successfully converted %d coordinates from UTM Zone %d%s to WGS84 (geographic coordinates).",
                  sum(has_coords), utm_zone, hemisphere),
          type = "message",
          duration = 5
        )

        cli::cli_alert_success("Conversion complete: {sum(has_coords)} points converted")

      }, error = function(e) {
        shiny::showNotification(
          paste("Error converting coordinates:", e$message),
          type = "error",
          duration = 10
        )
        cli::cli_alert_danger("Conversion failed: {e$message}")
      })
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
            shiny::p(i18n()$t("Rows to Import"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #28a745; text-align: center;",
            shiny::h3(result$summary$mapped_columns, style = "margin: 0; color: #28a745;"),
            shiny::p(i18n()$t("Columns Mapped"), style = "margin: 5px 0 0 0; color: #6c757d;")
          )
        ),
        shiny::column(
          3,
          shiny::div(
            class = "card",
            style = "padding: 20px; background-color: #f8f9fa; border-left: 4px solid #17a2b8; text-align: center;",
            shiny::h3(result$summary$changes_applied, style = "margin: 0; color: #17a2b8;"),
            shiny::p(i18n()$t("Values Auto-Fixed"), style = "margin: 5px 0 0 0; color: #6c757d;")
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
            shiny::p(i18n()$t("Ready to Import"), style = "margin: 5px 0 0 0; color: #28a745; font-weight: bold;")
          )
        )
      )
    })

    # Map preview UI
    output$map_preview_ui <- shiny::renderUI({
      shiny::req(display_data())

      data <- display_data()

      # Check if coordinates are available
      has_coords <- all(c("ddlat", "ddlon") %in% names(data))

      if (!has_coords) {
        return(NULL)  # No map if no coordinates
      }

      # Filter valid coordinates
      coords_data <- data[!is.na(data$ddlat) & !is.na(data$ddlon), ]

      if (nrow(coords_data) == 0) {
        return(NULL)  # No map if all coordinates are NA
      }

      # Check for potential lat/lon reversal
      # Coerce to numeric to handle non-numeric coordinate data
      lat_numeric <- suppressWarnings(as.numeric(coords_data$ddlat))
      lon_numeric <- suppressWarnings(as.numeric(coords_data$ddlon))

      lat_range <- range(lat_numeric, na.rm = TRUE)
      lon_range <- range(lon_numeric, na.rm = TRUE)

      # Detect if coordinates might be UTM (very large numbers)
      # Only check if we have valid numeric values
      looks_like_utm <- if (all(is.finite(lat_range)) && all(is.finite(lon_range))) {
        (abs(lat_range[1]) > 180 || abs(lat_range[2]) > 180 ||
         abs(lon_range[1]) > 180 || abs(lon_range[2]) > 180) &&
        (max(abs(lat_range), abs(lon_range)) > 1000)
      } else {
        FALSE
      }

      # Warning if coordinates seem suspicious
      warning_ui <- if (looks_like_utm) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-bottom: 10px;",
          shiny::icon("exclamation-circle"),
          shiny::strong(" UTM Coordinates Detected?"),
          shiny::br(),
          sprintf("Coordinates appear to be in UTM format (range: lat %.0f to %.0f, lon %.0f to %.0f). ",
                  lat_range[1], lat_range[2], lon_range[1], lon_range[2]),
          shiny::br(),
          "If these are UTM coordinates, use the converter below to transform them to geographic coordinates (WGS84).",
          shiny::br(),
          shiny::br(),
          shiny::strong("UTM to Geographic Converter:"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::numericInput(
                session$ns("utm_zone"),
                "UTM Zone (1-60):",
                value = NULL,
                min = 1,
                max = 60,
                step = 1
              )
            ),
            shiny::column(
              4,
              shiny::selectInput(
                session$ns("utm_hemisphere"),
                "Hemisphere:",
                choices = c("North" = "N", "South" = "S"),
                selected = "N"
              )
            ),
            shiny::column(
              4,
              shiny::br(),
              shiny::actionButton(
                session$ns("convert_utm"),
                "Convert to Geographic",
                class = "btn-primary",
                style = "margin-top: 5px;"
              )
            )
          ),
          shiny::tags$small(
            shiny::icon("info-circle"),
            " Note: Conversion requires the 'sf' package. The conversion will update your data before import.",
            style = "color: #856404;"
          )
        )
      } else if (any(suppressWarnings(abs(as.numeric(coords_data$ddlat)) > 90), na.rm = TRUE) ||
                 any(suppressWarnings(abs(as.numeric(coords_data$ddlon)) > 180), na.rm = TRUE)) {
        shiny::div(
          class = "alert alert-danger",
          style = "margin-bottom: 10px;",
          shiny::icon("exclamation-triangle"),
          shiny::strong(" Invalid Coordinates Detected!"),
          shiny::br(),
          "Some coordinates are outside valid ranges (latitude: -90 to 90, longitude: -180 to 180). Please check your data."
        )
      } else if (lat_range[1] < -60 || lat_range[2] > 60) {
        # Warn if latitudes suggest possible reversal (Central Africa is between ~20°S and 20°N)
        shiny::div(
          class = "alert alert-warning",
          style = "margin-bottom: 10px;",
          shiny::icon("exclamation-circle"),
          shiny::strong(" Unusual Coordinates - Possible Lat/Lon Reversal?"),
          shiny::br(),
          sprintf("Latitudes range from %.2f° to %.2f°. Central African plots should be between -20° and 20°N. Please verify coordinates are correct.",
                  lat_range[1], lat_range[2])
        )
      } else {
        NULL
      }

      shiny::tagList(
        shiny::hr(),
        shiny::h4(
          shiny::icon("map-marked-alt"),
          " Plot Locations Preview",
          style = "margin-top: 30px; margin-bottom: 15px;"
        ),
        warning_ui,
        shiny::p(
          sprintf("Showing %d plot location(s) on the map. Verify that locations match your expectations.", nrow(coords_data)),
          style = "color: #6c757d; margin-bottom: 10px;"
        ),
        shiny::div(
          class = "alert alert-info",
          style = "background-color: #e7f3ff; border-left: 4px solid #007bff; margin-bottom: 15px;",
          shiny::icon("info-circle"),
          shiny::strong(" Plots not where you expected?"),
          shiny::br(),
          "If your plots appear in the wrong location or continent, latitude and longitude columns may be reversed in your dataset.",
          shiny::br(),
          shiny::strong("No need to modify your file!"),
          " Simply go back to Step 3 (Column Mapping) and switch the ddlat/ddlon mappings."
        ),
        leaflet::leafletOutput(session$ns("location_map"), height = "400px")
      )
    })

    # Render the map
    output$location_map <- leaflet::renderLeaflet({
      shiny::req(cleaned_data())

      data <- cleaned_data()

      # Check if coordinates are available
      if (!all(c("ddlat", "ddlon") %in% names(data))) {
        return(NULL)
      }

      # Filter valid coordinates and coerce to numeric
      coords_data <- data[!is.na(data$ddlat) & !is.na(data$ddlon), ]

      if (nrow(coords_data) == 0) {
        return(NULL)
      }

      # Coerce coordinates to numeric for map rendering
      coords_data$ddlat <- suppressWarnings(as.numeric(coords_data$ddlat))
      coords_data$ddlon <- suppressWarnings(as.numeric(coords_data$ddlon))

      # Remove any rows with non-numeric coordinates
      coords_data <- coords_data[!is.na(coords_data$ddlat) & !is.na(coords_data$ddlon), ]

      if (nrow(coords_data) == 0) {
        return(NULL)
      }

      # Create popup text
      if ("plot_name" %in% names(coords_data)) {
        popup_text <- sprintf(
          "<strong>%s</strong><br>Lat: %.4f<br>Lon: %.4f",
          coords_data$plot_name,
          coords_data$ddlat,
          coords_data$ddlon
        )
      } else {
        popup_text <- sprintf(
          "Lat: %.4f<br>Lon: %.4f",
          coords_data$ddlat,
          coords_data$ddlon
        )
      }

      # Create map
      leaflet::leaflet(coords_data) %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(
          lng = ~ddlon,
          lat = ~ddlat,
          popup = popup_text,
          clusterOptions = leaflet::markerClusterOptions()
        ) %>%
        leaflet::fitBounds(
          lng1 = min(coords_data$ddlon, na.rm = TRUE),
          lat1 = min(coords_data$ddlat, na.rm = TRUE),
          lng2 = max(coords_data$ddlon, na.rm = TRUE),
          lat2 = max(coords_data$ddlat, na.rm = TRUE)
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
      shiny::req(display_data())

      # Show first 100 rows, exclude internal columns
      preview_data <- head(display_data(), 100)
      preview_data <- preview_data[, !names(preview_data) %in% ".row_idx", drop = FALSE]

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
          nrow(display_data())
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
        shiny::req(display_data())
        export_data <- display_data()
        export_data <- export_data[, !names(export_data) %in% ".row_idx", drop = FALSE]

        # Enrich data with lookup names (same as preview display)
        enriched_data <- .enrich_preview_with_lookup_names(export_data)

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
        shiny::req(display_data())
        export_data <- display_data()
        export_data <- export_data[, !names(export_data) %in% ".row_idx", drop = FALSE]

        # Enrich data with lookup names (same as preview display)
        enriched_data <- .enrich_preview_with_lookup_names(export_data)

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
        id_to_name <- stats::setNames(method_lookup$method, method_lookup$id_method)

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
        id_to_name <- stats::setNames(country_lookup$country, country_lookup$id_country)

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
      id_to_name <- stats::setNames(people_lookup$colnam, people_lookup$id_table_colnam)

      # Process each people column
      for (col in people_cols) {
        if (col %in% names(enriched_data)) {
          # People columns can have comma-separated values
          enriched_data[[col]] <- sapply(enriched_data[[col]], function(cell_value) {
            if (is.na(cell_value) || trimws(cell_value) == "") return(cell_value)

            # Split by comma
            values_list <- strsplit(as.character(cell_value), ",")[[1]]
            values_list <- trimws(values_list)

            # Process each value individually - convert if numeric (ID), keep if text (name)
            enriched_values <- sapply(values_list, function(val) {
              # Check if this specific value is numeric (an ID)
              is_id <- suppressWarnings(!is.na(as.numeric(val)))

              if (is_id) {
                # It's an ID - try to replace with name
                name <- id_to_name[[as.character(val)]]
                if (!is.null(name)) return(name) else return(val)
              } else {
                # It's already a name - keep as is
                return(val)
              }
            })

            # Join back with commas
            return(paste(enriched_values, collapse = ", "))
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
