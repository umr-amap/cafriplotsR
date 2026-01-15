#' Plot Metadata Viewer Module - UI
#'
#' UI component for displaying plot metadata with interactive map and table
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_plot_metadata_viewer_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("title_ui")),

    # Status message
    shiny::uiOutput(ns("status_message")),

    # Map panel
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns("map_title_ui")),
        leaflet::leafletOutput(ns("plot_map"), height = "400px")
      )
    ),

    shiny::br(),

    # Metadata table with selection
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::uiOutput(ns("table_title_ui")),
        DT::DTOutput(ns("metadata_table"))
      )
    ),

    shiny::br(),

    # Selection summary
    shiny::uiOutput(ns("selection_summary")),

    shiny::br(),

    # Metadata download panel
    shiny::div(
      id = ns("metadata_download_panel"),
      style = "display: none;",
      shiny::wellPanel(
        shiny::uiOutput(ns("download_title_ui")),
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::uiOutput(ns("download_excel_ui"))
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("download_csv_ui"))
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("download_rds_ui"))
          )
        )
      )
    )
  )
}

#' Plot Metadata Viewer Module - Server
#'
#' Server logic for plot metadata display and selection
#'
#' @param id Module namespace ID
#' @param metadata Reactive containing plot metadata from query_plots()
#' @param i18n Reactive returning shiny.i18n translator
#'
#' @return A reactive containing selected plot IDs
#'
#' @keywords internal
#' @export
mod_plot_metadata_viewer_server <- function(id, metadata, i18n) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      selected_rows = NULL,
      map_click_id = NULL
    )

    # Title UIs
    output$title_ui <- shiny::renderUI({
      shiny::h4(i18n()$t("Plot Metadata & Selection"))
    })

    output$map_title_ui <- shiny::renderUI({
      shiny::h5(i18n()$t("Interactive Map"))
    })

    output$table_title_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h5(i18n()$t("Plot Metadata Table")),
        shiny::p(
          class = "text-muted",
          i18n()$t("Click on map markers or select rows in the table below. Selected plots will be used for individual extraction.")
        )
      )
    })

    # Initialize selection when metadata is received
    shiny::observe({
      meta <- metadata()

      if (is.null(meta)) {
        cli::cli_alert_info("No metadata available yet")
        return()
      }

      cli::cli_alert_success("Metadata received! {nrow(meta)} plots - triggering map and table rendering")

      # Select ALL rows by default
      if (is.null(rv$selected_rows)) {
        rv$selected_rows <- 1:nrow(meta)
        cli::cli_alert_info("All {nrow(meta)} plots selected by default")
      }
    })

    # Status message
    output$status_message <- shiny::renderUI({
      if (is.null(metadata())) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " ", i18n()$t("Execute a query to view plot metadata")
          )
        )
      }

      n_plots <- nrow(metadata())
      cli::cli_alert_info("Rendering status message for {n_plots} plots")
      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"),
        sprintf(" %s", sprintf(i18n()$t("Found %d plot(s)"), n_plots))
      )
    })

    # Prepare spatial data for map
    map_data <- shiny::reactive({
      shiny::req(metadata())

      meta <- metadata()
      cli::cli_alert_info("Preparing map data for {nrow(meta)} plots")

      # Check for coordinate columns (handle both naming conventions)
      lat_col <- if ("latitude" %in% names(meta)) "latitude" else if ("ddlat" %in% names(meta)) "ddlat" else NULL
      lon_col <- if ("longitude" %in% names(meta)) "longitude" else if ("ddlon" %in% names(meta)) "ddlon" else NULL

      if (is.null(lat_col) || is.null(lon_col)) {
        cli::cli_alert_warning("No coordinate columns found (tried: latitude/longitude, ddlat/ddlon)")
        cli::cli_alert_info("Available columns: {paste(names(meta), collapse = ', ')}")
        return(NULL)
      }

      cli::cli_alert_info("Using coordinate columns: {lon_col}, {lat_col}")

      # Remove rows with missing coordinates
      meta_with_coords <- meta[!is.na(meta[[lat_col]]) & !is.na(meta[[lon_col]]), ]
      cli::cli_alert_info("{nrow(meta_with_coords)} plots have coordinates")

      if (nrow(meta_with_coords) == 0) {
        cli::cli_alert_warning("No plots with valid coordinates")
        return(NULL)
      }

      # Convert to sf object
      tryCatch({
        sf_obj <- sf::st_as_sf(
          meta_with_coords,
          coords = c(lon_col, lat_col),
          crs = 4326
        )
        cli::cli_alert_success("Created sf object with {nrow(sf_obj)} features")
        sf_obj
      }, error = function(e) {
        cli::cli_alert_danger("Failed to create spatial data: {e$message}")
        NULL
      })
    })

    # Render map
    output$plot_map <- leaflet::renderLeaflet({
      cli::cli_alert_info("=== renderLeaflet called ===")
      cli::cli_alert_info("Metadata status: {if(is.null(metadata())) 'NULL' else paste(nrow(metadata()), 'rows')}")
      cli::cli_alert_info("About to call map_data()...")

      map_result <- map_data()

      cli::cli_alert_info("map_data() returned: {if(is.null(map_result)) 'NULL' else paste('sf object with', nrow(map_result), 'features')}")

      shiny::req(map_result)

      sf_data <- map_data()
      cli::cli_alert_info("Map data has {nrow(sf_data)} plots")

      # Extract coordinates from sf object
      coords <- sf::st_coordinates(sf_data)

      # Create popup content
      popup_content <- paste0(
        "<strong>", sf_data$plot_name, "</strong><br/>",
        "Country: ", sf_data$country, "<br/>",
        "Method: ", sf_data$method, "<br/>",
        if ("locality_name" %in% names(sf_data)) paste0("Locality: ", sf_data$locality_name, "<br/>") else "",
        if ("area_ha" %in% names(sf_data)) paste0("Area: ", sf_data$area_ha, " ha<br/>") else "",
        "<em>Click to select</em>"
      )

      # Create map
      cli::cli_alert_info("Creating leaflet map...")

      tryCatch({
        map_obj <- leaflet::leaflet(sf_data) %>%
          leaflet::addTiles(group = "OpenStreetMap") %>%
          leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
          leaflet::addProviderTiles("Esri.WorldPhysical", group = "Physical") %>%
          leaflet::addCircleMarkers(
            lng = coords[, 1],
            lat = coords[, 2],
            popup = popup_content,
            label = sf_data$plot_name,
            radius = 8,
            fillColor = "#3388ff",
            fillOpacity = 0.8,
            color = "#fff",
            weight = 2
          ) %>%
          leaflet::addLayersControl(
            baseGroups = c("OpenStreetMap", "Satellite", "Physical"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )

        cli::cli_alert_success("Map created successfully with {nrow(sf_data)} markers")
        map_obj
      }, error = function(e) {
        cli::cli_alert_danger("Failed to create leaflet map: {e$message}")
        # Return empty map on error
        leaflet::leaflet() %>% leaflet::addTiles()
      })
    })

    # Render metadata table
    output$metadata_table <- DT::renderDT({
      shiny::req(metadata())

      meta <- metadata()

      # Select key columns to display (handle both column naming conventions)
      display_cols <- c(
        "plot_name", "country", "method", "locality_name",
        "ddlat", "ddlon", "latitude", "longitude", "area_ha", "nb_subplot",
        "id_liste_plots", "id_plot"
      )

      # Keep only existing columns
      display_cols <- intersect(display_cols, names(meta))

      meta_display <- meta[, display_cols, drop = FALSE]

      DT::datatable(
        meta_display,
        selection = list(mode = "multiple", selected = rv$selected_rows),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE,
        class = "display nowrap"
      )
    })

    # Handle table row selection
    shiny::observeEvent(input$metadata_table_rows_selected, {
      rv$selected_rows <- input$metadata_table_rows_selected
    })

    # Get selected plot IDs
    selected_plot_ids <- shiny::reactive({
      shiny::req(metadata())

      meta <- metadata()

      # Find the plot ID column - id_liste_plots is the primary key in data_liste_plots
      id_col <- if ("id_liste_plots" %in% names(meta)) {
        "id_liste_plots"
      } else if ("id_plot" %in% names(meta)) {
        "id_plot"
      } else {
        cli::cli_alert_danger("No plot ID column found! Available columns: {paste(names(meta), collapse = ', ')}")
        return(NULL)
      }

      cli::cli_alert_info("Using plot ID column: {id_col}")

      # If nothing selected, use ALL plots
      if (is.null(rv$selected_rows) || length(rv$selected_rows) == 0) {
        all_ids <- meta[[id_col]]
        cli::cli_alert_info("No rows selected, using ALL {length(all_ids)} plots")
        return(all_ids)
      }

      selected_ids <- meta[[id_col]][rv$selected_rows]
      cli::cli_alert_info("Selected {length(selected_ids)} plots: {paste(selected_ids, collapse = ', ')}")
      selected_ids
    })

    # Selection summary
    output$selection_summary <- shiny::renderUI({
      shiny::req(metadata())

      selected_ids <- selected_plot_ids()
      n_total <- nrow(metadata())
      n_selected <- length(selected_ids)

      # Check if all plots are selected
      if (n_selected == n_total) {
        return(
          shiny::div(
            class = "alert alert-success",
            shiny::icon("check-circle"),
            sprintf(" All %d plot(s) selected", n_selected),
            shiny::tags$br(),
            shiny::tags$small("Unselect rows in the table to exclude specific plots")
          )
        )
      }

      # Some plots selected
      selected_names <- metadata()$plot_name[rv$selected_rows]

      shiny::div(
        class = "alert alert-primary",
        shiny::icon("check-square"),
        sprintf(" %d of %d plot(s) selected: ", n_selected, n_total),
        shiny::tags$br(),
        shiny::tags$small(paste(selected_names, collapse = ", "))
      )
    })

    # Show download panel when metadata available
    shiny::observe({
      shiny::req(metadata())
      shinyjs::show("metadata_download_panel")
    })

    # Download panel title
    output$download_title_ui <- shiny::renderUI({
      shiny::h5(shiny::icon("download"), " ", i18n()$t("Download Plot Metadata"))
    })

    # Download button UIs
    output$download_excel_ui <- shiny::renderUI({
      shiny::downloadButton(
        ns("download_metadata_excel"),
        i18n()$t("Excel (.xlsx)"),
        class = "btn-primary btn-block"
      )
    })

    output$download_csv_ui <- shiny::renderUI({
      shiny::downloadButton(
        ns("download_metadata_csv"),
        i18n()$t("CSV (.csv)"),
        class = "btn-primary btn-block"
      )
    })

    output$download_rds_ui <- shiny::renderUI({
      shiny::downloadButton(
        ns("download_metadata_rds"),
        i18n()$t("R Object (.rds)"),
        class = "btn-primary btn-block"
      )
    })

    # Excel download handler
    output$download_metadata_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("plot_metadata_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        shiny::req(metadata())
        writexl::write_xlsx(list(metadata = metadata()), path = file)
      }
    )

    # CSV download handler
    output$download_metadata_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("plot_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        shiny::req(metadata())
        readr::write_csv(metadata(), file)
      }
    )

    # RDS download handler
    output$download_metadata_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("plot_metadata_", format(Sys.Date(), "%Y%m%d"), ".rds")
      },
      content = function(file) {
        shiny::req(metadata())
        saveRDS(metadata(), file = file)
      }
    )

    # Return selected IDs
    return(selected_plot_ids)
  })
}
