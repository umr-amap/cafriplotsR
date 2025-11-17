#' Results Display Module - UI
#'
#' UI component for displaying query results and download options
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_results_display_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Extraction Results"),

    # Status message
    shiny::uiOutput(ns("status_message")),

    # Download panel
    shiny::div(
      id = ns("download_panel"),
      style = "display: none;",
      shiny::wellPanel(
        shiny::h5(shiny::icon("download"), " Download Results"),
        shiny::fluidRow(
          shiny::column(
            3,
            shiny::downloadButton(
              ns("download_excel"),
              "Excel (.xlsx)",
              class = "btn-primary btn-block"
            )
          ),
          shiny::column(
            3,
            shiny::downloadButton(
              ns("download_csv"),
              "CSV (zipped)",
              class = "btn-primary btn-block"
            )
          ),
          shiny::column(
            3,
            shiny::downloadButton(
              ns("download_rds"),
              "R Object (.rds)",
              class = "btn-primary btn-block"
            )
          ),
          shiny::column(
            3,
            shiny::uiOutput(ns("download_shapefile_ui"))
          )
        ),
        shiny::hr(),
        shiny::checkboxGroupInput(
          ns("tables_to_export"),
          "Select tables to include in export:",
          choices = NULL
        )
      )
    ),

    shiny::br(),

    # Results tabs (dynamically generated)
    shiny::div(
      id = ns("results_tabs_panel"),
      shiny::uiOutput(ns("results_tabs_ui"))
    )
  )
}

#' Results Display Module - Server
#'
#' Server logic for results display and download
#'
#' @param id Module namespace ID
#' @param results Reactive containing query_plots() results
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_results_display_server <- function(id, results) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show download panel when results available
    shiny::observe({
      shiny::req(results())
      shinyjs::show("download_panel")
    })

    # Status message
    output$status_message <- shiny::renderUI({
      if (is.null(results())) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            " Configure extraction options and click 'Extract Individuals' to view results"
          )
        )
      }

      # Count total rows across all tables
      res <- results()
      total_rows <- 0

      if (is.data.frame(res)) {
        total_rows <- nrow(res)
      } else if (is.list(res)) {
        total_rows <- sum(sapply(res, function(x) if (is.data.frame(x)) nrow(x) else 0))
      }

      shiny::div(
        class = "alert alert-success",
        shiny::icon("check-circle"),
        sprintf(" Extraction complete! %d total records", total_rows)
      )
    })

    # Get table names from results
    table_names <- shiny::reactive({
      shiny::req(results())

      res <- results()

      if (is.data.frame(res)) {
        return("data")
      } else if (is.list(res)) {
        # Get names of data.frame components
        names(Filter(is.data.frame, res))
      } else {
        NULL
      }
    })

    # Update table export checkboxes
    shiny::observe({
      shiny::req(table_names())

      # Create readable labels
      labels <- setNames(
        table_names(),
        sapply(table_names(), function(x) {
          # Convert snake_case to Title Case
          gsub("_", " ", tools::toTitleCase(x))
        })
      )

      shiny::updateCheckboxGroupInput(
        session,
        "tables_to_export",
        choices = labels,
        selected = table_names()  # Select all by default
      )
    })

    # Shapefile download UI (only if spatial data present)
    output$download_shapefile_ui <- shiny::renderUI({
      shiny::req(results())

      res <- results()
      has_spatial <- FALSE

      if (is.list(res)) {
        has_spatial <- any(sapply(res, function(x) inherits(x, "sf")))
      }

      if (has_spatial) {
        shiny::downloadButton(
          ns("download_shapefile"),
          "Shapefile (.zip)",
          class = "btn-primary btn-block"
        )
      } else {
        NULL
      }
    })

    # Dynamically create result tabs
    output$results_tabs_ui <- shiny::renderUI({
      shiny::req(results(), table_names())

      res <- results()
      tabs <- lapply(table_names(), function(tab_name) {
        # Get the data
        tab_data <- if (is.data.frame(res)) res else res[[tab_name]]

        shiny::tabPanel(
          title = gsub("_", " ", tools::toTitleCase(tab_name)),
          shiny::br(),
          DT::DTOutput(ns(paste0("table_", tab_name))),
          shiny::br(),
          shiny::div(
            class = "text-muted",
            sprintf("Showing %d rows × %d columns", nrow(tab_data), ncol(tab_data))
          )
        )
      })

      # Create tabsetPanel with tabs
      do.call(shiny::tabsetPanel, c(
        list(id = ns("results_tabs"), type = "tabs"),
        tabs
      ))
    })

    # Render individual tables
    shiny::observe({
      shiny::req(results(), table_names())

      res <- results()

      lapply(table_names(), function(tab_name) {
        output_id <- paste0("table_", tab_name)

        output[[output_id]] <- DT::renderDT({
          tab_data <- if (is.data.frame(res)) res else res[[tab_name]]

          DT::datatable(
            tab_data,
            options = list(
              pageLength = 25,
              scrollX = TRUE,
              scrollY = "400px",
              autoWidth = TRUE,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel", "colvis")
            ),
            rownames = FALSE,
            class = "display nowrap compact",
            filter = "top"
          )
        })
      })
    })

    # Excel download
    output$download_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("query_plots_results_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        shiny::req(results())

        res <- results()
        tables_to_include <- input$tables_to_export

        # Prepare data for export
        if (is.data.frame(res)) {
          data_list <- list(data = res)
        } else if (is.list(res)) {
          data_list <- res[intersect(names(res), tables_to_include)]
          # Keep only data.frames
          data_list <- Filter(is.data.frame, data_list)
        }

        # Write to Excel
        writexl::write_xlsx(data_list, path = file)
      }
    )

    # CSV download (zipped)
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("query_plots_results_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        shiny::req(results())

        res <- results()
        tables_to_include <- input$tables_to_export

        # Prepare data for export
        if (is.data.frame(res)) {
          data_list <- list(data = res)
        } else if (is.list(res)) {
          data_list <- res[intersect(names(res), tables_to_include)]
          data_list <- Filter(is.data.frame, data_list)
        }

        # Create temp directory
        temp_dir <- tempdir()
        csv_files <- character()

        # Write CSVs
        for (name in names(data_list)) {
          csv_path <- file.path(temp_dir, paste0(name, ".csv"))
          readr::write_csv(data_list[[name]], csv_path)
          csv_files <- c(csv_files, csv_path)
        }

        # Zip files
        zip::zip(
          zipfile = file,
          files = basename(csv_files),
          root = temp_dir
        )
      }
    )

    # RDS download
    output$download_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("query_plots_results_", format(Sys.Date(), "%Y%m%d"), ".rds")
      },
      content = function(file) {
        shiny::req(results())

        res <- results()
        tables_to_include <- input$tables_to_export

        # Prepare data for export
        if (is.data.frame(res)) {
          data_to_save <- res
        } else if (is.list(res)) {
          data_to_save <- res[intersect(names(res), tables_to_include)]
        }

        saveRDS(data_to_save, file = file)
      }
    )

    # Shapefile download
    output$download_shapefile <- shiny::downloadHandler(
      filename = function() {
        paste0("query_plots_spatial_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        shiny::req(results())

        res <- results()

        # Find sf objects
        sf_objects <- Filter(function(x) inherits(x, "sf"), res)

        if (length(sf_objects) == 0) {
          return(NULL)
        }

        # Create temp directory
        temp_dir <- tempdir()
        shp_files <- character()

        # Write shapefiles
        for (name in names(sf_objects)) {
          shp_path <- file.path(temp_dir, paste0(name, ".shp"))
          sf::st_write(sf_objects[[name]], shp_path, delete_layer = TRUE, quiet = TRUE)

          # Collect all shapefile components
          shp_files <- c(
            shp_files,
            list.files(temp_dir, pattern = paste0("^", name, "\\."), full.names = TRUE)
          )
        }

        # Zip files
        zip::zip(
          zipfile = file,
          files = basename(shp_files),
          root = temp_dir
        )
      }
    )

    return(NULL)
  })
}
