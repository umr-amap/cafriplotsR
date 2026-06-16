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
    # All UI elements rendered dynamically for i18n support
    shiny::uiOutput(ns("results_ui"))
  )
}

#' Results Display Module - Server
#'
#' Server logic for results display and download
#'
#' @param id Module namespace ID
#' @param results Reactive containing query_plots() results
#' @param individual_features_results Reactive containing query_individual_features() results (optional)
#' @param i18n Reactive returning shiny.i18n translator
#' @param con Reactive returning a database connection (for column documentation)
#' @param citation_data Reactive returning a citation summary data.frame (optional, see mod_citation_panel_server)
#'
#' @return NULL
#'
#' @keywords internal
#' @export
mod_results_display_server <- function(id, results, individual_features_results = NULL, i18n, con = NULL, citation_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (!is.null(citation_data)) {
      mod_citation_panel_server("citations", citation_data = citation_data, i18n = i18n)
    }

    # Main UI with translations
    output$results_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::h4(i18n()$t("Extraction Results")),

        # Status message
        shiny::uiOutput(ns("status_message")),

        # Download panel
        shiny::div(
          id = ns("download_panel"),
          style = "display: none;",
          shiny::wellPanel(
            shiny::h5(shiny::icon("download"), " ", i18n()$t("Download Results")),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::downloadButton(
                  ns("download_excel"),
                  i18n()$t("Excel (.xlsx)"),
                  class = "btn-primary btn-block"
                )
              ),
              shiny::column(
                3,
                shiny::downloadButton(
                  ns("download_csv"),
                  i18n()$t("CSV (zipped)"),
                  class = "btn-primary btn-block"
                )
              ),
              shiny::column(
                3,
                shiny::downloadButton(
                  ns("download_rds"),
                  i18n()$t("R Object (.rds)"),
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
              i18n()$t("Select tables to include in export:"),
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
    })

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
            " ", i18n()$t("Configure extraction options and click 'Extract Individuals' to view results")
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
        sprintf(" %s %d %s",
                i18n()$t("Extraction complete!"),
                total_rows,
                i18n()$t("total records"))
      )
    })

    # Get trait metadata from individual features
    features_metadata <- shiny::reactive({
      if (is.null(individual_features_results) || is.null(individual_features_results())) {
        return(NULL)
      }

      feat_res <- individual_features_results()
      if (!is.data.frame(feat_res)) {
        return(NULL)
      }

      tryCatch({
        # Get all trait metadata
        all_traits <- traits_list()

        # Method 1: Extract trait IDs directly (for long format)
        if ("id_trait" %in% names(feat_res)) {
          trait_ids <- unique(feat_res$id_trait)
          trait_ids <- trait_ids[!is.na(trait_ids)]

          if (length(trait_ids) > 0) {
            metadata <- all_traits %>%
              dplyr::filter(id_trait %in% !!trait_ids) %>%
              dplyr::arrange(trait)
            return(metadata)
          }
        }

        # Method 2: Extract trait names from column names (for wide format)
        # Look for trait columns by matching against trait names in database
        col_names <- names(feat_res)

        # Get trait names from database
        trait_names <- all_traits$trait

        # Find which traits appear in column names
        # Traits in wide format may have suffixes like _mean, _sd, _n or no suffix
        matched_traits <- character()
        for (trait_name in trait_names) {
          # Check for exact match or with common suffixes
          pattern <- paste0("^", trait_name, "(_mean|_sd|_n|_min|_max)?$")
          if (any(grepl(pattern, col_names))) {
            matched_traits <- c(matched_traits, trait_name)
          }
        }

        if (length(matched_traits) > 0) {
          metadata <- all_traits %>%
            dplyr::filter(trait %in% !!matched_traits) %>%
            dplyr::arrange(trait)

          cli::cli_alert_success("Found metadata for {nrow(metadata)} trait(s)")
          return(metadata)
        }

        cli::cli_alert_info("No trait metadata found")
        return(NULL)

      }, error = function(e) {
        cli::cli_alert_warning("Could not fetch trait metadata: {e$message}")
        return(NULL)
      })
    })

    # Column documentation reactive
    column_docs <- shiny::reactive({
      shiny::req(results())
      tryCatch({
        describe_columns(results(), con = if (!is.null(con)) con() else NULL)
      }, error = function(e) {
        message("Could not generate column documentation: ", e$message)
        NULL
      })
    })

    # Combined documentation data.frame (used for display and export)
    coldoc_combined_df <- shiny::reactive({
      docs <- column_docs()
      if (is.null(docs)) return(NULL)

      all_parts <- list()
      if (inherits(docs, "column_documentation")) {
        for (tab_name in names(docs)) {
          doc_df <- docs[[tab_name]]
          if (is.data.frame(doc_df) && nrow(doc_df) > 0) {
            all_parts[[tab_name]] <- cbind(
              data.frame(table = tab_name, stringsAsFactors = FALSE),
              doc_df
            )
          }
        }
      } else if (inherits(docs, "column_documentation_table") && is.data.frame(docs) && nrow(docs) > 0) {
        all_parts[["data"]] <- cbind(
          data.frame(table = "data", stringsAsFactors = FALSE),
          docs
        )
      }
      if (length(all_parts) == 0) return(NULL)
      result <- do.call(rbind, all_parts)
      rownames(result) <- NULL
      result
    })

    # Get table names from results
    table_names <- shiny::reactive({
      table_list <- character()

      # Add tables from main results
      if (!is.null(results())) {
        res <- results()

        if (is.data.frame(res)) {
          table_list <- c(table_list, "data")
        } else if (is.list(res)) {
          # Get names of data.frame components
          table_list <- c(table_list, names(Filter(is.data.frame, res)))
        }
      }

      # Add individual features if available (always a data.frame)
      if (!is.null(individual_features_results) && !is.null(individual_features_results())) {
        feat_res <- individual_features_results()
        if (is.data.frame(feat_res)) {
          table_list <- c(table_list, "individual_features")

          # Add metadata table if available
          if (!is.null(features_metadata())) {
            table_list <- c(table_list, "features_metadata")
          }
        }
      }

      if (length(table_list) == 0) {
        return(NULL)
      }

      table_list
    })

    # Update table export checkboxes
    shiny::observe({
      shiny::req(table_names())

      all_choices <- table_names()

      # Add column documentation if available
      if (!is.null(coldoc_combined_df())) {
        all_choices <- c(all_choices, "column_documentation")
      }

      labels <- setNames(
        all_choices,
        sapply(all_choices, function(x) {
          if (x == "column_documentation") {
            i18n()$t("Column Documentation")
          } else {
            gsub("_", " ", tools::toTitleCase(x))
          }
        })
      )

      shiny::updateCheckboxGroupInput(
        session,
        "tables_to_export",
        choices = labels,
        selected = all_choices  # Select all by default
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
          i18n()$t("Shapefile (.zip)"),
          class = "btn-primary btn-block"
        )
      } else {
        NULL
      }
    })

    # Dynamically create result tabs
    output$results_tabs_ui <- shiny::renderUI({
      shiny::req(table_names())

      res <- results()
      feat_res <- individual_features_results()

      tabs <- lapply(table_names(), function(tab_name) {
        # Get the data from appropriate source
        if (tab_name == "individual_features") {
          tab_data <- feat_res
          title <- i18n()$t("Individual Features")
        } else if (tab_name == "features_metadata") {
          tab_data <- features_metadata()
          title <- i18n()$t("Features Metadata")
        } else if (is.data.frame(res)) {
          tab_data <- res
          title <- gsub("_", " ", tools::toTitleCase(tab_name))
        } else {
          tab_data <- res[[tab_name]]
          title <- gsub("_", " ", tools::toTitleCase(tab_name))
        }

        shiny::tabPanel(
          title = title,
          shiny::br(),
          # Add informative note for individual features
          if (tab_name == "individual_features") {
            shiny::div(
              class = "alert alert-info",
              style = "font-size: 0.9em; padding: 10px; margin-bottom: 15px;",
              shiny::HTML(paste0(
                "<strong>", i18n()$t("Note"), ":</strong> ",
                i18n()$t("Column 'id_data_individuals' corresponds to 'id_n' in the individuals table for joining.")
              ))
            )
          },
          DT::DTOutput(ns(paste0("table_", tab_name))),
          shiny::br(),
          shiny::div(
            class = "text-muted",
            sprintf("%s %d %s x %d %s",
                    i18n()$t("Showing"),
                    nrow(tab_data),
                    i18n()$t("rows"),
                    ncol(tab_data),
                    i18n()$t("columns"))
          )
        )
      })

      # Add Column Documentation tab
      coldoc_tab <- shiny::tabPanel(
        title = shiny::tagList(shiny::icon("book-open"), " ", i18n()$t("Column Documentation")),
        shiny::br(),
        shiny::div(
          class = "alert alert-info",
          style = "font-size: 0.9em; padding: 10px; margin-bottom: 15px;",
          shiny::HTML(paste0(
            "<strong>", i18n()$t("Note"), ":</strong> ",
            i18n()$t("Each row describes one output column: its original database name, a description, category, unit, and any contextual notes.")
          ))
        ),
        DT::DTOutput(ns("coldoc_combined"))
      )

      # Add Data Sources tab if citation data is available
      citation_tab_list <- if (!is.null(citation_data) && !is.null(citation_data())) {
        list(shiny::tabPanel(
          title = shiny::tagList(shiny::icon("book"), " ", i18n()$t("Data Sources")),
          shiny::br(),
          mod_citation_panel_ui(ns("citations"))
        ))
      } else {
        list()
      }

      # Create tabsetPanel with tabs + optional citation tab + documentation tab
      do.call(shiny::tabsetPanel, c(
        list(id = ns("results_tabs"), type = "tabs"),
        tabs,
        citation_tab_list,
        list(coldoc_tab)
      ))
    })

    # Render individual tables
    shiny::observe({
      shiny::req(table_names())

      res <- results()
      feat_res <- individual_features_results()

      lapply(table_names(), function(tab_name) {
        output_id <- paste0("table_", tab_name)

        output[[output_id]] <- DT::renderDT({
          # Get the data from appropriate source
          if (tab_name == "individual_features") {
            tab_data <- feat_res
          } else if (tab_name == "features_metadata") {
            tab_data <- features_metadata()
          } else if (is.data.frame(res)) {
            tab_data <- res
          } else {
            tab_data <- res[[tab_name]]
          }

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

    # Render combined column documentation table
    output$coldoc_combined <- DT::renderDT({
      combined <- coldoc_combined_df()
      if (is.null(combined)) return(NULL)

      # Use translated column names for display
      display_names <- c(
        table         = i18n()$t("Table"),
        column_name   = i18n()$t("Column Name"),
        original_name = i18n()$t("Original Name"),
        description   = i18n()$t("Description"),
        category      = i18n()$t("Category"),
        unit          = i18n()$t("Unit"),
        notes         = i18n()$t("Notes")
      )
      for (orig in names(display_names)) {
        if (orig %in% names(combined)) {
          names(combined)[names(combined) == orig] <- display_names[[orig]]
        }
      }

      DT::datatable(
        combined,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "Bfrtip",
          ordering = TRUE
        ),
        rownames = FALSE,
        class = "display compact",
        filter = "top"
      )
    })

    # Excel download
    output$download_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("query_plots_results_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        res <- results()
        tables_to_include <- input$tables_to_export

        # Prepare data for export
        data_list <- list()

        if (!is.null(res)) {
          if (is.data.frame(res)) {
            data_list$data <- res
          } else if (is.list(res)) {
            data_list <- res[intersect(names(res), tables_to_include)]
            # Keep only data.frames
            data_list <- Filter(is.data.frame, data_list)
          }
        }

        # Add individual features if selected (always a data.frame)
        if ("individual_features" %in% tables_to_include &&
            !is.null(individual_features_results) &&
            !is.null(individual_features_results())) {
          feat_res <- individual_features_results()
          if (is.data.frame(feat_res)) {
            data_list$individual_features <- feat_res
          }
        }

        # Add features metadata if selected
        if ("features_metadata" %in% tables_to_include &&
            !is.null(features_metadata())) {
          data_list$features_metadata <- features_metadata()
        }

        # Add column documentation if selected
        if ("column_documentation" %in% tables_to_include &&
            !is.null(coldoc_combined_df())) {
          data_list$column_documentation <- coldoc_combined_df()
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
        res <- results()
        tables_to_include <- input$tables_to_export

        # Prepare data for export
        data_list <- list()

        if (!is.null(res)) {
          if (is.data.frame(res)) {
            data_list$data <- res
          } else if (is.list(res)) {
            data_list <- res[intersect(names(res), tables_to_include)]
            data_list <- Filter(is.data.frame, data_list)
          }
        }

        # Add individual features if selected (always a data.frame)
        if ("individual_features" %in% tables_to_include &&
            !is.null(individual_features_results) &&
            !is.null(individual_features_results())) {
          feat_res <- individual_features_results()
          if (is.data.frame(feat_res)) {
            data_list$individual_features <- feat_res
          }
        }

        # Add features metadata if selected
        if ("features_metadata" %in% tables_to_include &&
            !is.null(features_metadata())) {
          data_list$features_metadata <- features_metadata()
        }

        # Add column documentation if selected
        if ("column_documentation" %in% tables_to_include &&
            !is.null(coldoc_combined_df())) {
          data_list$column_documentation <- coldoc_combined_df()
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
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(zipfile = file, files = basename(csv_files))
      }
    )

    # RDS download
    output$download_rds <- shiny::downloadHandler(
      filename = function() {
        paste0("query_plots_results_", format(Sys.Date(), "%Y%m%d"), ".rds")
      },
      content = function(file) {
        res <- results()
        tables_to_include <- input$tables_to_export

        # Prepare data for export
        data_to_save <- list()

        if (!is.null(res)) {
          if (is.data.frame(res)) {
            data_to_save$data <- res
          } else if (is.list(res)) {
            data_to_save <- res[intersect(names(res), tables_to_include)]
          }
        }

        # Add individual features if selected
        if ("individual_features" %in% tables_to_include &&
            !is.null(individual_features_results) &&
            !is.null(individual_features_results())) {
          data_to_save$individual_features <- individual_features_results()
        }

        # Add features metadata if selected
        if ("features_metadata" %in% tables_to_include &&
            !is.null(features_metadata())) {
          data_to_save$features_metadata <- features_metadata()
        }

        # Add column documentation if selected
        if ("column_documentation" %in% tables_to_include &&
            !is.null(coldoc_combined_df())) {
          data_to_save$column_documentation <- coldoc_combined_df()
        }

        # If only one table, save as data.frame instead of list
        if (length(data_to_save) == 1) {
          data_to_save <- data_to_save[[1]]
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
        old_wd <- setwd(temp_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(zipfile = file, files = basename(shp_files))
      }
    )

    return(NULL)
  })
}
