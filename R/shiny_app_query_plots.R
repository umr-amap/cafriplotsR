#' Query Plots Shiny App
#'
#' Main Shiny app function for interactive forest plot querying
#'
#' @param pool_main Main database connection pool (optional, will prompt for login)
#' @param language Character, initial language ("en" or "fr"), default: "fr"
#'
#' @return A Shiny app object
#' @keywords internal
#' @export
shiny_app_query_plots <- function(pool_main = NULL, language = "fr") {

  # Validate parameters
  language <- match.arg(language, c("en", "fr"))

  # Initialize translator (must be before UI for usei18n)
  translator <- init_translator()

  # Create UI
  ui <- function(request) {
    shiny::tagList(
      # Add shiny.i18n (required for automatic translation)
      shiny.i18n::usei18n(translator),

      # Add shinyjs for dynamic show/hide
      shinyjs::useShinyjs(),

      # CSS for better styling
      shiny::tags$head(
        shiny::tags$style(shiny::HTML("
          .well {
            background-color: #f8f9fa;
            border: 1px solid #dee2e6;
          }
          .btn-block {
            width: 100%;
          }
          .alert {
            margin-top: 10px;
            margin-bottom: 10px;
          }
          .module-section {
            margin-bottom: 30px;
          }
        "))
      ),

      # Login panel (shown if pool_main not provided)
      shiny::conditionalPanel(
        condition = "!output.authenticated",
        mod_database_login_ui("login")
      ),

      # Main app interface (shown after authentication)
      shiny::conditionalPanel(
        condition = "output.authenticated",

        # Language toggle (top right)
        shiny::absolutePanel(
          top = 10,
          right = 20,
          fixed = TRUE,
          draggable = FALSE,
          style = "z-index: 1000;",
          shiny::radioButtons(
            inputId = "selected_language",
            label = NULL,
            choices = c("EN" = "en", "FR" = "fr"),
            selected = language,
            inline = TRUE
          )
        ),

        # Navigation bar
        shiny::navbarPage(
          title = shiny::textOutput("app_title", inline = TRUE),
          id = "main_nav",
          windowTitle = "CafriplotsR - Plot Query Tool",

        # Page 1: Query Builder
        shiny::tabPanel(
          shiny::textOutput("tab_query_builder", inline = TRUE),
          value = "page_query",
          icon = shiny::icon("filter"),

          shiny::fluidPage(
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::h2(shiny::textOutput("page_query_title", inline = TRUE)),
                shiny::p(
                  class = "lead",
                  shiny::textOutput("page_query_subtitle", inline = TRUE)
                )
              )
            ),

            shiny::hr(),

            # Filters panel
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "module-section",
                  mod_plot_filters_ui("filters")
                )
              )
            )
          )
        ),

        # Page 2: Results
        shiny::tabPanel(
          shiny::textOutput("tab_results", inline = TRUE),
          value = "page_results",
          icon = shiny::icon("chart-line"),

          shiny::fluidPage(
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::h2(shiny::textOutput("page_results_title", inline = TRUE)),
                shiny::p(
                  class = "lead",
                  shiny::textOutput("page_results_subtitle", inline = TRUE)
                )
              )
            ),

            shiny::hr(),

            # Metadata viewer (map + table)
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "module-section",
                  mod_plot_metadata_viewer_ui("metadata")
                )
              )
            ),

            shiny::hr(),

            # Extraction configuration
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "module-section",
                  mod_extraction_config_ui("extraction")
                )
              )
            ),

            shiny::hr(),

            # Results display
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "module-section",
                  mod_results_display_ui("results")
                )
              )
            ),

            shiny::hr(),

            # Code preview (equivalent R code)
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "module-section",
                  mod_code_preview_ui("code_preview")
                )
              )
            )
          )
        ),

        # Page 3: Statistics & Visualizations
        shiny::tabPanel(
          shiny::textOutput("tab_statistics", inline = TRUE),
          value = "page_statistics",
          icon = shiny::icon("chart-bar"),

          shiny::fluidPage(
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::div(
                  class = "module-section",
                  mod_plot_statistics_ui("statistics")
                )
              )
            )
          )
        ),

        # About panel
        shiny::tabPanel(
          shiny::textOutput("tab_about", inline = TRUE),
          value = "page_about",
          icon = shiny::icon("info-circle"),

          shiny::fluidPage(
            shiny::br(),
            shiny::uiOutput("about_content")
          )
        )
        ) # End navbarPage
      ) # End conditionalPanel for main app
    )
  }

  # Create Server
  server <- function(input, output, session) {

    # Database authentication
    if (is.null(pool_main)) {
      # Use login module for authentication
      login_output <- mod_database_login_server("login")

      pool_reactive <- login_output$pool_main
      authenticated_reactive <- login_output$authenticated
    } else {
      # Pool provided, mark as authenticated
      pool_reactive <- shiny::reactive(pool_main)
      authenticated_reactive <- shiny::reactive(TRUE)

      # Store in global env
      .db_env$pool_main <- pool_main
      # Note: Pool cleanup is handled by cleanup_connections() below
    }

    # Sync language from login module to app language selector
    shiny::observe({
      lang <- login_output$language()
      shiny::req(lang)
      shiny::updateSelectInput(session, "selected_language", selected = lang)
    })

    # Stop app and quit R when browser is closed
    session$onSessionEnded(function() {
      # Clean up all connections and credentials
      tryCatch({
        cleanup_connections()
      }, error = function(e) {
        cli::cli_alert_warning("Failed to cleanup connections: {e$message}")
      })
      shiny::stopApp()
    })

    # Output for conditional panel (needs to be suspendable=FALSE)
    output$authenticated <- shiny::reactive({
      authenticated_reactive()
    })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    # Create reactive translator (shiny.i18n recommended pattern)
    i18n <- shiny::reactive({
      selected <- input$selected_language
      if (length(selected) == 1 && selected %in% translator$get_languages()) {
        translator$set_translation_language(selected)
      }
      translator
    })

    # App title
    output$app_title <- shiny::renderText({
      i18n()$t("CafriplotsR - Plot Query Tool")
    })

    # Tab labels
    output$tab_query_builder <- shiny::renderText({
      i18n()$t("Query Builder")
    })

    output$tab_results <- shiny::renderText({
      i18n()$t("Results & Extraction")
    })

    output$tab_statistics <- shiny::renderText({
      i18n()$t("Statistics")
    })

    output$tab_about <- shiny::renderText({
      i18n()$t("About")
    })

    # Page titles and subtitles
    output$page_query_title <- shiny::renderText({
      i18n()$t("Forest Plot Query Builder")
    })

    output$page_query_subtitle <- shiny::renderText({
      i18n()$t("Filter and select forest plots, then extract detailed individual tree data")
    })

    output$page_results_title <- shiny::renderText({
      i18n()$t("Query Results & Individual Extraction")
    })

    output$page_results_subtitle <- shiny::renderText({
      i18n()$t("View plot locations, select plots of interest, and extract individual tree data")
    })

    # About page content
    output$about_content <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::h2(i18n()$t("About CafriplotsR Query Tool")),
          shiny::hr(),
          shiny::h4(i18n()$t("Overview")),
          shiny::p(
            i18n()$t("This interactive tool provides a user-friendly interface for querying the CafriplotsR forest plot database."),
            i18n()$t("It wraps the powerful"), shiny::code("query_plots()"),
            i18n()$t("function with an intuitive two-stage workflow:")
          ),
          shiny::tags$ol(
            shiny::tags$li(
              shiny::strong(i18n()$t("Filter & Discover:")),
              " ", i18n()$t("Use various filters to find plots of interest, view them on an interactive map")
            ),
            shiny::tags$li(
              shiny::strong(i18n()$t("Select & Extract:")),
              " ", i18n()$t("Choose specific plots and extract detailed individual tree data with customizable options")
            )
          ),
          shiny::hr(),
          shiny::h4(i18n()$t("Features")),
          shiny::tags$ul(
            shiny::tags$li(shiny::icon("map"), " ", i18n()$t("Interactive map visualization with multiple basemaps")),
            shiny::tags$li(shiny::icon("filter"), " ", i18n()$t("Flexible filtering by country, plot name, method, and more")),
            shiny::tags$li(shiny::icon("table"), " ", i18n()$t("Dynamic table display with search and sort capabilities")),
            shiny::tags$li(shiny::icon("cog"), " ", i18n()$t("Multiple output styles for different analysis needs")),
            shiny::tags$li(shiny::icon("download"), " ", i18n()$t("Export results in Excel, CSV, RDS, or shapefile formats"))
          ),
          shiny::hr(),
          shiny::h4(i18n()$t("Package Information")),
          shiny::p(
            shiny::strong(i18n()$t("Version:")), " 1.7.2", shiny::br(),
            shiny::strong(i18n()$t("Authors:")), " Gilles Dauby, Hugo Leblanc", shiny::br(),
            shiny::strong(i18n()$t("Repository:")), " ", shiny::a(
              "github.com/umr-amap/cafriplotsR",
              href = "https://github.com/umr-amap/cafriplotsR",
              target = "_blank"
            ), shiny::br(),
            shiny::strong(i18n()$t("Documentation:")), " ", shiny::a(
              "umr-amap.github.io/cafriplotsR",
              href = "https://umr-amap.github.io/cafriplotsR",
              target = "_blank"
            )
          )
        )
      )
    })

    # Reactive values for data flow
    rv <- shiny::reactiveValues(
      metadata = NULL,
      individuals = NULL,
      individual_features = NULL,
      extracted_plot_ids = NULL,
      modules_initialized = FALSE
    )

    # Only initialize modules after authentication (runs once)
    shiny::observe({
      shiny::req(authenticated_reactive() == TRUE)
      shiny::req(pool_reactive())
      shiny::req(!rv$modules_initialized)  # Only run once

      cli::cli_alert_info("Initializing app modules...")

      # Module 1: Filters
      filter_output <- mod_plot_filters_server("filters", pool = pool_reactive, i18n = i18n)

      # Execute metadata query when filters are applied
      shiny::observeEvent(filter_output$execute_trigger(), {
        trigger_val <- filter_output$execute_trigger()
        cli::cli_alert_info("Execute trigger fired! Value: {trigger_val}")

        # Require counter > 0 (button was actually clicked)
        if (trigger_val == 0) {
          cli::cli_alert_info("Skipping execution - trigger value is 0")
          return()
        }

        # Reset any previous extraction results: filter parameters have changed
        rv$individuals <- NULL
        rv$individual_features <- NULL
        rv$extracted_plot_ids <- NULL

        cli::cli_alert_info("Getting filters...")
        # Get filters
        filters <- filter_output$filters()

        cli::cli_alert_info("Filters retrieved successfully")
        active_filters <- names(Filter(Negate(is.null), filters))
        if (length(active_filters) > 0) {
          cli::cli_alert_info("Active filters: {paste(active_filters, collapse = ', ')}")
        } else {
          cli::cli_alert_info("No active filters - will query all plots")
        }

        shiny::withProgress({
          tryCatch({
            # Query metadata only (no individuals)
            cli::cli_alert_info("Querying plot metadata...")

          result <- query_plots(
            country = filters$country,
            plot_name = filters$plot_name,
            locality_name = filters$locality_name,
            method = filters$method,
            tag = filters$tag,
            id_plot = filters$id_plot,
            id_individual = filters$id_individual,
            id_tax = filters$id_tax,
            id_specimen = filters$id_specimen,
            exact_match = filters$exact_match,
            extract_individuals = FALSE,  # Only metadata at this stage
            extract_traits = FALSE,
            con = pool_reactive(), 
            remove_ids = FALSE, output_style = "full"
          )

          # Store metadata (extract from list if needed)
          # query_plots() now always returns list with $metadata (not $meta_data)
          if (is.list(result) && "metadata" %in% names(result)) {
            rv$metadata <- result$metadata
            cli::cli_alert_info("Extracted 'metadata' from list result ({nrow(result$metadata)} rows)")
          } else if (is.list(result) && "extract" %in% names(result)) {
            rv$metadata <- result$extract
            cli::cli_alert_info("Extracted 'extract' from list result ({nrow(result$extract)} rows)")
          } else if (is.data.frame(result)) {
            rv$metadata <- result
            cli::cli_alert_info("Using data.frame result directly ({nrow(result)} rows)")
          } else {
            cli::cli_alert_warning("Unexpected result structure!")
            rv$metadata <- NULL
          }

          # Switch to results page
          shiny::updateTabsetPanel(session, "main_nav", selected = "page_results")

          # Show success notification
          n_plots <- if (is.data.frame(rv$metadata)) nrow(rv$metadata) else 0
          cli::cli_alert_success("Found {n_plots} plot(s)")
          shiny::showNotification(
            sprintf("Found %d plot(s)", n_plots),
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          cli::cli_alert_danger("Query failed: {e$message}")
          shiny::showNotification(
            paste("Error:", e$message),
            type = "error",
            duration = 10
          )
        })
      }, message = "Querying database...")
      })

      # Module 2: Metadata Viewer (map + table with selection)
      selected_plots <- mod_plot_metadata_viewer_server(
        "metadata",
        metadata = shiny::reactive(rv$metadata),
        i18n = i18n
      )

      # Module 3: Extraction Configuration
      extraction_output <- mod_extraction_config_server(
        "extraction",
        selected_plots = selected_plots,
        i18n = i18n
      )

      # Execute individual extraction when requested
      shiny::observeEvent(extraction_output$execute_trigger(), {
        shiny::req(selected_plots())

        filters <- filter_output$filters()
        options <- extraction_output$options()

        shiny::withProgress({
          tryCatch({
            cli::cli_alert_info("Extracting individual data for selected plots...")

            result <- query_plots(
              id_plot = selected_plots(),
              tag = filters$tag,
              extract_individuals = TRUE,
              extract_traits = options$extract_traits,
              extract_individual_features = options$extract_individual_features,
              extract_subplot_features = options$extract_subplot_features,
              traits_to_genera = options$traits_to_genera,
              output_style = options$output_style,
              census_strategy = options$census_strategy,
              show_multiple_census = options$show_multiple_census,
              individual_features_format = options$individual_features_format,
              concatenate_stem = options$concatenate_stem,
              remove_ids = options$remove_ids,
              issues = options$issues,
              con = pool_reactive()
            )

            # Store results
            rv$individuals <- result
            rv$extracted_plot_ids <- selected_plots()

            # Show success notification
            shiny::showNotification(
              "Individual extraction complete!",
              type = "message",
              duration = 5
            )

          }, error = function(e) {
            cli::cli_alert_danger("Extraction failed: {e$message}")
            shiny::showNotification(
              paste("Error:", e$message),
              type = "error",
              duration = 10
            )
          })
        }, message = "Extracting individuals...")
      })

      # Execute individual features query when requested
      shiny::observeEvent(extraction_output$individual_features_trigger(), {
        feat_opts <- extraction_output$individual_features_options()

        # Only execute if enabled
        if (!isTRUE(feat_opts$enabled)) {
          return()
        }

        # Check that individuals data has been extracted first
        if (is.null(rv$individuals)) {
          cli::cli_alert_warning("Must extract individuals first before querying individual features")
          shiny::showNotification(
            i18n()$t("Please extract individuals first before querying individual features"),
            type = "warning",
            duration = 5
          )
          return()
        }

        shiny::withProgress({
          tryCatch({
            cli::cli_alert_info("Querying individual features from extracted individuals...")

            # Extract individual IDs from the individuals data
            individuals_data <- rv$individuals

            # Handle different result structures
            if (is.data.frame(individuals_data)) {
              # Simple data.frame
              if (!"id_n" %in% names(individuals_data)) {
                stop("Column 'id_n' not found in individuals data")
              }
              individual_ids <- unique(individuals_data$id_n)
            } else if (is.list(individuals_data)) {
              # List of data.frames - look for 'extract' or 'individuals' table
              if ("extract" %in% names(individuals_data) && is.data.frame(individuals_data$extract)) {
                if (!"id_n" %in% names(individuals_data$extract)) {
                  stop("Column 'id_n' not found in extract table")
                }
                individual_ids <- unique(individuals_data$extract$id_n)
              } else if ("individuals" %in% names(individuals_data) && is.data.frame(individuals_data$individuals)) {
                if (!"id_n" %in% names(individuals_data$individuals)) {
                  stop("Column 'id_n' not found in individuals table")
                }
                individual_ids <- unique(individuals_data$individuals$id_n)
              } else {
                # Try to find any data.frame with id_n column
                found <- FALSE
                for (table_name in names(individuals_data)) {
                  if (is.data.frame(individuals_data[[table_name]]) &&
                      "id_n" %in% names(individuals_data[[table_name]])) {
                    individual_ids <- unique(individuals_data[[table_name]]$id_n)
                    found <- TRUE
                    cli::cli_alert_info("Using 'id_n' from table: {table_name}")
                    break
                  }
                }
                if (!found) {
                  stop("Could not find 'id_n' column in individuals data")
                }
              }
            } else {
              stop("Unexpected individuals data structure")
            }

            if (length(individual_ids) == 0) {
              cli::cli_alert_warning("No individual IDs found in extracted data")
              shiny::showNotification(
                i18n()$t("No individuals found in extracted data"),
                type = "warning",
                duration = 5
              )
              return()
            }

            cli::cli_alert_info("Found {length(individual_ids)} individual ID(s) from extracted data")

            # Query individual features
            con <- pool_reactive()
            result <- query_individual_features(
              individual_ids = individual_ids,
              trait_ids = feat_opts$trait_ids,
              include_multi_census = feat_opts$include_multi_census,
              format = feat_opts$format,
              issues = feat_opts$issues,
              include_metadata = feat_opts$include_metadata,
              census_strategy = feat_opts$census_strategy,
              con = con
            )

            # Store results
            rv$individual_features <- result

            # Show success notification
            n_rows <- if (is.data.frame(result)) nrow(result) else 0
            cli::cli_alert_success("Extracted {n_rows} individual feature record(s)")
            shiny::showNotification(
              sprintf("%s %d %s",
                      i18n()$t("Individual features extraction complete!"),
                      n_rows,
                      i18n()$t("total records")),
              type = "message",
              duration = 5
            )

          }, error = function(e) {
            cli::cli_alert_danger("Individual features query failed: {e$message}")
            shiny::showNotification(
              paste("Error:", e$message),
              type = "error",
              duration = 10
            )
          })
        }, message = "Querying individual features...")
      })

      # Module 4: Results Display
      mod_results_display_server(
        "results",
        results = shiny::reactive(rv$individuals),
        individual_features_results = shiny::reactive(rv$individual_features),
        i18n = i18n,
        con = pool_reactive
      )

      # Module 5: Code Preview (equivalent R code)
      mod_code_preview_server(
        "code_preview",
        filters = filter_output$filters,
        selected_plots = shiny::reactive(rv$extracted_plot_ids),
        extraction_options = extraction_output$options,
        metadata_available = shiny::reactive(!is.null(rv$metadata)),
        individuals_available = shiny::reactive(!is.null(rv$individuals)),
        i18n = i18n,
        individual_features_options = extraction_output$individual_features_options,
        individual_features_available = shiny::reactive(!is.null(rv$individual_features)),
        n_metadata_plots = shiny::reactive(if (is.null(rv$metadata)) 0L else nrow(rv$metadata))
      )

      # Module 6: Plot Statistics (new)
      mod_plot_statistics_server(
        "statistics",
        results = shiny::reactive(rv$individuals),
        pool_reactive = pool_reactive,
        i18n = i18n
      )

      # Reset extraction results when the plot selection changes (user ticks/unticks plots)
      shiny::observeEvent(selected_plots(), {
        rv$individuals <- NULL
        rv$individual_features <- NULL
        rv$extracted_plot_ids <- NULL
      }, ignoreInit = TRUE)

      # Reset extraction results when any extraction option changes (census strategy,
      # output style, individual_features_format, etc.)
      shiny::observeEvent(extraction_output$options(), {
        rv$individuals <- NULL
        rv$individual_features <- NULL
        rv$extracted_plot_ids <- NULL
      }, ignoreInit = TRUE)

      # Mark modules as initialized
      rv$modules_initialized <- TRUE
      cli::cli_alert_success("All modules initialized successfully!")
    })  # Close the observe block for module initialization
  }

  # Return app object
  shiny::shinyApp(ui = ui, server = server)
}
