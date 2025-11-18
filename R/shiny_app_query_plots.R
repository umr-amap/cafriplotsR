#' Query Plots Shiny App
#'
#' Main Shiny app function for interactive forest plot querying
#'
#' @param pool_main Main database connection pool (optional, will prompt for login)
#' @param language Language for UI (default: "en", future: "fr")
#'
#' @return A Shiny app object
#' @keywords internal
#' @export
shiny_app_query_plots <- function(pool_main = NULL, language = "en") {

  # Create UI
  ui <- function(request) {
    shiny::tagList(
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

        # Navigation bar
        shiny::navbarPage(
          title = "CafriplotsR - Plot Query Tool",
          id = "main_nav",
          theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

        # Page 1: Query Builder
        shiny::tabPanel(
          "Query Builder",
          value = "page_query",
          icon = shiny::icon("filter"),

          shiny::fluidPage(
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::h2("Forest Plot Query Builder"),
                shiny::p(
                  class = "lead",
                  "Filter and select forest plots, then extract detailed individual tree data"
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
          "Results & Extraction",
          value = "page_results",
          icon = shiny::icon("chart-line"),

          shiny::fluidPage(
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::h2("Query Results & Individual Extraction"),
                shiny::p(
                  class = "lead",
                  "View plot locations, select plots of interest, and extract individual tree data"
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
            )
          )
        ),

        # About panel
        shiny::tabPanel(
          "About",
          value = "page_about",
          icon = shiny::icon("info-circle"),

          shiny::fluidPage(
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                12,
                shiny::h2("About CafriplotsR Query Tool"),
                shiny::hr(),
                shiny::h4("Overview"),
                shiny::p(
                  "This interactive tool provides a user-friendly interface for querying the CafriplotsR forest plot database.",
                  "It wraps the powerful", shiny::code("query_plots()"), "function with an intuitive two-stage workflow:"
                ),
                shiny::tags$ol(
                  shiny::tags$li(shiny::strong("Filter & Discover:"), " Use various filters to find plots of interest, view them on an interactive map"),
                  shiny::tags$li(shiny::strong("Select & Extract:"), " Choose specific plots and extract detailed individual tree data with customizable options")
                ),
                shiny::hr(),
                shiny::h4("Features"),
                shiny::tags$ul(
                  shiny::tags$li(shiny::icon("map"), " Interactive map visualization with multiple basemaps"),
                  shiny::tags$li(shiny::icon("filter"), " Flexible filtering by country, plot name, method, and more"),
                  shiny::tags$li(shiny::icon("table"), " Dynamic table display with search and sort capabilities"),
                  shiny::tags$li(shiny::icon("cog"), " Multiple output styles for different analysis needs"),
                  shiny::tags$li(shiny::icon("download"), " Export results in Excel, CSV, RDS, or shapefile formats")
                ),
                shiny::hr(),
                shiny::h4("Package Information"),
                shiny::p(
                  shiny::strong("Version:"), "1.7", shiny::br(),
                  shiny::strong("Authors:"), "Gilles Dauby, Hugo Leblanc", shiny::br(),
                  shiny::strong("Repository:"), shiny::a(
                    "github.com/umr-amap/cafriplotsR",
                    href = "https://github.com/umr-amap/cafriplotsR",
                    target = "_blank"
                  ), shiny::br(),
                  shiny::strong("Documentation:"), shiny::a(
                    "umr-amap.github.io/cafriplotsR",
                    href = "https://umr-amap.github.io/cafriplotsR",
                    target = "_blank"
                  )
                )
              )
            )
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

      # Clean up pool on session end
      session$onSessionEnded(function() {
        if (!is.null(.db_env$pool_main)) {
          tryCatch({
            pool::poolClose(.db_env$pool_main)
            .db_env$pool_main <- NULL
          }, error = function(e) {
            cli::cli_alert_warning("Failed to close connection pool: {e$message}")
          })
        }
      })
    }

    # Stop app and quit R when browser is closed
    session$onSessionEnded(function() {
      shiny::stopApp()
      q("no")
    })

    # Output for conditional panel (needs to be suspendable=FALSE)
    output$authenticated <- shiny::reactive({
      authenticated_reactive()
    })
    shiny::outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

    # Reactive values for data flow
    rv <- shiny::reactiveValues(
      metadata = NULL,
      individuals = NULL,
      modules_initialized = FALSE
    )

    # Only initialize modules after authentication (runs once)
    shiny::observe({
      shiny::req(authenticated_reactive() == TRUE)
      shiny::req(pool_reactive())
      shiny::req(!rv$modules_initialized)  # Only run once

      cli::cli_alert_info("Initializing app modules...")

      # Module 1: Filters
      filter_output <- mod_plot_filters_server("filters", pool = pool_reactive)

      # Execute metadata query when filters are applied
      shiny::observeEvent(filter_output$execute_trigger(), {
        trigger_val <- filter_output$execute_trigger()
        cli::cli_alert_info("Execute trigger fired! Value: {trigger_val}")

        # Require counter > 0 (button was actually clicked)
        if (trigger_val == 0) {
          cli::cli_alert_info("Skipping execution - trigger value is 0")
          return()
        }

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
        metadata = shiny::reactive(rv$metadata)
      )

      # Module 3: Extraction Configuration
      extraction_output <- mod_extraction_config_server(
        "extraction",
        selected_plots = selected_plots
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
              extract_individuals = TRUE,
              extract_traits = options$extract_traits,
              extract_individual_features = options$extract_individual_features,
              extract_subplot_features = options$extract_subplot_features,
              traits_to_genera = options$traits_to_genera,
              output_style = options$output_style,
              census_strategy = options$census_strategy,
              show_multiple_census = options$show_multiple_census,
              concatenate_stem = options$concatenate_stem,
              remove_ids = options$remove_ids,
              remove_obs_with_issue = options$remove_obs_with_issue,
              include_issue = options$include_issue,
              con = pool_reactive()
            )

            # Store results
            rv$individuals <- result

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

      # Module 4: Results Display
      mod_results_display_server(
        "results",
        results = shiny::reactive(rv$individuals)
      )

      # Mark modules as initialized
      rv$modules_initialized <- TRUE
      cli::cli_alert_success("All modules initialized successfully!")
    })  # Close the observe block for module initialization
  }

  # Return app object
  shiny::shinyApp(ui = ui, server = server)
}
