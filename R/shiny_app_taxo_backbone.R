#' Taxonomic Backbone Management Shiny App
#'
#' Interactive Shiny application for managing the taxonomic backbone database.
#' Provides tools for adding new taxa, updating existing records, and managing
#' synonymy relationships.
#'
#' @param pool_taxa Optional taxa database connection pool (will prompt for login if NULL)
#' @param language Character, initial language ("en" or "fr"), default: "fr"
#'
#' @return A Shiny app object
#' @keywords internal
#' @export
shiny_app_taxo_backbone <- function(pool_taxa = NULL, language = "fr") {

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
          .read-only-badge {
            background-color: #ffc107;
            color: #000;
            padding: 5px 10px;
            border-radius: 3px;
            font-size: 0.9em;
            margin-left: 10px;
          }
          .write-enabled-badge {
            background-color: #28a745;
            color: #fff;
            padding: 5px 10px;
            border-radius: 3px;
            font-size: 0.9em;
            margin-left: 10px;
          }
          .synonym-indicator {
            color: #6c757d;
            font-style: italic;
          }
          .modified-field {
            background-color: #fff3cd;
            border-left: 3px solid #ffc107;
            padding-left: 10px;
          }
        "))
      ),

      # Login panel (shown if pool_taxa not provided)
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
          windowTitle = "CafriplotsR - Taxonomic Backbone Management",
          theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

          # Page 1: Browse & Search
          shiny::tabPanel(
            shiny::textOutput("tab_search", inline = TRUE),
            value = "page_search",
            icon = shiny::icon("search"),

            shiny::fluidPage(
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::h2(shiny::textOutput("page_search_title", inline = TRUE)),
                  shiny::uiOutput("permissions_badge"),
                  shiny::p(
                    class = "lead",
                    shiny::textOutput("page_search_subtitle", inline = TRUE)
                  )
                )
              ),

              shiny::hr(),

              # Search module
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::div(
                    class = "module-section",
                    mod_taxa_search_ui("search")
                  )
                )
              )
            )
          ),

          # Page 2: Add New Taxon
          shiny::tabPanel(
            shiny::textOutput("tab_add", inline = TRUE),
            value = "page_add",
            icon = shiny::icon("plus-circle"),

            shiny::fluidPage(
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::h2(shiny::textOutput("page_add_title", inline = TRUE)),
                  shiny::p(
                    class = "lead",
                    shiny::textOutput("page_add_subtitle", inline = TRUE)
                  )
                )
              ),

              shiny::hr(),

              # Add taxa module
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::div(
                    class = "module-section",
                    mod_taxa_add_ui("add")
                  )
                )
              )
            )
          ),

          # Page 3: Update Taxon
          shiny::tabPanel(
            shiny::textOutput("tab_update", inline = TRUE),
            value = "page_update",
            icon = shiny::icon("edit"),

            shiny::fluidPage(
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::h2(shiny::textOutput("page_update_title", inline = TRUE)),
                  shiny::p(
                    class = "lead",
                    shiny::textOutput("page_update_subtitle", inline = TRUE)
                  )
                )
              ),

              shiny::hr(),

              # Update taxa module
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::div(
                    class = "module-section",
                    mod_taxa_update_ui("update")
                  )
                )
              )
            )
          ),

          # Page 4: Synonymy Management
          shiny::tabPanel(
            shiny::textOutput("tab_synonymy", inline = TRUE),
            value = "page_synonymy",
            icon = shiny::icon("link"),

            shiny::fluidPage(
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::h2(shiny::textOutput("page_synonymy_title", inline = TRUE)),
                  shiny::p(
                    class = "lead",
                    shiny::textOutput("page_synonymy_subtitle", inline = TRUE)
                  )
                )
              ),

              shiny::hr(),

              # Synonymy module
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::div(
                    class = "module-section",
                    mod_taxa_synonymy_ui("synonymy")
                  )
                )
              )
            )
          ),

          # Page 5: Hierarchy View
          shiny::tabPanel(
            shiny::textOutput("tab_hierarchy", inline = TRUE),
            value = "page_hierarchy",
            icon = shiny::icon("sitemap"),

            shiny::fluidPage(
              shiny::br(),
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::h2(shiny::textOutput("page_hierarchy_title", inline = TRUE)),
                  shiny::p(
                    class = "lead",
                    shiny::textOutput("page_hierarchy_subtitle", inline = TRUE)
                  )
                )
              ),

              shiny::hr(),

              # Tree view module
              shiny::fluidRow(
                shiny::column(
                  12,
                  shiny::div(
                    class = "module-section",
                    mod_taxa_tree_view_ui("tree_view")
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
    if (is.null(pool_taxa)) {
      # Use login module for authentication
      login_output <- mod_database_login_server("login")

      pool_reactive <- login_output$pool_taxa  # NOTE: taxa DB, not main DB!
      authenticated_reactive <- login_output$authenticated
    } else {
      # Pool provided, mark as authenticated
      pool_reactive <- shiny::reactive(pool_taxa)
      authenticated_reactive <- shiny::reactive(TRUE)

      # Store in global env
      .db_env$pool_taxa <- pool_taxa
      # Note: Pool cleanup is handled by cleanup_connections() below
    }

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
      if (length(selected) > 0 && selected %in% translator$get_languages()) {
        translator$set_translation_language(selected)
      }
      translator
    })

    # Check write permissions on taxa database
    has_write_permission <- shiny::reactive({
      shiny::req(pool_reactive())

      tryCatch({
        # Try to get current user
        actual_con <- if (inherits(pool_reactive(), "Pool")) {
          pool::poolCheckout(pool_reactive())
        } else {
          pool_reactive()
        }

        on.exit({
          if (inherits(pool_reactive(), "Pool") && !is.null(actual_con)) {
            pool::poolReturn(actual_con)
          }
        }, add = TRUE)

        # Check if user has INSERT privilege on table_taxa
        check_query <- "
          SELECT has_table_privilege(current_user, 'table_taxa', 'INSERT') AS can_write
        "
        result <- DBI::dbGetQuery(actual_con, check_query)

        can_write <- result$can_write[1]

        if (!can_write) {
          cli::cli_alert_warning("User has READ-ONLY access to taxa database")
        } else {
          cli::cli_alert_success("User has WRITE access to taxa database")
        }

        return(can_write)

      }, error = function(e) {
        cli::cli_alert_warning("Could not check write permissions: {e$message}")
        # Assume read-only if check fails
        return(FALSE)
      })
    })

    # Permissions badge
    output$permissions_badge <- shiny::renderUI({
      shiny::req(authenticated_reactive() == TRUE)

      if (has_write_permission()) {
        shiny::span(
          class = "write-enabled-badge",
          shiny::icon("check-circle"),
          " ",
          i18n()$t("Write Access Enabled")
        )
      } else {
        shiny::span(
          class = "read-only-badge",
          shiny::icon("eye"),
          " ",
          i18n()$t("Read-Only Mode")
        )
      }
    })

    # App title
    output$app_title <- shiny::renderText({
      i18n()$t("Taxonomic Backbone Management")
    })

    # Tab labels
    output$tab_search <- shiny::renderText({
      i18n()$t("Browse & Search")
    })

    output$tab_add <- shiny::renderText({
      i18n()$t("Add New Taxon")
    })

    output$tab_update <- shiny::renderText({
      i18n()$t("Update Taxon")
    })

    output$tab_synonymy <- shiny::renderText({
      i18n()$t("Synonymy Management")
    })

    output$tab_hierarchy <- shiny::renderText({
      i18n()$t("Hierarchy View")
    })

    output$tab_about <- shiny::renderText({
      i18n()$t("About")
    })

    # Page titles and subtitles
    output$page_search_title <- shiny::renderText({
      i18n()$t("Browse & Search Taxonomy")
    })

    output$page_search_subtitle <- shiny::renderText({
      i18n()$t("Search and explore the taxonomic backbone database")
    })

    output$page_add_title <- shiny::renderText({
      i18n()$t("Add New Taxonomic Entry")
    })

    output$page_add_subtitle <- shiny::renderText({
      i18n()$t("Create new taxonomic names with optional Tropicos integration")
    })

    output$page_update_title <- shiny::renderText({
      i18n()$t("Update Existing Taxon")
    })

    output$page_update_subtitle <- shiny::renderText({
      i18n()$t("Modify taxonomic information for existing records")
    })

    output$page_synonymy_title <- shiny::renderText({
      i18n()$t("Manage Taxonomic Synonymy")
    })

    output$page_synonymy_subtitle <- shiny::renderText({
      i18n()$t("Set or cancel synonym relationships between taxa")
    })

    output$page_hierarchy_title <- shiny::renderText({
      i18n()$t("Taxonomic Hierarchy View")
    })

    output$page_hierarchy_subtitle <- shiny::renderText({
      i18n()$t("Visualize the hierarchical structure of selected taxa")
    })

    # About page content
    output$about_content <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          12,
          shiny::h2(i18n()$t("About Taxonomic Backbone Management")),
          shiny::hr(),
          shiny::h4(i18n()$t("Overview")),
          shiny::p(
            i18n()$t("This tool provides a user-friendly interface for managing the taxonomic backbone database."),
            i18n()$t("It allows authorized users to add new taxa, update existing records, and manage synonymy relationships.")
          ),
          shiny::hr(),
          shiny::h4(i18n()$t("Features")),
          shiny::tags$ul(
            shiny::tags$li(shiny::icon("search"), " ", i18n()$t("Search and browse taxonomic hierarchy")),
            shiny::tags$li(shiny::icon("plus-circle"), " ", i18n()$t("Add new taxonomic entries with Tropicos integration")),
            shiny::tags$li(shiny::icon("edit"), " ", i18n()$t("Update existing taxonomic information")),
            shiny::tags$li(shiny::icon("link"), " ", i18n()$t("Manage synonym relationships")),
            shiny::tags$li(shiny::icon("shield-alt"), " ", i18n()$t("Permission-based access control"))
          ),
          shiny::hr(),
          shiny::h4(i18n()$t("Database Permissions")),
          shiny::p(
            shiny::strong(i18n()$t("Read Access:")), " ", i18n()$t("All authenticated users can browse and search"), shiny::br(),
            shiny::strong(i18n()$t("Write Access:")), " ", i18n()$t("Only users with INSERT privileges can modify data"), shiny::br(),
            shiny::strong(i18n()$t("Backup:")), " ", i18n()$t("All modifications are logged for audit trail")
          ),
          shiny::hr(),
          shiny::h4(i18n()$t("Package Information")),
          shiny::p(
            shiny::strong(i18n()$t("Version:")), " 1.7.2", shiny::br(),
            shiny::strong(i18n()$t("Authors:")), " Gilles Dauby, Hugo Leblanc", shiny::br(),
            shiny::strong(i18n()$t("Contact:")), " gilles.dauby@ird.fr"
          )
        )
      )
    })

    # Reactive values for data flow
    rv <- shiny::reactiveValues(
      selected_taxon_id = NULL,
      selected_taxon_data = NULL,
      modules_initialized = FALSE
    )

    # Only initialize modules after authentication (runs once)
    shiny::observe({
      shiny::req(authenticated_reactive() == TRUE)
      shiny::req(pool_reactive())
      shiny::req(!rv$modules_initialized)  # Only run once

      cli::cli_alert_info("Initializing app modules...")

      # Module 1: Taxa Search & Browser
      selected_taxon <- mod_taxa_search_server(
        "search",
        pool = pool_reactive,
        i18n = i18n
      )

      # Update reactive values when taxon is selected
      shiny::observeEvent(selected_taxon(), {
        if (!is.null(selected_taxon()) && nrow(selected_taxon()) > 0) {
          rv$selected_taxon_id <- selected_taxon()$idtax_n[1]
          rv$selected_taxon_data <- selected_taxon()[1, ]
          cli::cli_alert_info("Taxon selected: {rv$selected_taxon_id}")
        }
      })

      # Module 2: Add New Taxa
      mod_taxa_add_server(
        "add",
        pool = pool_reactive,
        has_write_permission = has_write_permission,
        i18n = i18n
      )

      # Module 3: Update Existing Taxa
      mod_taxa_update_server(
        "update",
        pool = pool_reactive,
        selected_taxon = shiny::reactive(rv$selected_taxon_data),
        has_write_permission = has_write_permission,
        i18n = i18n
      )

      # Module 4: Synonymy Management
      mod_taxa_synonymy_server(
        "synonymy",
        pool = pool_reactive,
        selected_taxon = shiny::reactive(rv$selected_taxon_data),
        has_write_permission = has_write_permission,
        i18n = i18n
      )

      # Module 5: Tree View (Hierarchy)
      mod_taxa_tree_view_server(
        "tree_view",
        pool = pool_reactive,
        selected_taxon = shiny::reactive(rv$selected_taxon_data),
        i18n = i18n
      )

      # Mark modules as initialized
      rv$modules_initialized <- TRUE
      cli::cli_alert_success("All modules initialized successfully!")
    })  # Close the observe block for module initialization
  }

  # Return app object
  shiny::shinyApp(ui = ui, server = server)
}
