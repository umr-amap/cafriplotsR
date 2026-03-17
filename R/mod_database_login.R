#' Database Login Module - UI
#'
#' UI component for database authentication with language selection
#'
#' @param id Module namespace ID
#'
#' @return A shiny tagList
#' @keywords internal
#' @export
mod_database_login_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "container",
      style = "max-width: 500px; margin-top: 50px;",

      shiny::wellPanel(
        style = "background-color: #f8f9fa; padding: 30px;",

        # Language toggle (top-right)
        shiny::div(
          style = "text-align: right; margin-bottom: 10px;",
          shiny::radioButtons(
            ns("language"),
            label = NULL,
            choices = c("English" = "en", "Français" = "fr"),
            selected = "en",
            inline = TRUE
          )
        ),

        # All content rendered dynamically for i18n
        shiny::uiOutput(ns("login_header")),
        shiny::hr(),

        # Saved credentials message
        shiny::uiOutput(ns("saved_credentials_message")),

        # Option to use saved credentials (only shown when saved creds exist)
        shiny::conditionalPanel(
          condition = sprintf("output['%s'] == 'TRUE'", ns("has_saved_credentials")),
          shiny::uiOutput(ns("use_saved_checkbox")),
          shiny::hr()
        ),

        # Manual credentials input (shown when no saved creds or checkbox unchecked)
        shiny::conditionalPanel(
          condition = sprintf("output['%s'] != 'TRUE' || !input['%s']",
                              ns("has_saved_credentials"), ns("use_saved")),
          shiny::uiOutput(ns("credentials_form"))
        ),

        # Status message
        shiny::uiOutput(ns("status_message")),

        # Connect button
        shiny::uiOutput(ns("connect_button")),

        # Public access separator
        shiny::hr(style = "margin-top: 20px; margin-bottom: 15px;"),
        shiny::div(
          style = "text-align: center; color: #6c757d; font-size: 0.85em; margin-bottom: 10px;",
          shiny::uiOutput(ns("or_label"))
        ),

        # Public access button + notice
        shiny::uiOutput(ns("public_connect_button")),
        shiny::uiOutput(ns("public_access_notice")),

        # Hidden output for conditional panel
        shiny::textOutput(ns("has_saved_credentials"))
      )
    )
  )
}

#' Database Login Module - Server
#'
#' Server logic for database authentication with language selection
#'
#' @param id Module namespace ID
#'
#' @return A reactive list containing:
#'   - authenticated: Reactive logical indicating connection status
#'   - pool_main: Main database connection pool (NULL if not connected)
#'   - pool_taxa: Taxa database connection pool (NULL if not connected)
#'   - language: Reactive string returning selected language ("en" or "fr")
#'
#' @keywords internal
#' @export
mod_database_login_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize translator for login module
    i18n_translator <- tryCatch({
      init_translator()
    }, error = function(e) {
      cli::cli_alert_warning("Could not load translations: {e$message}")
      NULL
    })

    # Helper: translate text
    t <- function(key) {
      if (is.null(i18n_translator)) return(key)
      i18n_translator$t(key)
    }

    # Update translator language when toggle changes
    shiny::observe({
      shiny::req(input$language)
      if (!is.null(i18n_translator)) {
        i18n_translator$set_translation_language(input$language)
      }
    })

    # Public credentials (read-only user — intentionally embedded)
    public_user     <- "CafriP_public"
    public_password <- "CafriPublic01"

    # Reactive values
    rv <- shiny::reactiveValues(
      authenticated = FALSE,
      pool_main = NULL,
      pool_taxa = NULL,
      error_message = NULL,
      has_saved = FALSE,
      is_public = FALSE
    )

    # Check for saved credentials on startup
    shiny::observe({
      saved_user <- Sys.getenv("MYDB_USER")
      saved_pass <- Sys.getenv("MYDB_PASS")
      rv$has_saved <- (saved_user != "" && saved_pass != "")
    })

    # Output for conditional panel
    output$has_saved_credentials <- shiny::renderText({
      as.character(rv$has_saved)
    })
    shiny::outputOptions(output, "has_saved_credentials", suspendWhenHidden = FALSE)

    # -- Rendered UI elements (reactive to language changes) --

    output$login_header <- shiny::renderUI({
      input$language  # trigger re-render on language change
      shiny::tagList(
        shiny::h3(
          shiny::icon("database"),
          paste0(" ", t("Database Connection")),
          style = "text-align: center; margin-bottom: 20px;"
        ),
        shiny::p(
          t("Connect to the CafriplotsR database to access forest plot data."),
          class = "text-muted",
          style = "text-align: center;"
        )
      )
    })

    output$use_saved_checkbox <- shiny::renderUI({
      input$language
      shiny::checkboxInput(
        ns("use_saved"),
        t("Use saved credentials from .Renviron"),
        value = TRUE
      )
    })

    output$saved_credentials_message <- shiny::renderUI({
      input$language
      if (rv$has_saved) {
        shiny::div(
          class = "alert alert-success",
          style = "font-size: 0.9em;",
          shiny::icon("check-circle"),
          shiny::strong(paste0(" ", t("Saved credentials detected"))),
          shiny::br(),
          shiny::tags$small(
            t("You can use your saved credentials or enter new ones manually.")
          )
        )
      } else {
        shiny::div(
          class = "alert alert-info",
          style = "font-size: 0.9em;",
          shiny::icon("info-circle"),
          paste0(" ", t("No saved credentials found. Please enter your database credentials."))
        )
      }
    })

    output$credentials_form <- shiny::renderUI({
      input$language
      shiny::tagList(
        shiny::h5(shiny::icon("server"), paste0(" ", t("Database Credentials"))),
        shiny::textInput(
          ns("db_user"),
          t("Username"),
          placeholder = t("Database username")
        ),
        shiny::passwordInput(
          ns("db_password"),
          t("Password"),
          placeholder = t("Database password")
        ),
        shiny::hr(),
        shiny::p(
          class = "text-muted",
          style = "font-size: 0.9em;",
          shiny::icon("info-circle"),
          paste0(" ", t("Credentials will only be used for this session. To save credentials permanently, use")),
          " ",
          shiny::code("setup_db_credentials()"),
          " ",
          t("in R console.")
        )
      )
    })

    output$connect_button <- shiny::renderUI({
      input$language
      shiny::actionButton(
        ns("connect"),
        t("Connect to Database"),
        icon = shiny::icon("plug"),
        class = "btn-primary btn-lg btn-block",
        style = "margin-top: 10px;"
      )
    })

    # Status message
    output$status_message <- shiny::renderUI({
      input$language
      if (!is.null(rv$error_message)) {
        shiny::div(
          class = "alert alert-danger",
          shiny::icon("exclamation-triangle"),
          " ",
          rv$error_message
        )
      } else if (rv$authenticated) {
        shiny::div(
          class = "alert alert-success",
          shiny::icon("check-circle"),
          paste0(" ", t("Successfully connected to database!"))
        )
      } else {
        NULL
      }
    })

    output$or_label <- shiny::renderUI({
      input$language
      shiny::tagList(
        shiny::tags$span(
          style = "background: #f8f9fa; padding: 0 10px;",
          t("or")
        )
      )
    })

    output$public_connect_button <- shiny::renderUI({
      input$language
      shiny::actionButton(
        ns("connect_public"),
        shiny::tagList(
          shiny::icon("globe"),
          paste0(" ", t("Connect as public user"))
        ),
        class = "btn-outline-secondary btn-block",
        style = "margin-bottom: 8px;"
      )
    })

    output$public_access_notice <- shiny::renderUI({
      input$language
      shiny::div(
        class = "alert alert-warning",
        style = "font-size: 0.85em; margin-top: 5px; margin-bottom: 0; padding: 8px 12px;",
        shiny::icon("exclamation-triangle"),
        " ",
        shiny::strong(t("Read-only access:")),
        " ",
        t("Public authentication gives access to taxonomy and traits only. Adding or modifying data is not available.")
      )
    })

    # Connect button handler
    shiny::observeEvent(input$connect, {
      # Clear previous error
      rv$error_message <- NULL

      # Determine which credentials to use
      if (rv$has_saved && isTRUE(input$use_saved)) {
        # Use saved credentials
        db_user <- Sys.getenv("MYDB_USER")
        db_password <- Sys.getenv("MYDB_PASS")
        cli::cli_alert_info("Using saved credentials from .Renviron")
      } else {
        # Use manual input
        db_user <- input$db_user
        db_password <- input$db_password

        # Validate inputs
        if (is.null(db_user) || is.null(db_password) ||
            nzchar(db_user) == FALSE || nzchar(db_password) == FALSE) {
          rv$error_message <- t("Please enter username and password")
          return()
        }
      }

      # Load database configuration
      tryCatch({
        create_db_config()
      }, error = function(e) {
        cli::cli_alert_warning("Could not load db config: {e$message}")
      })

      # Get database connection parameters
      db_host <- if (exists("db_host", envir = .GlobalEnv)) {
        get("db_host", envir = .GlobalEnv)
      } else {
        "dg474899-001.dbaas.ovh.net"
      }

      db_port <- if (exists("db_port", envir = .GlobalEnv)) {
        get("db_port", envir = .GlobalEnv)
      } else {
        35699
      }

      db_name <- if (exists("db_name", envir = .GlobalEnv)) {
        get("db_name", envir = .GlobalEnv)
      } else {
        "plots_transects"
      }

      db_name_taxa <- if (exists("db_name_taxa", envir = .GlobalEnv)) {
        get("db_name_taxa", envir = .GlobalEnv)
      } else {
        "rainbio"
      }

      # Show progress
      shiny::withProgress({

        # Try to create main pool
        shiny::setProgress(0.2, message = t("Connecting to main database..."))

        tryCatch({
          pool_main <- pool::dbPool(
            drv = RPostgres::Postgres(),
            host = db_host,
            port = db_port,
            dbname = db_name,
            user = db_user,
            password = db_password
          )

          # Test connection
          test_result <- DBI::dbGetQuery(pool_main, "SELECT 1 AS test")

          if (nrow(test_result) == 1) {
            rv$pool_main <- pool_main
            cli::cli_alert_success("Connected to main database ({db_name})")
          } else {
            stop("Connection test failed")
          }

        }, error = function(e) {
          rv$error_message <- paste(t("Main database connection failed:"), e$message)
          cli::cli_alert_danger("Main database connection failed: {e$message}")
          return()
        })

        # If main connection failed, stop here
        if (is.null(rv$pool_main)) {
          return()
        }

        # Try to create taxa pool
        shiny::setProgress(0.6, message = t("Connecting to taxa database..."))

        tryCatch({
          pool_taxa <- pool::dbPool(
            drv = RPostgres::Postgres(),
            host = db_host,
            port = db_port,
            dbname = db_name_taxa,
            user = db_user,
            password = db_password
          )

          # Test connection
          test_result <- DBI::dbGetQuery(pool_taxa, "SELECT 1 AS test")

          if (nrow(test_result) == 1) {
            rv$pool_taxa <- pool_taxa
            cli::cli_alert_success("Connected to taxa database ({db_name_taxa})")
          } else {
            stop("Connection test failed")
          }

        }, error = function(e) {
          rv$error_message <- paste(t("Taxa database connection failed:"), e$message)
          cli::cli_alert_danger("Taxa database connection failed: {e$message}")

          # Close main pool if taxa connection failed
          if (!is.null(rv$pool_main)) {
            pool::poolClose(rv$pool_main)
            rv$pool_main <- NULL
          }
          return()
        })

        # Mark as authenticated if both pools created
        shiny::setProgress(1, message = t("Connection successful!"))

        if (!is.null(rv$pool_main) && !is.null(rv$pool_taxa)) {
          rv$authenticated <- TRUE

          # Store pools in global environment for query functions
          .db_env$pool_main <- rv$pool_main
          .db_env$pool_taxa <- rv$pool_taxa

          # Cache credentials in memory for this session
          credentials$user_db <- db_user
          credentials$password <- db_password

          shiny::showNotification(
            t("Successfully connected to databases!"),
            type = "message",
            duration = 5
          )
        }

      }, message = t("Connecting to database..."))
    })

    # Public connect button handler
    shiny::observeEvent(input$connect_public, {
      rv$error_message <- NULL
      rv$is_public <- FALSE

      tryCatch({
        create_db_config()
      }, error = function(e) {
        cli::cli_alert_warning("Could not load db config: {e$message}")
      })

      db_host <- if (exists("db_host", envir = .GlobalEnv)) {
        get("db_host", envir = .GlobalEnv)
      } else {
        "dg474899-001.dbaas.ovh.net"
      }

      db_port <- if (exists("db_port", envir = .GlobalEnv)) {
        get("db_port", envir = .GlobalEnv)
      } else {
        35699
      }

      shiny::withProgress({

        shiny::setProgress(0.3, message = t("Connecting as public user..."))

        tryCatch({
          pool_main <- pool::dbPool(
            drv = RPostgres::Postgres(),
            host = db_host,
            port = db_port,
            dbname = "plots_transects",
            user = public_user,
            password = public_password
          )
          DBI::dbGetQuery(pool_main, "SELECT 1 AS test")
          rv$pool_main <- pool_main
        }, error = function(e) {
          rv$error_message <- paste(t("Public connection failed (main database):"), e$message)
          return()
        })

        if (is.null(rv$pool_main)) return()

        shiny::setProgress(0.7, message = t("Connecting to taxa database..."))

        tryCatch({
          pool_taxa <- pool::dbPool(
            drv = RPostgres::Postgres(),
            host = db_host,
            port = db_port,
            dbname = "rainbio",
            user = public_user,
            password = public_password
          )
          DBI::dbGetQuery(pool_taxa, "SELECT 1 AS test")
          rv$pool_taxa <- pool_taxa
        }, error = function(e) {
          rv$error_message <- paste(t("Public connection failed (taxa database):"), e$message)
          if (!is.null(rv$pool_main)) {
            pool::poolClose(rv$pool_main)
            rv$pool_main <- NULL
          }
          return()
        })

        if (!is.null(rv$pool_main) && !is.null(rv$pool_taxa)) {
          rv$authenticated <- TRUE
          rv$is_public     <- TRUE
          .db_env$pool_main <- rv$pool_main
          .db_env$pool_taxa <- rv$pool_taxa
          shiny::setProgress(1, message = t("Connected!"))
          shiny::showNotification(
            paste0(t("Connected as public user — read-only access")),
            type = "message",
            duration = 5
          )
        }

      }, message = t("Connecting as public user..."))
    })

    # Note: Pool cleanup is handled by cleanup_connections() in the main app's
    # onSessionEnded callback. Removing duplicate cleanup here prevents
    # "Can't access reactive value outside of reactive consumer" errors
    # that occur when the session ends and reactive context is destroyed.

    # Return reactive values (including language for parent app)
    return(
      list(
        authenticated = shiny::reactive(rv$authenticated),
        pool_main     = shiny::reactive(rv$pool_main),
        pool_taxa     = shiny::reactive(rv$pool_taxa),
        language      = shiny::reactive(input$language %||% "en"),
        is_public     = shiny::reactive(rv$is_public)
      )
    )
  })
}
