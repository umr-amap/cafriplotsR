#' Database Login Module - UI
#'
#' UI component for database authentication
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

        shiny::h3(
          shiny::icon("database"),
          " Database Connection",
          style = "text-align: center; margin-bottom: 20px;"
        ),

        shiny::p(
          "Connect to the CafriplotsR database to access forest plot data.",
          class = "text-muted",
          style = "text-align: center;"
        ),

        shiny::hr(),

        # Check for saved credentials
        shiny::uiOutput(ns("saved_credentials_message")),

        # Option to use saved credentials
        shiny::conditionalPanel(
          condition = sprintf("output['%s']", ns("has_saved_credentials")),
          shiny::checkboxInput(
            ns("use_saved"),
            "Use saved credentials from .Renviron",
            value = TRUE
          ),
          shiny::hr()
        ),

        # Manual credentials input (hidden when using saved credentials)
        shiny::conditionalPanel(
          condition = sprintf("!input['%s']", ns("use_saved")),

          shiny::h5(shiny::icon("server"), " Database Credentials"),

          shiny::textInput(
            ns("db_user"),
            "Username",
            placeholder = "Database username"
          ),

          shiny::passwordInput(
            ns("db_password"),
            "Password",
            placeholder = "Database password"
          ),

          shiny::hr(),

          shiny::p(
            class = "text-muted",
            style = "font-size: 0.9em;",
            shiny::icon("info-circle"),
            " Credentials will only be used for this session. ",
            "To save credentials permanently, use ",
            shiny::code("setup_db_credentials()"),
            " in R console."
          )
        ),

        # Status message
        shiny::uiOutput(ns("status_message")),

        # Connect button
        shiny::actionButton(
          ns("connect"),
          "Connect to Database",
          icon = shiny::icon("plug"),
          class = "btn-primary btn-lg btn-block",
          style = "margin-top: 10px;"
        ),

        # Hidden output for conditional panel
        shiny::textOutput(ns("has_saved_credentials"))
      )
    )
  )
}

#' Database Login Module - Server
#'
#' Server logic for database authentication
#'
#' @param id Module namespace ID
#'
#' @return A reactive list containing:
#'   - authenticated: Reactive logical indicating connection status
#'   - pool_main: Main database connection pool (NULL if not connected)
#'   - pool_taxa: Taxa database connection pool (NULL if not connected)
#'
#' @keywords internal
#' @export
mod_database_login_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- shiny::reactiveValues(
      authenticated = FALSE,
      pool_main = NULL,
      pool_taxa = NULL,
      error_message = NULL,
      has_saved = FALSE
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

    # Saved credentials message
    output$saved_credentials_message <- shiny::renderUI({
      if (rv$has_saved) {
        shiny::div(
          class = "alert alert-success",
          style = "font-size: 0.9em;",
          shiny::icon("check-circle"),
          shiny::strong(" Saved credentials detected"),
          shiny::br(),
          shiny::tags$small(
            "You can use your saved credentials or enter new ones manually."
          )
        )
      } else {
        shiny::div(
          class = "alert alert-info",
          style = "font-size: 0.9em;",
          shiny::icon("info-circle"),
          " No saved credentials found. Please enter your database credentials."
        )
      }
    })

    # Status message
    output$status_message <- shiny::renderUI({
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
          " Successfully connected to database!"
        )
      } else {
        NULL
      }
    })

    # Connect button handler
    shiny::observeEvent(input$connect, {
      # Clear previous error
      rv$error_message <- NULL

      # Determine which credentials to use
      if (rv$has_saved && input$use_saved) {
        # Use saved credentials
        db_user <- Sys.getenv("MYDB_USER")
        db_password <- Sys.getenv("MYDB_PASS")
        cli::cli_alert_info("Using saved credentials from .Renviron")
      } else {
        # Use manual input
        db_user <- input$db_user
        db_password <- input$db_password

        # Validate inputs
        if (nzchar(db_user) == FALSE || nzchar(db_password) == FALSE) {
          rv$error_message <- "Please enter username and password"
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
        shiny::setProgress(0.2, message = "Connecting to main database...")

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
          rv$error_message <- paste("Main database connection failed:", e$message)
          cli::cli_alert_danger("Main database connection failed: {e$message}")
          return()
        })

        # If main connection failed, stop here
        if (is.null(rv$pool_main)) {
          return()
        }

        # Try to create taxa pool
        shiny::setProgress(0.6, message = "Connecting to taxa database...")

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
          rv$error_message <- paste("Taxa database connection failed:", e$message)
          cli::cli_alert_danger("Taxa database connection failed: {e$message}")

          # Close main pool if taxa connection failed
          if (!is.null(rv$pool_main)) {
            pool::poolClose(rv$pool_main)
            rv$pool_main <- NULL
          }
          return()
        })

        # Mark as authenticated if both pools created
        shiny::setProgress(1, message = "Connection successful!")

        if (!is.null(rv$pool_main) && !is.null(rv$pool_taxa)) {
          rv$authenticated <- TRUE

          # Store pools in global environment for query functions
          .db_env$pool_main <- rv$pool_main
          .db_env$pool_taxa <- rv$pool_taxa

          # Cache credentials in memory for this session
          credentials$user_db <- db_user
          credentials$password <- db_password

          shiny::showNotification(
            "Successfully connected to databases!",
            type = "message",
            duration = 5
          )
        }

      }, message = "Connecting to database...")
    })

    # Note: Pool cleanup is handled by cleanup_connections() in the main app's
    # onSessionEnded callback. Removing duplicate cleanup here prevents
    # "Can't access reactive value outside of reactive consumer" errors
    # that occur when the session ends and reactive context is destroyed.

    # Return reactive values
    return(
      list(
        authenticated = shiny::reactive(rv$authenticated),
        pool_main = shiny::reactive(rv$pool_main),
        pool_taxa = shiny::reactive(rv$pool_taxa)
      )
    )
  })
}
