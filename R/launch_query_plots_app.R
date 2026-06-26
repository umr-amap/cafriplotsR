#' Launch Query Plots Interactive App
#'
#' Launches an interactive Shiny application for querying forest plot data.
#' This app provides a user-friendly interface to the \code{\link{query_plots}}
#' function with two main stages:
#'
#' \enumerate{
#'   \item \strong{Filter & Discover}: Apply filters to find plots of interest,
#'         view them on an interactive map with metadata
#'   \item \strong{Select & Extract}: Choose specific plots and extract detailed
#'         individual tree data with customizable output styles and options
#' }
#'
#' @param pool_main Optional database connection pool for the main database.
#'   If not provided, the app will create one automatically using
#'   \code{\link{create_pool_main}}.
#' @param language Character string for UI language. Options:
#'   \itemize{
#'     \item "fr" (French, default)
#'     \item "en" (English)
#'   }
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'   (e.g., \code{launch.browser = TRUE}, \code{port = 3838})
#'
#' @return NULL (launches the Shiny app)
#'
#' @details
#' The app provides an intuitive interface for:
#'
#' \strong{Filtering}: Use multiple criteria to find plots:
#' \itemize{
#'   \item Country, plot name, locality, method
#'   \item Individual tags, taxon IDs
#'   \item Advanced filters (plot IDs, specimen IDs)
#' }
#'
#' \strong{Visualization}: Explore plot locations:
#' \itemize{
#'   \item Interactive map with multiple basemaps
#'   \item Clickable markers with plot information
#'   \item Synchronized table view with selection
#' }
#'
#' \strong{Extraction}: Configure detailed data extraction:
#' \itemize{
#'   \item Choose output style (auto, minimal, standard, permanent_plot, etc.)
#'   \item Set census strategy (last, first, mean)
#'   \item Toggle traits, features, and additional data
#'   \item Configure data organization options
#' }
#'
#' \strong{Export}: Download results in multiple formats:
#' \itemize{
#'   \item Excel (.xlsx) - multi-sheet workbook
#'   \item CSV (zipped folder)
#'   \item R Object (.rds)
#'   \item Shapefile (.zip) - if spatial data included
#' }
#'
#' @section Database Connection:
#' The app requires access to the CafriplotsR database. If you don't provide
#' a connection pool, the app will prompt for credentials or use stored
#' credentials from \code{\link{setup_db_credentials}}.
#'
#' @section Output Styles:
#' The app supports all output styles from \code{\link{query_plots}}:
#' \describe{
#'   \item{auto}{Auto-detect from plot method (default)}
#'   \item{minimal}{Essential columns only}
#'   \item{standard}{Common columns for general analysis}
#'   \item{permanent_plot}{Structured format for single census monitoring}
#'   \item{permanent_plot_multi_census}{Time-series format preserving all census columns}
#'   \item{transect}{Simplified format for transect surveys}
#'   \item{full}{Complete dataset with all columns}
#' }
#'
#' @examples
#' \dontrun{
#' # Launch app with default settings
#' launch_query_plots_app()
#'
#' # Launch app in browser on specific port
#' launch_query_plots_app(launch.browser = TRUE, port = 8080)
#'
#' # Use existing connection pool
#' pool <- create_pool_main()
#' launch_query_plots_app(pool_main = pool)
#' }
#'
#' @seealso
#' \code{\link{query_plots}} for the underlying query function
#' \code{\link{create_pool_main}} for database connection pooling
#' \code{\link{launch_taxonomic_match_app}} for the taxonomic matching app
#'
#' @export
launch_query_plots_app <- function(pool_main = NULL, language = c("fr", "en"), ...) {

  # Validate language
  language <- match.arg(language)

  # Check required packages
  required_pkgs <- c("shiny", "DT", "sf", "bslib", "shinyjs", "writexl", "zip")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required but not installed: ",
      paste(missing_pkgs, collapse = ", "),
      "\nInstall them with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # Welcome message
  cli::cli_h1("CafriplotsR - Query Plots Interactive App")
  cli::cli_alert_info("Starting application...")

  # Test database connection if pool not provided
  if (is.null(pool_main)) {
    cli::cli_alert_info("No connection pool provided, will create one on startup")
    cli::cli_alert_warning("Make sure you have database credentials configured")
    cli::cli_alert_info("Use {.fn setup_db_credentials} to store credentials if needed")
  } else {
    # Verify pool is valid
    if (!inherits(pool_main, "Pool")) {
      stop("pool_main must be a database connection pool created with create_pool_main()", call. = FALSE)
    }

    # Test connection
    tryCatch({
      test_query <- DBI::dbGetQuery(pool_main, "SELECT 1 AS test")
      cli::cli_alert_success("Database connection verified")
    }, error = function(e) {
      cli::cli_alert_danger("Database connection test failed: {e$message}")
      stop("Cannot connect to database. Please check your connection pool.", call. = FALSE)
    })
  }

  # Launch app
  cli::cli_alert_success("Launching app...")
  cli::cli_text("")
  cli::cli_alert_info("To stop the app, press {.kbd Ctrl+C} or close the browser window")
  cli::cli_text("")

  app <- shiny_app_query_plots(pool_main = pool_main, language = language)

  shiny::runApp(app, ...)
}
