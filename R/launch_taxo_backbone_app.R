#' Launch Taxonomic Backbone Management App
#'
#' Launches an interactive Shiny application for managing the taxonomic backbone database.
#' This app provides tools for adding new taxa, updating existing records, and managing
#' synonymy relationships.
#'
#' @param pool_taxa Optional database connection pool for the taxa database.
#'   If not provided, the app will prompt for login credentials.
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
#' \strong{Browse & Search}:
#' \itemize{
#'   \item Search by genus, species, family, order, or taxon ID
#'   \item Filter by synonymy status (all/accepted/synonyms)
#'   \item View database statistics
#'   \item Select taxa for editing
#' }
#'
#' \strong{Add New Taxa}:
#' \itemize{
#'   \item Search Tropicos for taxonomic information
#'   \item Manual entry for all taxonomic ranks
#'   \item Duplicate checking and validation
#'   \item Set synonymy relationships
#'   \item Growth form selection
#' }
#'
#' \strong{Update Existing Taxa}:
#' \itemize{
#'   \item Edit all taxonomic fields
#'   \item View modification history
#'   \item Automatic backup creation
#'   \item Change validation
#' }
#'
#' \strong{Synonymy Management}:
#' \itemize{
#'   \item Set taxa as synonyms
#'   \item Cancel existing synonymy
#'   \item View synonym networks
#'   \item Handle synonym cascades
#' }
#'
#' \strong{Hierarchy Viewer}:
#' \itemize{
#'   \item Interactive taxonomic tree
#'   \item Statistics by rank
#'   \item Export capabilities
#' }
#'
#' @section Database Permissions:
#' The app checks user permissions on startup:
#' \describe{
#'   \item{Read Access}{All authenticated users can browse and search}
#'   \item{Write Access}{Only users with INSERT privileges on table_taxa can modify data}
#' }
#'
#' Users without write permissions will see a "Read-Only Mode" badge and
#' modification features will be disabled.
#'
#' @section Core Functions:
#' The app wraps existing package functions:
#' \itemize{
#'   \item \code{\link{add_entry_taxa}} - Add new taxonomic entries
#'   \item \code{\link{update_dico_name}} - Update existing records and manage synonymy
#'   \item \code{\link{query_taxa}} - Search taxonomic database
#' }
#'
#' @section Security:
#' All modifications are:
#' \itemize{
#'   \item Logged in \code{followup_updates_diconames} table
#'   \item Backed up before changes
#'   \item Validated for taxonomic consistency
#'   \item Subject to database permissions
#' }
#'
#' @examples
#' \dontrun{
#' # Launch app with default settings
#' launch_taxo_backbone_app()
#'
#' # Launch app in English
#' launch_taxo_backbone_app(language = "en")
#'
#' # Launch app in browser on specific port
#' launch_taxo_backbone_app(launch.browser = TRUE, port = 8080)
#'
#' # Use existing connection pool
#' pool <- call.mydb.taxa()
#' launch_taxo_backbone_app(pool_taxa = pool)
#' }
#'
#' @seealso
#' \code{\link{add_entry_taxa}} for adding new taxa programmatically
#' \code{\link{update_dico_name}} for updating taxa programmatically
#' \code{\link{query_taxa}} for searching the taxonomic database
#' \code{\link{launch_query_plots_app}} for the plot query app
#' \code{\link{launch_taxonomic_match_app}} for the taxonomic matching app
#'
#' @export
launch_taxo_backbone_app <- function(pool_taxa = NULL, language = c("fr", "en"), ...) {

  # Validate language
  language <- match.arg(language)

  # Check required packages
  required_pkgs <- c("shiny", "DT", "bslib", "shinyjs", "shiny.i18n", "dplyr", "pool")
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
  cli::cli_h1("CafriplotsR - Taxonomic Backbone Management")
  cli::cli_alert_info("Starting application...")

  # Test database connection if pool not provided
  if (is.null(pool_taxa)) {
    cli::cli_alert_info("No connection pool provided, will prompt for login on startup")
  } else {
    cli::cli_alert_success("Using provided connection pool")

    # Verify it's the taxa database
    tryCatch({
      actual_con <- if (inherits(pool_taxa, "Pool")) {
        pool::poolCheckout(pool_taxa)
      } else {
        pool_taxa
      }

      on.exit({
        if (inherits(pool_taxa, "Pool") && !is.null(actual_con)) {
          pool::poolReturn(actual_con)
        }
      }, add = TRUE)

      # Check if table_taxa exists
      tables <- DBI::dbListTables(actual_con)
      if (!"table_taxa" %in% tables) {
        cli::cli_alert_warning("Warning: table_taxa not found in provided connection")
        cli::cli_alert_warning("Make sure you're connected to the TAXA database, not the main database")
      } else {
        cli::cli_alert_success("Connected to taxa database")
      }

    }, error = function(e) {
      cli::cli_alert_warning("Could not verify database connection: {e$message}")
    })
  }

  # Launch message
  cli::cli_alert_info("Launching app in {language} mode...")
  cli::cli_rule()
  cli::cli_alert_info("Press Ctrl+C or close browser window to stop the app")
  cli::cli_rule()

  # Create and run the app
  app <- shiny_app_taxo_backbone(pool_taxa = pool_taxa, language = language)

  # Run the app
  shiny::runApp(app, ...)
}
