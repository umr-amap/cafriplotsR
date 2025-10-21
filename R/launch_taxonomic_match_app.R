# Launch Function for Taxonomic Matching Shiny App

#' Launch Taxonomic Name Standardization App
#'
#' Launch an interactive Shiny application for standardizing taxonomic names
#' against the Central African plant taxonomic backbone. The app provides
#' automatic matching with intelligent fuzzy search, manual review capabilities,
#' and export functionality.
#'
#' @param data Optional data.frame containing taxonomic names to standardize.
#'   If NULL, user will upload an Excel file through the app interface.
#' @param name_column Optional character string specifying the column name
#'   containing taxonomic names. If NULL, user will select interactively.
#' @param language Character, initial interface language. Options:
#'   - "en" (English, default)
#'   - "fr" (French)
#' @param min_similarity Numeric between 0 and 1, minimum similarity threshold
#'   for fuzzy matching. Lower values are more permissive. Default: 0.3
#' @param max_suggestions Integer, maximum number of fuzzy match suggestions
#'   to show per unmatched name. Default: 10
#' @param mode Character, review mode for unmatched names. Options:
#'   - "interactive" (default): Review names one-by-one
#'   - "batch": Review all names in table view (not yet implemented)
#' @param launch.browser Logical, whether to launch app in browser.
#'   Default: TRUE
#'
#' @return Invisibly returns NULL. The app runs until user closes it.
#'
#' @details
#' ## Workflow
#'
#' 1. **Data Input**: Upload Excel file or provide R data.frame
#' 2. **Column Selection**: Choose column containing taxonomic names
#' 3. **Auto Matching**: Automatic matching using hierarchical strategy:
#'    - Exact match on full name
#'    - Genus-constrained fuzzy match (searches species within matched genera)
#'    - Full database fuzzy match (last resort)
#' 4. **Review** (Phase 3): Manually review unmatched names with suggestions
#' 5. **Export**: Download results in Excel, CSV, or RDS format
#'
#' ## Match Quality
#'
#' The app uses the new `match_taxonomic_names()` function which provides:
#' - Intelligent genus-constrained fuzzy matching
#' - Similarity scoring for match quality assessment
#' - Synonym resolution
#' - Taxonomic hierarchy information
#'
#' ## Data Format
#'
#' Input data should contain:
#' - One column with taxonomic names (genus + species, optionally with authors)
#' - Other columns with associated data (plots, measurements, etc.)
#'
#' Output data includes all original columns plus:
#' - `idtax_n`: Matched taxon ID
#' - `idtax_good_n`: Accepted taxon ID (for synonyms)
#' - `matched_name`: Matched name from backbone
#' - `corrected_name`: Final standardized name
#' - `match_method`: How name was matched (exact, genus_constrained, fuzzy)
#' - `match_score`: Similarity score (0-1)
#' - `is_synonym`: Whether matched name is a synonym
#' - `accepted_name`: Accepted name if synonym
#'
#' @examples
#' \dontrun{
#' # Launch with file upload interface
#' launch_taxonomic_match_app()
#'
#' # Launch with R data
#' my_data <- read.csv("tree_inventory.csv")
#' launch_taxonomic_match_app(
#'   data = my_data,
#'   name_column = "species_name"
#' )
#'
#' # Launch in French
#' launch_taxonomic_match_app(language = "fr")
#'
#' # More strict fuzzy matching
#' launch_taxonomic_match_app(min_similarity = 0.7)
#'
#' # Show more suggestions per name
#' launch_taxonomic_match_app(max_suggestions = 20)
#' }
#'
#' @seealso
#' - [match_taxonomic_names()] for the underlying matching function
#' - [query_taxa()] for programmatic taxonomic queries
#' - [standardize_taxonomic_batch()] for batch processing without Shiny
#'
#' @export
launch_taxonomic_match_app <- function(
  data = NULL,
  name_column = NULL,
  language = c("en", "fr"),
  min_similarity = 0.3,
  max_suggestions = 10,
  mode = c("interactive", "batch"),
  launch.browser = TRUE
) {

  # Validate parameters
  language <- match.arg(language)
  mode <- match.arg(mode)

  if (!is.null(data) && !is.data.frame(data)) {
    stop("'data' must be a data.frame or NULL")
  }

  if (!is.null(name_column) && !is.character(name_column)) {
    stop("'name_column' must be a character string or NULL")
  }

  if (!is.numeric(min_similarity) || min_similarity < 0 || min_similarity > 1) {
    stop("'min_similarity' must be a number between 0 and 1")
  }

  if (!is.numeric(max_suggestions) || max_suggestions < 1) {
    stop("'max_suggestions' must be a positive integer")
  }

  # Verify database connection
  tryCatch({
    con <- call.mydb.taxa()
    cli::cli_alert_success("Connected to taxonomic backbone database")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to connect to database: {e$message}")
    cli::cli_alert_info("Please check your database credentials")
    stop("Database connection required to run app")
  })

  # Launch message
  cli::cli_h1("Taxonomic Name Standardization App")
  cli::cli_alert_info("Launching Shiny app in {.strong {language}} mode")

  if (!is.null(data)) {
    cli::cli_alert_info("Pre-loaded data: {nrow(data)} rows, {ncol(data)} columns")
  }

  if (!is.null(name_column)) {
    cli::cli_alert_info("Pre-selected column: {.field {name_column}}")
  }

  cli::cli_alert_info("Minimum similarity: {.val {min_similarity}}")
  cli::cli_alert_info("Max suggestions: {.val {max_suggestions}}")
  cli::cli_rule()

  # Create and launch app
  app <- app_taxonomic_match(
    data = data,
    name_column = name_column,
    language = language,
    min_similarity = min_similarity,
    max_suggestions = max_suggestions,
    mode = mode
  )

  # Run app
  shiny::runApp(
    app,
    launch.browser = launch.browser,
    quiet = FALSE
  )

  invisible(NULL)
}
