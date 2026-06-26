# Growth and Census Analysis Functions
#
# This file contains functions for computing tree growth rates and analyzing
# census data from forest plot inventories. These functions handle multiple
# census periods, outlier detection, and mortality/recruitment calculations.
#
# Main functions:
# - compute_growth(): Calculate growth rates between censuses
# - compute_mortality(): Calculate mortality and recruitment rates
#
# Dependencies: dplyr, cli, date
# Adapted from CTFS R Package: http://ctfs.si.edu/Public/CTFSRPackage/

#' Compute growth rates for permanent plots
#'
#' @description
#' Calculates tree growth rates between successive censuses for plots with
#' multiple census records. The function automatically fetches data from the
#' database and computes growth metrics.
#'
#' @param plot_ids Integer vector. Plot IDs to analyze.
#' @param plot_names Character vector. Plot names to analyze.
#' @param mindbh Numeric. Minimum diameter (mm) for including measurements. Default 100.
#' @param err.limit Numeric. Error limit for negative growth detection. Default 4.
#' @param maxgrow Numeric. Maximum valid growth rate (mm/year). Default 75.
#' @param method Character. Growth calculation method: "I" for incremental or "E" for exponential.
#' @param return_individual Logical. Whether to return individual-level growth data.
#' @param con Database connection. If NULL, connects automatically.
#'
#' @returns A list with:
#'   - `summary`: Data frame with growth summary per plot and census interval
#'   - `individuals`: Data frame with individual growth rates (if return_individual = TRUE)
#'
#' @examples
#' \dontrun{
#' # Compute growth for specific plots by ID
#' growth <- compute_growth(plot_ids = c(1, 2, 3))
#'
#' # Compute growth for plots by name
#' growth <- compute_growth(plot_names = c("plot001", "plot002"))
#'
#' # Get only summary statistics
#' growth <- compute_growth(plot_ids = 1, return_individual = FALSE)
#' }
#'
#' @export
compute_growth <- function(plot_ids = NULL,
                           plot_names = NULL,
                           mindbh = 100,
                           err.limit = 4,
                           maxgrow = 75,
                           method = c("I", "E"),
                           return_individual = TRUE,
                           con = NULL) {

  method <- match.arg(method)

  # Check inputs
  if (is.null(plot_ids) && is.null(plot_names)) {
    stop("Either plot_ids or plot_names must be provided")
  }

  # Connect to database
  if (is.null(con)) {
    con <- call.mydb()
  }

  cli::cli_h2("Computing growth rates")

  # Fetch data using query_plots
  cli::cli_alert_info("Fetching census data...")

  query_result <- query_plots(
    plot_name = plot_names,
    id_plot = plot_ids,
    extract_individuals = TRUE,
    show_multiple_census = TRUE,
    remove_ids = FALSE,
    extract_traits = FALSE,
    extract_individual_features = TRUE,
    extract_subplot_features = TRUE,
    interactive = FALSE,
    output_style = "full",
    con = con
  )

  # Extract data components
  if (!any(names(query_result) == "census_features")) {
    stop("No census data found. Make sure plots have multiple censuses recorded.")
  }

  dataset <- query_result$extract
  census_info <- query_result$census_features

  if (is.null(dataset) || nrow(dataset) == 0) {
    stop("No individual data found for the specified plots")
  }

  # Identify plots with multiple censuses
  census_cols <- grep("^stem_diameter_census_\\d+$", names(dataset), value = TRUE)
  n_censuses <- length(census_cols)

  if (n_censuses < 2) {
    stop("Need at least 2 censuses. Found only ", n_censuses)
  }

  cli::cli_alert_success("Found {n_censuses} censuses")

  # Get unique plots
  plot_col <- "id_table_liste_plots_n"

  unique_plots <- unique(dataset[[plot_col]])
  cli::cli_alert_info("Processing {length(unique_plots)} plot(s)")

  # Initialize results
  all_summaries <- list()
  all_individuals <- list()

  # Process each plot
  for (plot_id in unique_plots) {

    plot_data <- dataset[dataset[[plot_col]] == plot_id, ]
    plot_name_val <- unique(plot_data$plot_name)[1]

    cli::cli_alert_info("Processing: {plot_name_val}")

    # Process each consecutive census pair
    for (i in 1:(n_censuses - 1)) {

      # Get column names for this census pair
      dbh_col_1 <- paste0("stem_diameter_census_", i)
      dbh_col_2 <- paste0("stem_diameter_census_", i + 1)
      date_col_1 <- paste0("date_census_julian_", i)
      date_col_2 <- paste0("date_census_julian_", i + 1)

      # Check if columns exist
      if (!dbh_col_1 %in% names(plot_data) || !dbh_col_2 %in% names(plot_data)) {
        next
      }

      # Extract census pair data
      cols_to_keep <- c("id_n", "tag", "plot_name", dbh_col_1, dbh_col_2)
      if (date_col_1 %in% names(plot_data)) cols_to_keep <- c(cols_to_keep, date_col_1)
      if (date_col_2 %in% names(plot_data)) cols_to_keep <- c(cols_to_keep, date_col_2)

      census_pair <- plot_data[, cols_to_keep, drop = FALSE]

      # Standardize column names
      names(census_pair)[names(census_pair) == dbh_col_1] <- "dbh_1"
      names(census_pair)[names(census_pair) == dbh_col_2] <- "dbh_2"
      if (date_col_1 %in% names(census_pair)) {
        names(census_pair)[names(census_pair) == date_col_1] <- "date_julian_1"
      }
      if (date_col_2 %in% names(census_pair)) {
        names(census_pair)[names(census_pair) == date_col_2] <- "date_julian_2"
      }

      # Filter valid measurements (both censuses present)
      valid_data <- census_pair[!is.na(census_pair$dbh_1) &
                                  !is.na(census_pair$dbh_2) &
                                  census_pair$dbh_1 > 0 &
                                  census_pair$dbh_2 > 0, ]

      if (nrow(valid_data) == 0) {
        cli::cli_alert_warning("No valid measurements for census pair {i}-{i+1}")
        next
      }

      # Calculate time difference
      if ("date_julian_1" %in% names(valid_data) && "date_julian_2" %in% names(valid_data)) {
        valid_data$time_diff <- (valid_data$date_julian_2 - valid_data$date_julian_1) / 365.25
      } else {
        cli::cli_alert_warning("No date information for census pair {i}-{i+1}")
        next
      }

      # Convert to mm for calculations
      valid_data$dbh_mm_1 <- valid_data$dbh_1 * 10
      valid_data$dbh_mm_2 <- valid_data$dbh_2 * 10

      # Apply growth trimming
      valid_data <- .trim.growth_internal(
        valid_data,
        err.limit = err.limit,
        maxgrow = maxgrow,
        mindbh = mindbh
      )

      # Calculate growth rate
      if (method == "I") {
        valid_data$growthrate <- (valid_data$dbh_mm_2 - valid_data$dbh_mm_1) / valid_data$time_diff
      } else {
        valid_data$growthrate <- (log(valid_data$dbh_mm_2) - log(valid_data$dbh_mm_1)) / valid_data$time_diff
      }

      # Set NA for excluded measurements
      valid_data$growthrate[!valid_data$accepted_growth] <- NA

      # Create summary
      summary_row <- data.frame(
        plot_name = plot_name_val,
        census_pair = paste0(i, "-", i + 1),
        n_individuals = nrow(valid_data),
        n_valid = sum(valid_data$accepted_growth, na.rm = TRUE),
        n_excluded = sum(!valid_data$accepted_growth, na.rm = TRUE),
        mean_growth_mm_yr = mean(valid_data$growthrate, na.rm = TRUE),
        sd_growth_mm_yr = sd(valid_data$growthrate, na.rm = TRUE),
        median_growth_mm_yr = stats::median(valid_data$growthrate, na.rm = TRUE),
        mean_dbh_1_mm = mean(valid_data$dbh_mm_1[valid_data$accepted_growth], na.rm = TRUE),
        mean_dbh_2_mm = mean(valid_data$dbh_mm_2[valid_data$accepted_growth], na.rm = TRUE),
        mean_interval_years = mean(valid_data$time_diff[valid_data$accepted_growth], na.rm = TRUE),
        stringsAsFactors = FALSE
      )

      all_summaries[[length(all_summaries) + 1]] <- summary_row

      # Store individual results
      if (return_individual) {
        valid_data$census_pair <- paste0(i, "-", i + 1)

        # Add taxonomic info if available
        tax_cols <- intersect(c("tax_fam", "tax_gen", "tax_sp_level", "idtax_n"),
                              names(plot_data))
        if (length(tax_cols) > 0) {
          valid_data <- merge(valid_data,
                              unique(plot_data[, c("id_n", tax_cols), drop = FALSE]),
                              by = "id_n", all.x = TRUE)
        }

        all_individuals[[length(all_individuals) + 1]] <- valid_data
      }
    }
  }

  # Combine results
  if (length(all_summaries) == 0) {
    cli::cli_alert_warning("No growth data could be computed")
    return(NULL)
  }

  summary_df <- do.call(rbind, all_summaries)

  result <- list(
    summary = dplyr::as_tibble(summary_df)
  )

  if (return_individual && length(all_individuals) > 0) {
    result$individuals <- dplyr::as_tibble(do.call(rbind, all_individuals))
  }

  cli::cli_alert_success("Growth computation complete for {nrow(summary_df)} census intervals")

  return(result)
}


#' Compute mortality and recruitment rates
#'
#' @description
#' Calculates mortality and recruitment rates between successive censuses
#' for plots with multiple census records.
#'
#' @param plot_ids Integer vector. Plot IDs to analyze.
#' @param plot_names Character vector. Plot names to analyze.
#' @param mindbh Numeric. Minimum diameter (mm) for including measurements. Default 100.
#' @param con Database connection. If NULL, connects automatically.
#'
#' @returns A list with:
#'   - `summary`: Data frame with mortality/recruitment summary per plot and census interval
#'   - `dead_individuals`: Data frame with individuals that died
#'   - `recruits`: Data frame with newly recruited individuals
#'
#' @examples
#' \dontrun{
#' # Compute mortality for specific plots
#' mort <- compute_mortality(plot_ids = c(1, 2, 3))
#'
#' # View summary
#' mort$summary
#'
#' # View dead individuals
#' mort$dead_individuals
#' }
#'
#' @export
compute_mortality <- function(plot_ids = NULL,
                              plot_names = NULL,
                              mindbh = 100,
                              con = NULL) {

  # Check inputs
  if (is.null(plot_ids) && is.null(plot_names)) {
    stop("Either plot_ids or plot_names must be provided")
  }

  # Connect to database
  if (is.null(con)) {
    con <- call.mydb()
  }

  cli::cli_h2("Computing mortality and recruitment rates")

  # Fetch data
  cli::cli_alert_info("Fetching census data...")

  query_result <- query_plots(
    plot_name = plot_names,
    id_plot = plot_ids,
    extract_individuals = TRUE,
    show_multiple_census = TRUE,
    remove_ids = FALSE,
    extract_traits = FALSE,
    extract_individual_features = TRUE,
    extract_subplot_features = TRUE,
    interactive = FALSE,
    output_style = "full",
    con = con
  )

  # Extract data
  if (!any(names(query_result) == "census_features")) {
    stop("No census data found. Make sure plots have multiple censuses recorded.")
  }

  dataset <- query_result$extract

  if (is.null(dataset) || nrow(dataset) == 0) {
    stop("No individual data found for the specified plots")
  }

  # Identify censuses
  census_cols <- grep("^stem_diameter_census_\\d+$", names(dataset), value = TRUE)
  n_censuses <- length(census_cols)

  if (n_censuses < 2) {
    stop("Need at least 2 censuses. Found only ", n_censuses)
  }

  # Get plot column
  plot_col <- "id_table_liste_plots_n"
  # if ("id_table_liste_plots_n" %in% names(dataset)) {
  #   "id_table_liste_plots_n"
  # } else if ("id_liste_plots" %in% names(dataset)) {
  #   "id_liste_plots"
  # } else {
  #   stop("Cannot find plot ID column")
  # }

  unique_plots <- unique(dataset[[plot_col]])
  cli::cli_alert_info("Processing {length(unique_plots)} plot(s)")

  # Initialize results
  all_summaries <- list()
  all_dead <- list()
  all_recruits <- list()

  # Process each plot
  for (plot_id in unique_plots) {

    plot_data <- dataset[dataset[[plot_col]] == plot_id, ]
    plot_name_val <- unique(plot_data$plot_name)[1]

    cli::cli_alert_info("Processing: {plot_name_val}")

    # Process each census pair
    for (i in 1:(n_censuses - 1)) {

      dbh_col_1 <- paste0("stem_diameter_census_", i)
      dbh_col_2 <- paste0("stem_diameter_census_", i + 1)
      date_col_1 <- paste0("date_census_julian_", i)
      date_col_2 <- paste0("date_census_julian_", i + 1)

      if (!dbh_col_1 %in% names(plot_data) || !dbh_col_2 %in% names(plot_data)) {
        next
      }

      # Get dbh values
      dbh_1 <- plot_data[[dbh_col_1]]
      dbh_2 <- plot_data[[dbh_col_2]]

      # Get dates if available
      if (date_col_1 %in% names(plot_data) && date_col_2 %in% names(plot_data)) {
        date_1 <- plot_data[[date_col_1]]
        date_2 <- plot_data[[date_col_2]]

        valid_dates <- !is.na(date_1) & !is.na(date_2)
        if (sum(valid_dates) > 0) {
          mean_interval <- mean((date_2[valid_dates] - date_1[valid_dates]) / 365.25)
        } else {
          mean_interval <- NA
        }
      } else {
        mean_interval <- NA
      }

      # Identify individuals alive in census 1 (above mindbh)
      alive_census_1 <- !is.na(dbh_1) & dbh_1 > 0 & (dbh_1 * 10) >= mindbh

      # Identify dead: alive in census 1, dead/missing in census 2
      is_dead <- alive_census_1 & (is.na(dbh_2) | dbh_2 == 0)

      # Identify recruits: not in census 1, present in census 2
      is_recruit <- (is.na(dbh_1) | dbh_1 == 0) & !is.na(dbh_2) & dbh_2 > 0

      # Calculate rates
      N_outset <- sum(alive_census_1)
      N_dead <- sum(is_dead)
      N_survivor <- N_outset - N_dead
      N_recruits <- sum(is_recruit)

      # Mortality rate (exponential)
      if (N_outset > 0 && N_survivor > 0 && !is.na(mean_interval) && mean_interval > 0) {
        mortality_rate <- (log(N_outset) - log(N_survivor)) / mean_interval
        recruitment_rate <- (log(N_survivor + N_recruits) - log(N_survivor)) / mean_interval
      } else {
        mortality_rate <- NA
        recruitment_rate <- NA
      }

      # Summary
      summary_row <- data.frame(
        plot_name = plot_name_val,
        census_pair = paste0(i, "-", i + 1),
        N_outset = N_outset,
        N_dead = N_dead,
        N_survivor = N_survivor,
        N_recruits = N_recruits,
        mortality_rate = mortality_rate,
        mortality_percent_yr = (1 - exp(-mortality_rate)) * 100,
        recruitment_rate = recruitment_rate,
        recruitment_percent_yr = (exp(recruitment_rate) - 1) * 100,
        mean_interval_years = mean_interval,
        mean_dbh_dead_cm = ifelse(N_dead > 0, mean(dbh_1[is_dead], na.rm = TRUE), NA),
        mean_dbh_recruit_cm = ifelse(N_recruits > 0, mean(dbh_2[is_recruit], na.rm = TRUE), NA),
        stringsAsFactors = FALSE
      )

      all_summaries[[length(all_summaries) + 1]] <- summary_row

      # Store dead individuals
      if (sum(is_dead) > 0) {
        dead_df <- plot_data[is_dead, c("id_n", "tag", "plot_name", dbh_col_1), drop = FALSE]
        dead_df$census_pair <- paste0(i, "-", i + 1)
        names(dead_df)[names(dead_df) == dbh_col_1] <- "dbh_at_death_cm"

        # Add taxonomy
        tax_cols <- intersect(c("tax_fam", "tax_gen", "tax_sp_level"), names(plot_data))
        if (length(tax_cols) > 0) {
          dead_df <- cbind(dead_df, plot_data[is_dead, tax_cols, drop = FALSE])
        }

        all_dead[[length(all_dead) + 1]] <- dead_df
      }

      # Store recruits
      if (sum(is_recruit) > 0) {
        recruit_df <- plot_data[is_recruit, c("id_n", "tag", "plot_name", dbh_col_2), drop = FALSE]
        recruit_df$census_pair <- paste0(i, "-", i + 1)
        names(recruit_df)[names(recruit_df) == dbh_col_2] <- "dbh_at_recruitment_cm"

        tax_cols <- intersect(c("tax_fam", "tax_gen", "tax_sp_level"), names(plot_data))
        if (length(tax_cols) > 0) {
          recruit_df <- cbind(recruit_df, plot_data[is_recruit, tax_cols, drop = FALSE])
        }

        all_recruits[[length(all_recruits) + 1]] <- recruit_df
      }
    }
  }

  # Combine results
  if (length(all_summaries) == 0) {
    cli::cli_alert_warning("No mortality data could be computed")
    return(NULL)
  }

  result <- list(
    summary = dplyr::as_tibble(do.call(rbind, all_summaries))
  )

  if (length(all_dead) > 0) {
    result$dead_individuals <- dplyr::as_tibble(do.call(rbind, all_dead))
  }

  if (length(all_recruits) > 0) {
    result$recruits <- dplyr::as_tibble(do.call(rbind, all_recruits))
  }

  cli::cli_alert_success("Mortality computation complete for {nrow(result$summary)} census intervals")

  return(result)
}


# Internal helper functions -----------------------------------------------

#' Internal growth trimming function
#'
#' Identifies and flags problematic growth measurements based on
#' statistical criteria adapted from CTFS R Package.
#'
#' @param data Data frame with dbh_mm_1, dbh_mm_2, and time_diff columns
#' @param slope Slope parameter for error model. Default 0.006214.
#' @param intercept Intercept parameter for error model. Default 0.9036.
#' @param err.limit Number of standard deviations for negative growth threshold. Default 4.
#' @param maxgrow Maximum valid growth rate in mm/year. Default 75.
#' @param mindbh Minimum diameter in mm for inclusion. Default 100.
#'
#' @returns Data frame with added accepted_growth column
#'
#' @keywords internal
#' @noRd
.trim.growth_internal <- function(data,
                                   slope = 0.006214,
                                   intercept = 0.9036,
                                   err.limit = 4,
                                   maxgrow = 75,
                                   mindbh = 100) {

  # Standard deviation based on DBH
  stdev.dbh1 <- slope * data$dbh_mm_1 + intercept

  # Growth rate
  growth <- (data$dbh_mm_2 - data$dbh_mm_1) / data$time_diff

  # Identify problematic measurements
  bad.neggrow <- data$dbh_mm_2 <= (data$dbh_mm_1 - err.limit * stdev.dbh1)
  bad.posgrow <- growth > maxgrow

  # Acceptance criteria
  accept <- rep(TRUE, nrow(data))
  accept[bad.neggrow] <- FALSE
  accept[bad.posgrow] <- FALSE
  accept[is.na(growth)] <- FALSE
  accept[data$dbh_mm_1 < mindbh] <- FALSE
  accept[is.na(data$dbh_mm_1) | is.na(data$dbh_mm_2) | data$dbh_mm_2 <= 0] <- FALSE

  data$accepted_growth <- accept

  return(data)
}
