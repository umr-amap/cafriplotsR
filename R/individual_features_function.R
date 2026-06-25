
#' Aggregate individual features to individual level
#'
#' Takes raw trait measurements and aggregates them by individual, handling
#' multiple census and multiple values appropriately. Uses data.table for
#' optimal performance on large datasets.
#'
#' @param individual_ids Vector of individual IDs
#' @param trait_ids Vector of trait IDs to extract (optional)
#' @param plot_ids Vector of plot IDs (optional, for filtering)
#' @param include_multi_census Include census-specific values
#' @param issues Character. How to handle flagged measurements: "remove" (default,
#'   drop flagged rows), "include" (keep rows and add issue column), or "ignore"
#'   (keep rows, no issue column).
#' @param aggregation_mode How to aggregate: "mean", "last", "mode", "concat"
#' @param include_measurement_ids Include aggregated id_trait_measures column (default FALSE)
#' @param con Database connection
#'
#' @return Tibble with one row per individual and aggregated feature values
#' @export
get_individual_aggregated_features <- function(
    individual_ids = NULL,
    trait_ids = NULL,
    plot_ids = NULL,
    include_multi_census = FALSE,
    issues = c("remove", "include", "ignore"),
    aggregation_mode = c("auto", "mean", "last", "mode", "concat"),
    include_measurement_ids = FALSE,
    census_strategy = c("last", "first", "mean"),
    con = NULL
) {

  aggregation_mode <- match.arg(aggregation_mode)
  census_strategy <- match.arg(census_strategy)
  issues <- match.arg(issues)
  if (is.null(con)) con <- call.mydb()
  
  # If plot_ids provided, get individual_ids from plots
  if (!is.null(plot_ids) && is.null(individual_ids)) {
    cli::cli_alert_info("Fetching individuals from plot(s): {paste(plot_ids, collapse=', ')}")
    
    individual_ids_query <- DBI::dbGetQuery(con, glue::glue_sql("
      SELECT DISTINCT id_n
      FROM data_individuals
      WHERE id_table_liste_plots_n IN ({plot_ids*})
    ", plot_ids = plot_ids, .con = con))
    
    individual_ids <- individual_ids_query$id_n
    
    if (length(individual_ids) == 0) {
      cli::cli_alert_warning("No individuals found in specified plot(s)")
      return(tibble(id_data_individuals = integer()))
    }
    
    cli::cli_alert_info("Found {length(individual_ids)} individual(s) in plot(s)")
  }
  
  # If both plot_ids and individual_ids provided, intersect them
  if (!is.null(plot_ids) && !is.null(individual_ids)) {
    cli::cli_alert_info("Filtering individuals by plot(s): {paste(plot_ids, collapse=', ')}")
    
    individuals_in_plots <- DBI::dbGetQuery(con, glue::glue_sql("
      SELECT DISTINCT id_n
      FROM data_individuals
      WHERE id_table_liste_plots_n IN ({plot_ids*})
        AND id_n IN ({individual_ids*})
    ", plot_ids = plot_ids, individual_ids = individual_ids, .con = con))
    
    individual_ids <- individuals_in_plots$id_n
    
    if (length(individual_ids) == 0) {
      cli::cli_alert_warning("No matching individuals found")
      return(tibble(id_data_individuals = integer()))
    }
    
    cli::cli_alert_info("Filtered to {length(individual_ids)} individual(s)")
  }
  
  cli::cli_h2("Fetching individual features")
  
  # 1. Get raw measurements in LONG format
  raw_data <- query_individual_features(
    individual_ids = individual_ids,
    trait_ids = trait_ids,
    include_multi_census = include_multi_census,
    format = "long",
    issues = issues,
    census_strategy = census_strategy,
    con = con
  )

  if (nrow(raw_data) == 0) {
    cli::cli_alert_warning("No features found")
    return(tibble(id_data_individuals = integer()))
  }

  # 2. Separate by value type and aggregate with data.table
  cli::cli_h2("Aggregating features by individual")

  # Derive internal booleans for the aggregate functions
  .include_issue <- (issues == "include")

  numeric_features <- aggregate_numeric_features_dt(
    data = raw_data %>% filter(valuetype %in% c("numeric", "integer")),
    include_census = include_multi_census,
    mode = aggregation_mode,
    include_issue = .include_issue,
    include_measurement_ids = include_measurement_ids,
    census_strategy = census_strategy
  )

  character_features <- aggregate_character_features_dt(
    data = raw_data %>% filter(valuetype %in% c("character", "ordinal", "categorical")),
    include_census = include_multi_census,
    mode = aggregation_mode,
    include_issue = .include_issue,
    include_measurement_ids = include_measurement_ids,
    census_strategy = census_strategy
  )
  
  # 3. Combine results
  if (is.null(numeric_features) && is.null(character_features)) {
    return(tibble(id_data_individuals = unique(raw_data$id_data_individuals)))
  }
  
  result <- merge(
    numeric_features %||% data.table(id_data_individuals = integer()),
    character_features %||% data.table(id_data_individuals = integer()),
    by = "id_data_individuals",
    all = TRUE
  )
  
  result <- as_tibble(result)
  
  cli::cli_alert_success("Aggregated {nrow(result)} individual(s)")
  
  return(result)
}

#' Aggregate numeric features using data.table
#' @keywords internal
aggregate_numeric_features_dt <- function(data, include_census, mode,
                                          include_issue = FALSE,
                                          include_measurement_ids = FALSE,
                                          census_strategy = c("last", "first", "mean")) {

  census_strategy <- match.arg(census_strategy)
  if (nrow(data) == 0) return(NULL)

  cli::cli_alert_info("Aggregating {nrow(data)} numeric measurement(s)")

  # Convert to data.table
  if (!data.table::is.data.table(data)) data.table::setDT(data)

  # Convert traitvalue to numeric
  data[, traitvalue_num := as.numeric(traitvalue)]

  if (include_census && "census_name" %in% names(data) && any(!is.na(data$census_name))) {

    # Split data into census-linked and non-census features
    data_census <- data[!is.na(census_name)]
    data_no_census <- data[is.na(census_name)]

    results_to_merge <- list()

    # Process census-linked features with census columns
    if (nrow(data_census) > 0) {
      # Aggregate by (individual, subplot, trait, census)
      if (include_issue && "issue" %in% names(data_census) &&
          include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
        # Include both issue and IDs
        dt_agg <- data_census[,
                              .(value = mean(traitvalue_num, na.rm = TRUE),
                                issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                              by = .(id_data_individuals, id_sub_plots, trait, census_name)]
      } else if (include_issue && "issue" %in% names(data_census)) {
        # Include only issue
        dt_agg <- data_census[,
                              .(value = mean(traitvalue_num, na.rm = TRUE),
                                issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                              by = .(id_data_individuals, id_sub_plots, trait, census_name)]
      } else if (include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
        # Include only IDs
        dt_agg <- data_census[,
                              .(value = mean(traitvalue_num, na.rm = TRUE),
                                ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                              by = .(id_data_individuals, id_sub_plots, trait, census_name)]
      } else {
        # No additional columns
        dt_agg <- data_census[,
                              .(value = mean(traitvalue_num, na.rm = TRUE)),
                              by = .(id_data_individuals, id_sub_plots, trait, census_name)]
      }
      


      # Further aggregate by (individual, trait, census) - mean across subplots
      if (include_issue && "issue_agg" %in% names(dt_agg) &&
          include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
        dt_ind <- dt_agg[,
                         .(value = mean(value, na.rm = TRUE),
                           issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                           ids_agg = paste(unique(ids_agg), collapse = ",")),
                         by = .(id_data_individuals, trait, census_name)]
      } else if (include_issue && "issue_agg" %in% names(dt_agg)) {
        dt_ind <- dt_agg[,
                         .(value = mean(value, na.rm = TRUE),
                           issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                         by = .(id_data_individuals, trait, census_name)]
      } else if (include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
        dt_ind <- dt_agg[,
                         .(value = mean(value, na.rm = TRUE),
                           ids_agg = paste(unique(ids_agg), collapse = ",")),
                         by = .(id_data_individuals, trait, census_name)]
      } else {
        dt_ind <- dt_agg[,
                         .(value = mean(value, na.rm = TRUE)),
                         by = .(id_data_individuals, trait, census_name)]
      }
      
      dt_ind <- 
        dt_ind %>% mutate(value = round(value, 2))

      # Pivot to wide format with trait_census columns
      value_vars <- c("value")
      if (include_issue && "issue_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "issue_agg")
      if (include_measurement_ids && "ids_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "ids_agg")

      result_census <- data.table::dcast(
        dt_ind,
        id_data_individuals ~ trait + census_name,
        value.var = value_vars,
        sep = "_"
      )

      # Remove "value_" prefix from column names
      setnames(result_census,
               names(result_census),
               gsub("^value_", "", names(result_census)))

      results_to_merge[[1]] <- result_census
    }

    # Process non-census features without census columns
    if (nrow(data_no_census) > 0) {
      if ("id_sub_plots" %in% names(data_no_census)) {
        # Aggregate by subplot first, then by individual
        if (include_issue && "issue" %in% names(data_no_census) &&
            include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
          dt_subplot <- data_no_census[,
                             .(value = mean(traitvalue_num, na.rm = TRUE),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[,
                               .(value = mean(value, na.rm = TRUE),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data_no_census)) {
          dt_subplot <- data_no_census[,
                             .(value = mean(traitvalue_num, na.rm = TRUE),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[,
                               .(value = mean(value, na.rm = TRUE),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                               by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
          dt_subplot <- data_no_census[,
                             .(value = mean(traitvalue_num, na.rm = TRUE),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[,
                               .(value = mean(value, na.rm = TRUE),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else {
          dt_subplot <- data_no_census[,
                             .(value = mean(traitvalue_num, na.rm = TRUE)),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[,
                               .(value = mean(value, na.rm = TRUE)),
                               by = .(id_data_individuals, trait)]
        }
      } else {
        # Direct aggregation
        if (include_issue && "issue" %in% names(data_no_census) &&
            include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
          dt_ind <- data_no_census[,
                         .(value = mean(traitvalue_num, na.rm = TRUE),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data_no_census)) {
          dt_ind <- data_no_census[,
                         .(value = mean(traitvalue_num, na.rm = TRUE),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                         by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
          dt_ind <- data_no_census[,
                         .(value = mean(traitvalue_num, na.rm = TRUE),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else {
          dt_ind <- data_no_census[,
                         .(value = mean(traitvalue_num, na.rm = TRUE)),
                         by = .(id_data_individuals, trait)]
        }
      }

      # Pivot to wide format
      value_vars <- c("value")
      if (include_issue && "issue_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "issue_agg")
      if (include_measurement_ids && "ids_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "ids_agg")

      result_no_census <- data.table::dcast(
        dt_ind,
        id_data_individuals ~ trait,
        value.var = value_vars
      )

      # Remove "value_" prefix from column names
      setnames(result_no_census,
               names(result_no_census),
               gsub("^value_", "", names(result_no_census)))

      results_to_merge[[2]] <- result_no_census
    }

    # Merge census and non-census results
    if (length(results_to_merge) == 2) {
      result <- merge(results_to_merge[[1]], results_to_merge[[2]],
                      by = "id_data_individuals", all = TRUE)
    } else if (length(results_to_merge) == 1) {
      result <- results_to_merge[[1]]
    } else {
      return(NULL)
    }

  } else {

    # Without census: aggregate by (individual, trait)
    if ("id_sub_plots" %in% names(data)) {
      # Aggregate by subplot first, then by individual
      if (include_issue && "issue" %in% names(data) &&
          include_measurement_ids && "id_trait_measures" %in% names(data)) {
        dt_subplot <- data[,
                           .(value = mean(traitvalue_num, na.rm = TRUE),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        dt_ind <- dt_subplot[,
                             .(value = mean(value, na.rm = TRUE),
                               issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                               ids_agg = paste(unique(ids_agg), collapse = ",")),
                             by = .(id_data_individuals, trait)]
      } else if (include_issue && "issue" %in% names(data)) {
        dt_subplot <- data[,
                           .(value = mean(traitvalue_num, na.rm = TRUE),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        dt_ind <- dt_subplot[,
                             .(value = mean(value, na.rm = TRUE),
                               issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                             by = .(id_data_individuals, trait)]
      } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
        dt_subplot <- data[,
                           .(value = mean(traitvalue_num, na.rm = TRUE),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        dt_ind <- dt_subplot[,
                             .(value = mean(value, na.rm = TRUE),
                               ids_agg = paste(unique(ids_agg), collapse = ",")),
                             by = .(id_data_individuals, trait)]
      } else {
        dt_subplot <- data[,
                           .(value = mean(traitvalue_num, na.rm = TRUE)),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        dt_ind <- dt_subplot[,
                             .(value = mean(value, na.rm = TRUE)),
                             by = .(id_data_individuals, trait)]
      }
    } else {
      # Direct aggregation
      if (include_issue && "issue" %in% names(data) &&
          include_measurement_ids && "id_trait_measures" %in% names(data)) {
        dt_ind <- data[,
                       .(value = mean(traitvalue_num, na.rm = TRUE),
                         issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                         ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                       by = .(id_data_individuals, trait)]
      } else if (include_issue && "issue" %in% names(data)) {
        dt_ind <- data[,
                       .(value = mean(traitvalue_num, na.rm = TRUE),
                         issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                       by = .(id_data_individuals, trait)]
      } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
        dt_ind <- data[,
                       .(value = mean(traitvalue_num, na.rm = TRUE),
                         ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                       by = .(id_data_individuals, trait)]
      } else {
        dt_ind <- data[,
                       .(value = mean(traitvalue_num, na.rm = TRUE)),
                       by = .(id_data_individuals, trait)]
      }
    }

    # Pivot to wide format
    value_vars <- c("value")
    if (include_issue && "issue_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "issue_agg")
    if (include_measurement_ids && "ids_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "ids_agg")

    result <- data.table::dcast(
      dt_ind,
      id_data_individuals ~ trait,
      value.var = value_vars
    )

    # Remove "value_" prefix from column names
    setnames(result,
             names(result),
             gsub("^value_", "", names(result)))
  }

  return(result)
}

#' Aggregate character features using data.table
#' @keywords internal
aggregate_character_features_dt <- function(data, include_census, mode,
                                            include_issue = FALSE,
                                            include_measurement_ids = FALSE,
                                            census_strategy = c("last", "first", "mean")) {

  census_strategy <- match.arg(census_strategy)
  if (nrow(data) == 0) return(NULL)

  cli::cli_alert_info("Aggregating {nrow(data)} character measurement(s)")

  # Convert to data.table
  if (!data.table::is.data.table(data)) data.table::setDT(data)

  # Clean text values
  data[, traitvalue_char := str_squish(traitvalue_char)]

  if (include_census && "census_name" %in% names(data) && any(!is.na(data$census_name))) {

    # Split data into census-linked and non-census features
    data_census <- data[!is.na(census_name)]
    data_no_census <- data[is.na(census_name)]

    results_to_merge <- list()

    # Process census-linked features with census columns
    if (nrow(data_census) > 0) {
      # Aggregate by (individual, subplot, trait, census) - mode determines aggregation
      if (mode == "mode") {
        if (include_issue && "issue" %in% names(data_census) &&
            include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = get_mode_dt(traitvalue_char),
                                  issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                  ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else if (include_issue && "issue" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = get_mode_dt(traitvalue_char),
                                  issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = get_mode_dt(traitvalue_char),
                                  ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = get_mode_dt(traitvalue_char)),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        }
      } else if (mode == "last") {
        if (include_issue && "issue" %in% names(data_census) &&
            include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = last(traitvalue_char),
                                  issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                  ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else if (include_issue && "issue" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = last(traitvalue_char),
                                  issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = last(traitvalue_char),
                                  ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = last(traitvalue_char)),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        }
      } else {
        # concat mode
        if (include_issue && "issue" %in% names(data_census) &&
            include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = paste(unique(traitvalue_char), collapse = ", "),
                                  issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                  ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else if (include_issue && "issue" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = paste(unique(traitvalue_char), collapse = ", "),
                                  issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data_census)) {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = paste(unique(traitvalue_char), collapse = ", "),
                                  ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        } else {
          dt_agg <- data_census[!is.na(traitvalue_char),
                                .(value = paste(unique(traitvalue_char), collapse = ", ")),
                                by = .(id_data_individuals, id_sub_plots, trait, census_name)]
        }
      }

      # Further aggregate by (individual, trait, census) across subplots
      if (mode == "mode") {
        if (include_issue && "issue_agg" %in% names(dt_agg) &&
            include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = get_mode_dt(value),
                             issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                             ids_agg = paste(unique(ids_agg), collapse = ",")),
                           by = .(id_data_individuals, trait, census_name)]
        } else if (include_issue && "issue_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = get_mode_dt(value),
                             issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                           by = .(id_data_individuals, trait, census_name)]
        } else if (include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = get_mode_dt(value),
                             ids_agg = paste(unique(ids_agg), collapse = ",")),
                           by = .(id_data_individuals, trait, census_name)]
        } else {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = get_mode_dt(value)),
                           by = .(id_data_individuals, trait, census_name)]
        }
      } else if (mode == "last") {
        if (include_issue && "issue_agg" %in% names(dt_agg) &&
            include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = last(value),
                             issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                             ids_agg = paste(unique(ids_agg), collapse = ",")),
                           by = .(id_data_individuals, trait, census_name)]
        } else if (include_issue && "issue_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = last(value),
                             issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                           by = .(id_data_individuals, trait, census_name)]
        } else if (include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = last(value),
                             ids_agg = paste(unique(ids_agg), collapse = ",")),
                           by = .(id_data_individuals, trait, census_name)]
        } else {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = last(value)),
                           by = .(id_data_individuals, trait, census_name)]
        }
      } else {
        # concat mode
        if (include_issue && "issue_agg" %in% names(dt_agg) &&
            include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = paste(unique(value), collapse = ", "),
                             issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                             ids_agg = paste(unique(ids_agg), collapse = ",")),
                           by = .(id_data_individuals, trait, census_name)]
        } else if (include_issue && "issue_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = paste(unique(value), collapse = ", "),
                             issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                           by = .(id_data_individuals, trait, census_name)]
        } else if (include_measurement_ids && "ids_agg" %in% names(dt_agg)) {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = paste(unique(value), collapse = ", "),
                             ids_agg = paste(unique(ids_agg), collapse = ",")),
                           by = .(id_data_individuals, trait, census_name)]
        } else {
          dt_ind <- dt_agg[!is.na(value),
                           .(value = paste(unique(value), collapse = ", ")),
                           by = .(id_data_individuals, trait, census_name)]
        }
      }

      # Pivot to wide with trait_census columns
      value_vars <- c("value")
      if (include_issue && "issue_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "issue_agg")
      if (include_measurement_ids && "ids_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "ids_agg")

      result_census <- data.table::dcast(
        dt_ind,
        id_data_individuals ~ trait + census_name,
        value.var = value_vars,
        sep = "_"
      )

      # Remove "value_" prefix from column names
      setnames(result_census,
               names(result_census),
               gsub("^value_", "", names(result_census)))

      results_to_merge[[1]] <- result_census
    }

    # Process non-census features without census columns
    if (nrow(data_no_census) > 0) {
      # Similar logic as census but without census_name column - uses same 3-mode aggregation
      if ("id_sub_plots" %in% names(data_no_census)) {
        # Aggregate by subplot first
        if (mode == "mode") {
          if (include_issue && "issue" %in% names(data_no_census) &&
              include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = get_mode_dt(traitvalue_char),
                                 issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                 ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = get_mode_dt(value),
                                   issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                   ids_agg = paste(unique(ids_agg), collapse = ",")),
                                 by = .(id_data_individuals, trait)]
          } else if (include_issue && "issue" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = get_mode_dt(traitvalue_char),
                                 issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = get_mode_dt(value),
                                   issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                                 by = .(id_data_individuals, trait)]
          } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = get_mode_dt(traitvalue_char),
                                 ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = get_mode_dt(value),
                                   ids_agg = paste(unique(ids_agg), collapse = ",")),
                                 by = .(id_data_individuals, trait)]
          } else {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = get_mode_dt(traitvalue_char)),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = get_mode_dt(value)),
                                 by = .(id_data_individuals, trait)]
          }
        } else if (mode == "last") {
          if (include_issue && "issue" %in% names(data_no_census) &&
              include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = last(traitvalue_char),
                                 issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                 ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = last(value),
                                   issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                   ids_agg = paste(unique(ids_agg), collapse = ",")),
                                 by = .(id_data_individuals, trait)]
          } else if (include_issue && "issue" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = last(traitvalue_char),
                                 issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = last(value),
                                   issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                                 by = .(id_data_individuals, trait)]
          } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = last(traitvalue_char),
                                 ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = last(value),
                                   ids_agg = paste(unique(ids_agg), collapse = ",")),
                                 by = .(id_data_individuals, trait)]
          } else {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = last(traitvalue_char)),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = last(value)),
                                 by = .(id_data_individuals, trait)]
          }
        } else {
          # concat mode
          if (include_issue && "issue" %in% names(data_no_census) &&
              include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = paste(unique(traitvalue_char), collapse = ", "),
                                 issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                                 ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = paste(unique(value), collapse = ", "),
                                   issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                   ids_agg = paste(unique(ids_agg), collapse = ",")),
                                 by = .(id_data_individuals, trait)]
          } else if (include_issue && "issue" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = paste(unique(traitvalue_char), collapse = ", "),
                                 issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = paste(unique(value), collapse = ", "),
                                   issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                                 by = .(id_data_individuals, trait)]
          } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = paste(unique(traitvalue_char), collapse = ", "),
                                 ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = paste(unique(value), collapse = ", "),
                                   ids_agg = paste(unique(ids_agg), collapse = ",")),
                                 by = .(id_data_individuals, trait)]
          } else {
            dt_subplot <- data_no_census[!is.na(traitvalue_char),
                               .(value = paste(unique(traitvalue_char), collapse = ", ")),
                               by = .(id_data_individuals, id_sub_plots, trait)]
            dt_ind <- dt_subplot[!is.na(value),
                                 .(value = paste(unique(value), collapse = ", ")),
                                 by = .(id_data_individuals, trait)]
          }
        }
      } else {
        # Direct aggregation without subplot
        if (mode == "mode") {
          if (include_issue && "issue" %in% names(data_no_census) &&
              include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = get_mode_dt(traitvalue_char),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, trait)]
          } else if (include_issue && "issue" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = get_mode_dt(traitvalue_char),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                           by = .(id_data_individuals, trait)]
          } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = get_mode_dt(traitvalue_char),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, trait)]
          } else {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = get_mode_dt(traitvalue_char)),
                           by = .(id_data_individuals, trait)]
          }
        } else if (mode == "last") {
          if (include_issue && "issue" %in% names(data_no_census) &&
              include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = last(traitvalue_char),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, trait)]
          } else if (include_issue && "issue" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = last(traitvalue_char),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                           by = .(id_data_individuals, trait)]
          } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = last(traitvalue_char),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, trait)]
          } else {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = last(traitvalue_char)),
                           by = .(id_data_individuals, trait)]
          }
        } else {
          # concat mode
          if (include_issue && "issue" %in% names(data_no_census) &&
              include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = paste(unique(traitvalue_char), collapse = ", "),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, trait)]
          } else if (include_issue && "issue" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = paste(unique(traitvalue_char), collapse = ", "),
                             issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                           by = .(id_data_individuals, trait)]
          } else if (include_measurement_ids && "id_trait_measures" %in% names(data_no_census)) {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = paste(unique(traitvalue_char), collapse = ", "),
                             ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                           by = .(id_data_individuals, trait)]
          } else {
            dt_ind <- data_no_census[!is.na(traitvalue_char),
                           .(value = paste(unique(traitvalue_char), collapse = ", ")),
                           by = .(id_data_individuals, trait)]
          }
        }
      }

      # Pivot to wide (NO PREFIX)
      value_vars <- c("value")
      if (include_issue && "issue_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "issue_agg")
      if (include_measurement_ids && "ids_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "ids_agg")

      result_no_census <- data.table::dcast(
        dt_ind,
        id_data_individuals ~ trait,
        value.var = value_vars
      )

      # Remove "value_" prefix from column names
      setnames(result_no_census,
               names(result_no_census),
               gsub("^value_", "", names(result_no_census)))

      results_to_merge[[2]] <- result_no_census
    }

    # Merge census and non-census results
    if (length(results_to_merge) == 2) {
      result <- merge(results_to_merge[[1]], results_to_merge[[2]],
                      by = "id_data_individuals", all = TRUE)
    } else if (length(results_to_merge) == 1) {
      result <- results_to_merge[[1]]
    } else {
      return(NULL)
    }

  } else {

    # Without census: aggregate by (individual, trait) - exactly same logic as non-census above
    if ("id_sub_plots" %in% names(data)) {
      # Aggregate by subplot first
      if (mode == "mode") {
        if (include_issue && "issue" %in% names(data) &&
            include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = get_mode_dt(traitvalue_char),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = get_mode_dt(value),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = get_mode_dt(traitvalue_char),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = get_mode_dt(value),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                               by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = get_mode_dt(traitvalue_char),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = get_mode_dt(value),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = get_mode_dt(traitvalue_char)),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = get_mode_dt(value)),
                               by = .(id_data_individuals, trait)]
        }
      } else if (mode == "last") {
        if (include_issue && "issue" %in% names(data) &&
            include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = last(traitvalue_char),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = last(value),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = last(traitvalue_char),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = last(value),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                               by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = last(traitvalue_char),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = last(value),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = last(traitvalue_char)),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = last(value)),
                               by = .(id_data_individuals, trait)]
        }
      } else {
        # concat mode
        if (include_issue && "issue" %in% names(data) &&
            include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = paste(unique(traitvalue_char), collapse = ", "),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = paste(unique(value), collapse = ", "),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | "),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = paste(unique(traitvalue_char), collapse = ", "),
                               issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = paste(unique(value), collapse = ", "),
                                 issue_agg = paste(unique(issue_agg[issue_agg != ""]), collapse = " | ")),
                               by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = paste(unique(traitvalue_char), collapse = ", "),
                               ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = paste(unique(value), collapse = ", "),
                                 ids_agg = paste(unique(ids_agg), collapse = ",")),
                               by = .(id_data_individuals, trait)]
        } else {
          dt_subplot <- data[!is.na(traitvalue_char),
                             .(value = paste(unique(traitvalue_char), collapse = ", ")),
                             by = .(id_data_individuals, id_sub_plots, trait)]
          dt_ind <- dt_subplot[!is.na(value),
                               .(value = paste(unique(value), collapse = ", ")),
                               by = .(id_data_individuals, trait)]
        }
      }
    } else {
      # Direct aggregation without subplot
      if (mode == "mode") {
        if (include_issue && "issue" %in% names(data) &&
            include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = get_mode_dt(traitvalue_char),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = get_mode_dt(traitvalue_char),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                         by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = get_mode_dt(traitvalue_char),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = get_mode_dt(traitvalue_char)),
                         by = .(id_data_individuals, trait)]
        }
      } else if (mode == "last") {
        if (include_issue && "issue" %in% names(data) &&
            include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = last(traitvalue_char),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = last(traitvalue_char),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                         by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = last(traitvalue_char),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = last(traitvalue_char)),
                         by = .(id_data_individuals, trait)]
        }
      } else {
        # concat mode
        if (include_issue && "issue" %in% names(data) &&
            include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = paste(unique(traitvalue_char), collapse = ", "),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | "),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else if (include_issue && "issue" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = paste(unique(traitvalue_char), collapse = ", "),
                           issue_agg = paste(unique(issue[!is.na(issue)]), collapse = " | ")),
                         by = .(id_data_individuals, trait)]
        } else if (include_measurement_ids && "id_trait_measures" %in% names(data)) {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = paste(unique(traitvalue_char), collapse = ", "),
                           ids_agg = paste(unique(id_trait_measures), collapse = ",")),
                         by = .(id_data_individuals, trait)]
        } else {
          dt_ind <- data[!is.na(traitvalue_char),
                         .(value = paste(unique(traitvalue_char), collapse = ", ")),
                         by = .(id_data_individuals, trait)]
        }
      }
    }

    # Pivot to wide (NO PREFIX)
    value_vars <- c("value")
    if (include_issue && "issue_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "issue_agg")
    if (include_measurement_ids && "ids_agg" %in% names(dt_ind)) value_vars <- c(value_vars, "ids_agg")

    result <- data.table::dcast(
      dt_ind,
      id_data_individuals ~ trait,
      value.var = value_vars
    )

    # Remove "value_" prefix from column names
    setnames(result,
             names(result),
             gsub("^value_", "", names(result)))
  }

  # No prefix added - character and numeric traits share the same namespace
  return(result)
}

#' Get statistical mode using data.table
#' @keywords internal
get_mode_dt <- function(x) {
  if (length(x) == 0) return(NA_character_)
  
  # Count occurrences
  counts <- table(x)
  # Return most frequent
  names(counts)[which.max(counts)]
}

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Legacy function - wrapper for backward compatibility
#' 
#' This maintains the old API while using the new architecture internally.
#' @keywords internal
#' @export
.get_trait_individuals_values <- function(
    traits,
    src_individuals = NULL,
    ids_plot = NULL,
    skip_dates = TRUE,
    show_multiple_census = FALSE,
    remove_obs_with_issue = TRUE
) {

  cli::cli_alert_info("Using legacy wrapper - consider migrating to get_individual_aggregated_features()")

  # Map old boolean to new issues parameter
  issues <- if (isTRUE(remove_obs_with_issue)) "remove" else "ignore"

  # Use new function
  result <- get_individual_aggregated_features(
    individual_ids = src_individuals$id_n,
    trait_ids = traits,
    plot_ids = ids_plot,
    include_multi_census = show_multiple_census,
    issues = issues,
    aggregation_mode = "auto"
  )
  
  # Convert back to data.table for compatibility
  if (!data.table::is.data.table(result)) data.table::setDT(result)
  
  # Split back into old format for compatibility
  # Note: No more char_ prefix, so we need to detect by column type
  char_cols <- names(result)[sapply(result, is.character)]
  char_cols <- setdiff(char_cols, "id_data_individuals")
  num_cols <- setdiff(names(result), c("id_data_individuals", char_cols))
  
  traits_char <- if (length(char_cols) > 0) {
    result[, c("id_data_individuals", char_cols), with = FALSE]
  } else {
    NA
  }
  
  traits_num <- if (length(num_cols) > 0) {
    result[, c("id_data_individuals", num_cols), with = FALSE]
  } else {
    NA
  }
  
  return(list(
    traits_char = as_tibble(traits_char),
    traits_num = as_tibble(traits_num),
    issue_num = NA  # Issues are now filtered upstream
  ))
}

#' Query individual features with improved architecture
#' 
#' @param individual_ids Numeric vector of individual IDs
#' @param trait_ids Numeric vector of trait IDs (optional filter)
#' @param id_trait_measures Integer vector of trait measurement IDs to filter on (optional).
#'   When provided, only the specified measurements are returned.
#' @param include_multi_census Include multiple census data
#' @param format Output format: "wide" (pivot) or "long" (raw)
#' @param issues Character. How to handle flagged measurements: "remove" (default,
#'   drop flagged rows), "include" (keep rows and add issue column), or "ignore"
#'   (keep rows, no issue column).
#' @param include_metadata Include trait measurement features (only available with format="long")
#' @param include_individuals Include linked individual data
#' @param con Database connection (optional)
#' @param backbone Character. Which taxonomic backbone to use for synonym resolution
#'   when fetching linked individuals. \code{"internal"} (default) uses the internal
#'   \code{table_taxa}. \code{"wcvp"} uses WCVP via \code{wcvp_idtax_link}.
#'
#' @return Tibble with individual features in requested format
#' @export
query_individual_features <- function(
    individual_ids = NULL,
    trait_ids = NULL,
    id_trait_measures = NULL,
    include_multi_census = FALSE,
    format = c("wide", "long"),
    issues = c("remove", "include", "ignore"),
    include_metadata = FALSE,
    include_individuals = FALSE,
    census_strategy = c("last", "first", "mean"),
    con = NULL,
    backbone = c("internal", "wcvp")
) {

  format <- match.arg(format)
  census_strategy <- match.arg(census_strategy)
  issues <- match.arg(issues)
  backbone <- match.arg(backbone)
  if (is.null(con)) con <- call.mydb()
  
  # Check incompatible parameter combination
  if (include_metadata && format == "wide") {
    cli::cli_alert_warning(
      "include_metadata=TRUE is incompatible with format='wide'. Setting include_metadata=FALSE."
    )
    cli::cli_alert_info(
      "Metadata can only be included with format='long' to preserve measurement-level details."
    )
    include_metadata <- FALSE
  }
  
  # 1. Fetch raw trait measurements
  cli::cli_h2("Fetching trait measurements")
  raw_data <- fetch_trait_measurements(
    individual_ids = individual_ids,
    trait_ids = trait_ids,
    id_trait_measures = id_trait_measures,
    con = con
  )
  
  if (nrow(raw_data) == 0) {
    cli::cli_alert_warning("No trait measurements found")
    return(tibble())
  }
  
  # 2. Handle issue flagging
  if (issues == "remove") {
    n_before <- nrow(raw_data)
    raw_data <- raw_data %>% filter(is.na(issue))
    n_removed <- n_before - nrow(raw_data)
    if (n_removed > 0) {
      cli::cli_alert_info("Removed {n_removed} measurement(s) with issues")
    }
  }
  
  # 3. Handle census information and filtering
  if (include_multi_census) {
    cli::cli_alert_info("Enriching with census information")
    raw_data <- enrich_census_info(raw_data, con)
  } else if (census_strategy != "mean") {
    # Census selection: need census info even when not showing multiple censuses
    cli::cli_alert_info("Enriching with census information for {census_strategy} census selection")
    raw_data <- enrich_census_info(data = raw_data, con)

    # Filter to first or last census
    if ("census_name" %in% names(raw_data) && any(!is.na(raw_data$census_name))) {
      raw_data <- filter_to_census(data = raw_data, strategy = census_strategy)
    } else {
      cli::cli_alert_warning("No census information available; using all measurements")
    }
  }

  # 4. Optional: Add measurement metadata (only for long format)
  if (include_metadata) {
    cli::cli_alert_info("Enriching with measurement metadata")
    raw_data <- enrich_measurement_features(data = raw_data, con)

    # Remove internal aggregated ID column when metadata is included
    raw_data <- raw_data %>% select(-any_of("id_ind_meas_feat"))
    raw_data <- raw_data %>% mutate(traitvalue_char = stringr::str_squish(traitvalue_char))
  }

  # 5. Format output
  if (format == "wide") {
    cli::cli_h2("Pivoting to wide format")
    result <- pivot_traits_to_wide(
      data = raw_data,
      include_census = include_multi_census
    )
  } else {
    result <- raw_data %>% as_tibble()
  }
  
  # 6. Optional: Add individual data
  if (include_individuals && nrow(result) > 0) {
    cli::cli_alert_info("Fetching linked individuals")
    ind_data <- fetch_linked_individuals(
      individual_ids = unique(raw_data$id_data_individuals),
      con = con,
      backbone = backbone
    )
    result <- list(
      features = result,
      individuals = ind_data
    )
  }
  
  cli::cli_alert_success("Query completed: {nrow(raw_data)} measurement(s)")
  return(result)
}


#' Fetch trait measurements with automatic chunking
#' @keywords internal
fetch_trait_measurements <- function(individual_ids, trait_ids, id_trait_measures = NULL, con) {

  # Chunk if necessary
  if (!is.null(individual_ids) && length(individual_ids) > 1000) {
    result <- fetch_with_chunking(
      ids = individual_ids,
      query_fun = function(ids, traits) {
        build_trait_query(ids, traits, id_trait_measures, con)
      },
      chunk_size = 1000,
      con = con,
      desc = "individual features",
      trait_ids = trait_ids
    )
  } else {
    sql <- build_trait_query(individual_ids, trait_ids, id_trait_measures, con)
    result <- DBI::dbGetQuery(con, sql)
  }
  
  # Clean up columns
  result <- result %>%
    select(-starts_with("date_modif"), -any_of("id_diconame"))
  
  return(result)
}

#' Build appropriate SQL query based on parameters
#' @keywords internal
build_trait_query <- function(individual_ids, trait_ids, id_trait_measures = NULL, con) {

  base_query <- "
    SELECT
      tm.id_trait_measures,
      tm.id_data_individuals,
      tm.id_sub_plots,
      tm.traitvalue,
      tm.traitvalue_char,
      tm.issue,
      tm.traitid,
      tm.id_table_liste_plots,
      tm.year,
      tm.month,
      tm.day,
      tl.id_trait,
      tl.trait,
      tl.valuetype,
      tl.traitdescription,
      tl.expectedunit,
      tl.minallowedvalue,
      tl.maxallowedvalue
    FROM data_traits_measures tm
    LEFT JOIN traitlist tl ON tm.traitid = tl.id_trait
    WHERE 1=1
  "
  
  conditions <- character()
  
  if (!is.null(individual_ids)) {
    conditions <- c(conditions, 
                    glue::glue_sql("tm.id_data_individuals IN ({individual_ids*})", 
                                   individual_ids = individual_ids, .con = con)
    )
  }
  
  if (!is.null(trait_ids)) {
    conditions <- c(conditions,
                    glue::glue_sql("tl.id_trait IN ({trait_ids*})",
                                   trait_ids = trait_ids, .con = con)
    )
  }

  if (!is.null(id_trait_measures)) {
    conditions <- c(conditions,
                    glue::glue_sql("tm.id_trait_measures IN ({id_trait_measures*})",
                                   id_trait_measures = id_trait_measures, .con = con)
    )
  }

  if (length(conditions) > 0) {
    base_query <- paste(base_query, "AND", paste(conditions, collapse = " AND "))
  }
  
  return(base_query)
}

#' Generic chunking function for large queries
#' @keywords internal
fetch_with_chunking <- function(ids, query_fun, chunk_size, con, desc = "data", trait_ids = NULL) {

  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  n_chunks <- length(chunks)

  cli::cli_alert_info("Processing {n_chunks} chunk(s) for {desc}")

  pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)

  results <- lapply(seq_along(chunks), function(i) {
    setTxtProgressBar(pb, i)
    sql <- query_fun(chunks[[i]], trait_ids)
    DBI::dbGetQuery(con, sql) %>% as_tibble(.name_repair = "minimal")
  })

  close(pb)

  suppressMessages(bind_rows(results))
}


#' Enrich data with census information
#' @keywords internal
enrich_census_info <- function(data, con) {
  
  subplot_ids <- unique(data$id_sub_plots)
  subplot_ids <- subplot_ids[!is.na(subplot_ids)]
  
  if (length(subplot_ids) == 0) return(data)
  
  census_info <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT
      sp.id_sub_plots,
      sp.id_table_liste_plots,
      sp.typevalue,
      sp.day,
      sp.month,
      sp.year,
      spt.type,
      CONCAT(spt.type, '_', sp.typevalue) as census_name
    FROM data_liste_sub_plots sp
    LEFT JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
    WHERE sp.id_sub_plots IN ({subplot_ids*})
  ", subplot_ids = subplot_ids, .con = con))

  data %>%
    left_join(
      census_info %>%
        select(
          id_sub_plots,
          .subplot_plot_id = id_table_liste_plots,
          census_name,
          census_typevalue = typevalue,
          census_day  = day,
          census_month = month,
          census_year  = year
        ),
      by = "id_sub_plots"
    ) %>%
    # Pre-existing rows in data_traits_measures may have id_table_liste_plots = NULL
    # (inserted before the column was systematically populated). Fill from the
    # subplot table, which always carries the correct plot foreign key.
    dplyr::mutate(
      id_table_liste_plots = dplyr::coalesce(id_table_liste_plots, .subplot_plot_id)
    ) %>%
    dplyr::select(-.subplot_plot_id)
}

#' Filter data to first or last census
#' @keywords internal
filter_to_census <- function(data, strategy = c("first", "last")) {

  strategy <- match.arg(strategy)

  # Need census_name from enrich_census_info
  if (!"census_name" %in% names(data)) {
    cli::cli_alert_warning("No census information in data")
    return(data)
  }

  # Split data: census-linked vs non-census features
  data_with_census <- data %>% filter(!is.na(census_name), grepl("^census_", census_name))
  data_no_census <- data %>% filter(is.na(census_name) | !grepl("^census_", census_name))

  if (nrow(data_with_census) == 0) {
    cli::cli_alert_info("No census-linked measurements to filter")
    return(data)
  }

  # Determine first/last census per plot using proper date computation
  census_selection <- data_with_census %>%
    mutate(
      day_clean = coalesce(census_day, 1),
      month_clean = coalesce(census_month, 1),
      year_clean = coalesce(census_year, NA_integer_),
      census_date = suppressWarnings(lubridate::dmy(paste(day_clean, month_clean, year_clean, sep = "-"))),
      typevalue_num = as.numeric(census_typevalue)
    ) %>%
    # Use typevalue as fallback if date is missing
    mutate(
      sort_key = if_else(is.na(census_date), as.Date(paste0(typevalue_num, "-01-01")), census_date)
    ) %>%
    group_by(id_table_liste_plots) %>%
    {
      if (strategy == "first") {
        slice_min(., sort_key, n = 1, with_ties = TRUE) %>%
          slice_min(typevalue_num, n = 1, with_ties = FALSE)
      } else {
        slice_max(., sort_key, n = 1, with_ties = TRUE) %>%
          slice_max(typevalue_num, n = 1, with_ties = FALSE)
      }
    } %>%
    ungroup() %>%
    distinct(id_table_liste_plots, census_name_selected = census_name)

  # Filter to selected census
  data_filtered <- data_with_census %>%
    left_join(census_selection, by = "id_table_liste_plots") %>%
    filter(census_name == census_name_selected) %>%
    select(-census_name_selected)

  n_removed <- nrow(data_with_census) - nrow(data_filtered)
  n_plots <- length(unique(census_selection$id_table_liste_plots))

  cli::cli_alert_success("Selected {strategy} census for {n_plots} plot(s)")
  if (n_removed > 0) {
    cli::cli_alert_info("Filtered out {n_removed} measurement(s) from other censuses")
  }

  # Combine filtered census data with non-census data
  bind_rows(data_filtered, data_no_census)
}



#' Enrich data with measurement-level features/metadata
#' @keywords internal
enrich_measurement_features <- function(data, con, src = "individuals") {
  
  measure_ids <- unique(data$id_trait_measures)
  
  if (length(measure_ids) == 0) return(data)
  
  # Récupérer les features liées aux mesures
  features <- query_traits_measures_features(
    id_trait_measures = measure_ids,
    src = src,
    con = con
  )
  
  if (is.null(features) || nrow(features) == 0) {
    return(data)
  }
  
  # Agréger les features par mesure
  id_col <- if (src == "individuals") "id_ind_meas_feat" else "id_taxa_trait_feat"

  # Remove trait metadata columns to avoid duplicates (already in main data)
  # Keep only measurement-specific features
  features_to_agg <- features %>%
    select(-any_of(c("id_trait", "trait", "valuetype", "traitdescription")))

  features_agg <- features_to_agg %>%
    mutate(!!sym(id_col) := as.character(!!sym(id_col))) %>%
    group_by(id_trait_measures) %>%
    summarise(
      across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
      across(where(is.character), ~paste(.x[!is.na(.x)], collapse = "|"))
    ) %>%
    mutate(across(where(is.character), ~na_if(.x, "")))

  data %>%
    left_join(features_agg, by = "id_trait_measures")
}


#' Query features associated with trait measurements
#'
#' Retrieves and aggregates features (metadata) linked to specific trait measurements.
#' Handles numeric, character, ordinal, and table-referenced value types.
#'
#' @param id_trait_measures Integer vector of trait measurement IDs
#' @param id_trait Integer vector of trait IDs to filter features (optional).
#'   When provided, only features linked to the specified trait(s) are returned.
#' @param src Source type: "individuals" or "taxa"
#' @param format Output format: "wide" (pivoted) or "long" (raw)
#' @param con Database connection (optional)
#'
#' @return Tibble with measurement features in requested format
#' @export
query_traits_measures_features <- function(
    id_trait_measures = NULL,
    id_trait = NULL,
    src = c("individuals", "taxa"), # src = "taxa" is not yet operational because id_taxa_trait_feat does not exist
    format = c("wide", "long"),
    con = NULL
) {
  
  src <- match.arg(src)
  format <- match.arg(format)

  # Get appropriate connection and table names
  config <- get_measurement_features_config(src)

  # Get connection if not provided
  if (is.null(con)) {
    con <- call.mydb()
  }
  
  # Quick count check - skip for very large queries to avoid timeout
  if (!is.null(id_trait_measures) && length(id_trait_measures) > 10000) {
    cli::cli_alert_info("Large query detected ({length(id_trait_measures)} trait measurements)")
    cli::cli_alert_info("Skipping count to avoid timeout - proceeding with fetch")
    n_records <- NA
  } else {
    n_records <- count_measurement_features(
      id_trait_measures = id_trait_measures,
      id_trait = id_trait,
      config = config,
      con = con
    )

    if (n_records == 0) {
      cli::cli_alert_info("No measurement features found")
      return(tibble())
    }

    cli::cli_alert_info("Found {n_records} measurement feature(s)")
  }

  # Fetch raw data with chunking for large queries
  if (!is.null(id_trait_measures) && length(id_trait_measures) > 15000) {
    cli::cli_alert_info("Large query detected - using chunked fetching")

    # Chunk the IDs
    chunks <- split(id_trait_measures, ceiling(seq_along(id_trait_measures) / 15000))
    n_chunks <- length(chunks)

    cli::cli_alert_info("Processing {n_chunks} chunk(s) for measurement features")

    # Fetch each chunk
    results <- lapply(seq_along(chunks), function(i) {
      cli::cli_alert_info("Fetching chunk {i}/{n_chunks}...")
      fetch_measurement_features_raw(
        id_trait_measures = chunks[[i]],
        id_trait = id_trait,
        config = config,
        con = con
      )
    })

    # Combine results
    raw_data <- bind_rows(results)
    cli::cli_alert_success("Fetched all {nrow(raw_data)} measurement feature(s)")

  } else {
    # Small query - fetch all at once
    raw_data <- fetch_measurement_features_raw(
      id_trait_measures = id_trait_measures,
      id_trait = id_trait,
      config = config,
      con = con
    )
  }
  
  # Return long format if requested
  if (format == "long") {
    return(raw_data)
  }
  
  # Pivot to wide format
  pivoted <- pivot_measurement_features(data = raw_data, config, con = con)

  return(pivoted)
}

#' Get configuration for measurement features
#' @keywords internal
get_measurement_features_config <- function(src) {

  if (src == "individuals") {
    list(
      table_name = "data_ind_measures_feat",
      trait_table = "traitlist",
      id_col = "id_ind_meas_feat"
    )
  } else if (src == "taxa") {
    list(
      table_name = "taxa_traits_measures_feat",
      trait_table = "traitlist",
      id_col = "id_taxa_trait_feat"
    )
  } else {
    stop("Unknown src: ", src)
  }
}

#' Count measurement features
#' @keywords internal
count_measurement_features <- function(id_trait_measures, id_trait = NULL, config, con) {

  id_measures_filter <- if (!is.null(id_trait_measures)) {
    glue::glue_sql("AND id_trait_measures IN ({id_trait_measures*})",
                   id_trait_measures = id_trait_measures, .con = con)
  } else {
    DBI::SQL("")
  }

  id_trait_filter <- if (!is.null(id_trait)) {
    glue::glue_sql("AND id_trait IN ({id_trait*})", id_trait = id_trait, .con = con)
  } else {
    DBI::SQL("")
  }

  query <- glue::glue_sql("
    SELECT COUNT(*) AS n
    FROM {`config$table_name`}
    WHERE 1=1
    {id_measures_filter}
    {id_trait_filter}
  ", .con = con)

  result <- func_try_fetch(con = con, sql = query, verbose = FALSE)
  return(result$n)
}

#' Fetch raw measurement features data
#' @keywords internal
fetch_measurement_features_raw <- function(id_trait_measures, id_trait = NULL, config, con) {

  # Get trait metadata with retry logic
  trait_meta_query <- as.character(glue::glue_sql("
    SELECT id_trait, trait, valuetype, traitdescription
    FROM {`config$trait_table`}
  ", .con = con))

  trait_meta <- func_try_fetch(con = con, sql = trait_meta_query, verbose = FALSE)

  # Build feature query with explicit column name
  # Convert config$id_col to SQL identifier
  id_col_sql <- DBI::dbQuoteIdentifier(con, config$id_col)
  table_name_sql <- DBI::dbQuoteIdentifier(con, config$table_name)

  id_measures_filter <- if (!is.null(id_trait_measures)) {
    glue::glue_sql("AND id_trait_measures IN ({id_trait_measures*})",
                   id_trait_measures = id_trait_measures, .con = con)
  } else {
    DBI::SQL("")
  }

  id_trait_filter <- if (!is.null(id_trait)) {
    glue::glue_sql("AND id_trait IN ({id_trait*})", id_trait = id_trait, .con = con)
  } else {
    DBI::SQL("")
  }

  feat_query <- glue::glue_sql("
    SELECT
      id_trait_measures,
      id_trait,
      {id_col_sql},
      typevalue,
      typevalue_char
    FROM {table_name_sql}
    WHERE 1=1
    {id_measures_filter}
    {id_trait_filter}
  ", .con = con)

  feat_data <- func_try_fetch(con = con, sql = as.character(feat_query), verbose = FALSE)

  # Check if id_col exists in feat_data
  if (!config$id_col %in% names(feat_data)) {
    cli::cli_alert_warning("Column {config$id_col} not found in feature data. Available columns: {paste(names(feat_data), collapse=', ')}")
    # Try to continue without it
    result <- feat_data %>%
      distinct() %>%
      left_join(trait_meta, by = "id_trait")
    return(result)
  }

  # Join with metadata and convert id column to character
  result <- feat_data %>%
    distinct() %>%
    left_join(trait_meta, by = "id_trait") %>%
    mutate(!!sym(config$id_col) := as.character(!!sym(config$id_col)))

  return(result)
}

#' Pivot measurement features to wide format
#' @keywords internal
pivot_measurement_features <- function(data, config, con) {

  if (nrow(data) == 0) return(tibble())

  # Aggregate id_col separately (keep all unique IDs per id_trait_measures)
  id_col_aggregated <- data %>%
    select(id_trait_measures, !!sym(config$id_col)) %>%
    distinct() %>%
    group_by(id_trait_measures) %>%
    summarise(
      !!sym(config$id_col) := paste(unique(!!sym(config$id_col)), collapse = "|"),
      .groups = "drop"
    )

  # Separate by valuetype
  valuetypes <- unique(data$valuetype)

  pivoted_list <- list()

  # Handle character traits
  if (any(valuetypes == "character")) {
    pivoted_list$character <- pivot_features_by_type(
      data = data %>% filter(valuetype == "character"),
      value_col = "typevalue_char",
      id_col = config$id_col,
      agg_fun = function(x) paste(na.omit(unique(x)), collapse = "|")
    )
  }

  # Handle numeric traits
  if (any(valuetypes == "numeric")) {
    pivoted_list$numeric <- pivot_features_by_type(
      data = data %>% filter(valuetype == "numeric"),
      value_col = "typevalue",
      id_col = config$id_col,
      agg_fun = function(x) mean(as.numeric(x), na.rm = TRUE)
    )
  }

  # Handle ordinal traits
  if (any(valuetypes == "ordinal")) {
    pivoted_list$ordinal <- pivot_features_by_type(
      data = data %>% filter(valuetype == "ordinal"),
      value_col = "typevalue_char",
      id_col = config$id_col,
      agg_fun = function(x) paste(na.omit(unique(x)), collapse = "|")
    )
  }

  # Handle table_* valuetypes (references to other tables)
  table_types <- valuetypes[grepl("^table_", valuetypes)]
  if (length(table_types) > 0) {
    pivoted_list$tables <- pivot_table_references(
      data = data %>% filter(valuetype %in% table_types),
      id_col = config$id_col,
      con = con
    )
  }

  # Combine all pivoted data
  if (length(pivoted_list) == 0) {
    return(tibble())
  }

  combined <- bind_rows(pivoted_list, .id = NULL)

  # Ensure one row per id_trait_measures by grouping and collapsing
  # This handles cases where bind_rows created multiple rows for same id_trait_measures
  if (nrow(combined) > 0 && "id_trait_measures" %in% names(combined)) {
    combined <- combined %>%
      group_by(id_trait_measures) %>%
      summarise(across(everything(), ~{
        # For each column, take first non-NA value or collapse unique values
        vals <- na.omit(unique(.))
        if (length(vals) == 0) return(NA)
        if (length(vals) == 1) return(vals)
        # Multiple values - collapse with separator
        paste(vals, collapse = "|")
      }), .groups = "drop")
  }

  # Add back the aggregated id_col
  combined <- combined %>%
    left_join(id_col_aggregated, by = "id_trait_measures")

  return(combined)
}

#' Pivot features by type with custom aggregation
#' @keywords internal
pivot_features_by_type <- function(data, value_col, id_col, agg_fun) {

  if (nrow(data) == 0) return(NULL)

  # Convert to data.table for efficient pivoting
  dt <- data.table::as.data.table(data)

  # Create formula for dcast - only use id_trait_measures to avoid duplicates
  # Multiple feature records for same id_trait_measures will be aggregated by agg_fun
  formula <- as.formula("id_trait_measures ~ trait")

  # Pivot
  pivoted <- data.table::dcast(
    dt,
    formula = formula,
    value.var = value_col,
    fun.aggregate = agg_fun
  )

  # Prefix measurement feature columns to avoid conflicts with main trait columns
  # Keep id_trait_measures unchanged
  result <- as_tibble(pivoted)
  names(result)[-1] <- paste0("meas_feat_", names(result)[-1])

  return(result)
}

#' Pivot table-referenced features
#' @keywords internal
pivot_table_references <- function(data, id_col, con) {

  results <- list()

  for (vt in unique(data$valuetype)) {

    # Get lookup table info
    lookup_info <- get_lookup_table_info(vt)

    if (is.null(lookup_info)) {
      cli::cli_alert_warning("Unknown table valuetype: {vt}")
      next
    }

    # Get lookup values with retry logic
    lookup_query <- glue::glue_sql("
      SELECT {`lookup_info$id_col`}, {`lookup_info$value_col`}
      FROM {`vt`}
    ", .con = con)

    lookup_values <- func_try_fetch(con = con, sql = lookup_query, verbose = FALSE)

    # Join and pivot
    tmp <- data %>%
      filter(valuetype == vt) %>%
      left_join(
        lookup_values,
        by = setNames(lookup_info$id_col, "typevalue")
      ) %>%
      mutate(typevalue_char = !!sym(lookup_info$value_col)) %>%
      select(id_trait_measures, trait, typevalue_char)  # Remove id_col to avoid duplicates

    if (nrow(tmp) > 0) {
      results[[vt]] <- tmp %>%
        pivot_wider(
          id_cols = id_trait_measures,  # Explicitly specify ID column
          names_from = trait,
          values_from = typevalue_char,
          values_fn = ~paste(unique(.), collapse = "|")  # Use unique to handle duplicates
        )
    }
  }
  
  if (length(results) > 0) {
    return(bind_rows(results))
  }
  
  return(NULL)
}

#' Get lookup table information
#' @keywords internal
get_lookup_table_info <- function(table_name) {
  
  lookup_config <- list(
    table_colnam = list(
      id_col = "id_table_colnam",
      value_col = "colnam"
    )
    # Ajouter d'autres tables de référence ici si nécessaire
  )
  
  lookup_config[[table_name]]
}

#' Fetch linked individual data
#' @keywords internal
fetch_linked_individuals <- function(individual_ids, con, chunk_size = 30000,
                                     backbone = c("internal", "wcvp")) {
  backbone <- match.arg(backbone)
  
  if (length(individual_ids) == 0) {
    cli::cli_alert_warning("No individual IDs provided")
    return(tibble())
  }
  
  # If small number of IDs, fetch directly
  if (length(individual_ids) <= chunk_size) {
    return(merge_individuals_taxa(id_individual = individual_ids, backbone = backbone))
  }

  # For large number of IDs, use chunking
  chunks <- split(individual_ids, ceiling(seq_along(individual_ids) / chunk_size))
  n_chunks <- length(chunks)

  cli::cli_alert_info("Processing {n_chunks} chunk(s) for linked individuals")

  pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)

  results <- lapply(seq_along(chunks), function(i) {
    setTxtProgressBar(pb, i)

    # merge_individuals_taxa returns a tibble directly
    chunk_result <- merge_individuals_taxa(id_individual = chunks[[i]], backbone = backbone)
    
    return(chunk_result)
  })
  
  close(pb)
  
  # Combine all chunks
  combined <- bind_rows(results)
  
  cli::cli_alert_success("Fetched {nrow(combined)} individual record(s)")
  
  return(combined)
}


#' Pivot trait measurements to wide format
#' @keywords internal
pivot_traits_to_wide <- function(data, include_census = FALSE) {
  
  # Séparer par type de valeur
  numeric_data <- data %>% filter(valuetype == "numeric")
  char_data <- data %>% filter(valuetype %in% c("character", "ordinal", "categorical"))
  
  results <- list()
  
  # Pivot numeric traits
  if (nrow(numeric_data) > 0) {
    results$numeric <- pivot_numeric_traits(numeric_data, include_census)
  }
  
  # Pivot character traits
  if (nrow(char_data) > 0) {
    results$character <- pivot_character_traits(char_data, include_census)
  }
  
  # Combiner les résultats
  if (length(results) == 0) {
    return(tibble())
  }
  
  # Join sur id_data_individuals (et id_sub_plots si multi-census)
  join_cols <- "id_data_individuals"
  if (include_census) {
    join_cols <- c(join_cols, "id_sub_plots")
  }
  
  combined <- 
    reduce(results[which(unlist(lapply(results, nrow)) > 0)], full_join, by = join_cols)
  
  return(combined)
}

#' Pivot numeric traits
#' @keywords internal
pivot_numeric_traits <- function(data, include_census) {

  if (include_census) {
    # Keep separate rows for each subplot/census
    # Only process data with subplot info
    data_with_subplot <- data %>% filter(!is.na(id_sub_plots))

    if (nrow(data_with_subplot) == 0) {
      return(tibble())
    }

    result <- data_with_subplot %>%
      select(id_data_individuals, id_sub_plots, trait,
             traitvalue, id_trait_measures, census_name) %>%
      group_by(id_data_individuals, id_sub_plots, trait, census_name) %>%
      summarise(
        value = first(traitvalue),
        id_measure = first(id_trait_measures),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = c(trait, census_name),
        values_from = c(value, id_measure),
        names_glue = "{.value}_{trait}_{census_name}"
      ) %>%
      rename_with(~stringr::str_remove(.x, "value_"), starts_with("value_")) %>%
      select(-starts_with("id_measure_")) %>%
      distinct()

    return(result)

  } else {
    # Aggregate by individual only - COMBINE all data first to avoid duplicates
    result <- data %>%
      select(id_data_individuals, trait, traitvalue, id_trait_measures) %>%
      group_by(id_data_individuals, trait) %>%
      summarise(
        value = first(traitvalue),
        id_measure = first(id_trait_measures),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = trait,
        values_from = c(value, id_measure),
        names_glue = "{.value}_{trait}"
      ) %>%
      rename_with(~stringr::str_remove(.x, "value_"), starts_with("value_")) %>%
      select(-starts_with("id_measure_")) %>%
      distinct()

    return(result)
  }
}

#' Pivot character traits
#' @keywords internal
pivot_character_traits <- function(data, include_census) {

  # Utiliser traitvalue_char pour les valeurs textuelles
  data <- data %>%
    mutate(traitvalue_char = str_squish(traitvalue_char))

  if (include_census) {
    # Keep separate rows for each subplot/census
    # Only process data with subplot info
    data_with_subplot <- data %>% filter(!is.na(id_sub_plots))

    if (nrow(data_with_subplot) == 0) {
      return(tibble())
    }

    result <- data_with_subplot %>%
      select(id_data_individuals, id_sub_plots, trait,
             traitvalue_char, id_trait_measures, census_name) %>%
      group_by(id_data_individuals, id_sub_plots, trait, census_name) %>%
      summarise(
        value = paste(traitvalue_char[!is.na(traitvalue_char)], collapse = ", "),
        id_measure = first(id_trait_measures),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = c(trait, census_name),
        values_from = value,
        names_glue = "char_{trait}_{census_name}"
      ) %>%
      mutate(across(starts_with("char_"), ~na_if(.x, ""))) %>%
      distinct()

    return(result)

  } else {
    # Aggregate by individual only - COMBINE all data first to avoid duplicates
    result <- data %>%
      select(id_data_individuals, trait, traitvalue_char) %>%
      group_by(id_data_individuals, trait) %>%
      summarise(
        value = paste(traitvalue_char[!is.na(traitvalue_char)], collapse = ", "),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = trait,
        values_from = value,
        names_prefix = "char_"
      ) %>%
      mutate(across(starts_with("char_"), ~na_if(.x, ""))) %>%
      distinct()

    return(result)
  }
}


#' List all available individual features
#' @export
list_individual_features <- function(con = NULL) {
  
  if (is.null(con)) con <- call.mydb()
  
  features <- DBI::dbGetQuery(con, "
    SELECT DISTINCT
      tl.trait,
      tl.valuetype,
      tl.description,
      tl.unit,
      tl.category,
      COUNT(DISTINCT tm.id_data_individuals) as n_individuals,
      COUNT(*) as n_measurements
    FROM traitlist tl
    LEFT JOIN data_traits_measures tm ON tl.id_trait = tm.traitid
    GROUP BY tl.trait, tl.valuetype, tl.description, tl.unit, tl.category
    ORDER BY tl.category, n_measurements DESC
  ")
  
  return(features)
}

#' Get summary statistics for a specific feature
#' @export
summarize_feature <- function(trait_name, con = NULL) {
  
  if (is.null(con)) con <- call.mydb()
  
  DBI::dbGetQuery(con, glue::glue_sql("
    SELECT 
      tl.trait,
      tl.valuetype,
      COUNT(DISTINCT tm.id_data_individuals) as n_individuals,
      COUNT(*) as n_measurements,
      COUNT(DISTINCT tm.id_sub_plots) as n_census,
      MIN(tm.date_measure) as first_measure,
      MAX(tm.date_measure) as last_measure
    FROM traitlist tl
    LEFT JOIN data_traits_measures tm ON tl.id_trait = tm.traitid
    WHERE tl.trait = {trait_name}
    GROUP BY tl.trait, tl.valuetype
  ", trait_name = trait_name, .con = con))
}


#' Delete an entry in individual feature table
#'
#' Delete an entry in individual/trait feature table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_individual_feature_type <- function(id) {

  mydb <- call.mydb()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_traits_measures WHERE id_trait_measures=$1", params=list(id)
  # )

  query <- "DELETE FROM traitlist WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_trait IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
}




#' List of trait and features potentially liked to individual
#'
#' Provide list of traits and features available
#'
#' @return A tibble of all traits and features that can be linked to individual
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
traits_list <- function(id_trait = NULL) {

  mydb <- call.mydb()

  cols <- c("trait", "id_trait", "traitdescription",
            "maxallowedvalue", "minallowedvalue", "expectedunit",
            "valuetype", "category", "factorlevels")

  all_colnames_ind <- get_traitlist(mydb)
  all_colnames_ind <- all_colnames_ind[, intersect(cols, names(all_colnames_ind)), drop = FALSE]

  if (!is.null(id_trait)) {
    all_colnames_ind <- all_colnames_ind[all_colnames_ind$id_trait == id_trait, , drop = FALSE]
  }

  return(as_tibble(all_colnames_ind))
}

