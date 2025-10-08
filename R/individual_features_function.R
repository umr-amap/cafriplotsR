
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
#' @param remove_issues Remove measurements flagged with issues
#' @param aggregation_mode How to aggregate: "mean", "last", "mode", "concat"
#' @param con Database connection
#'
#' @return Tibble with one row per individual and aggregated feature values
#' @export
get_individual_aggregated_features <- function(
    individual_ids = NULL,
    trait_ids = NULL,
    plot_ids = NULL,
    include_multi_census = FALSE,
    remove_issues = TRUE,
    aggregation_mode = c("auto", "mean", "last", "mode", "concat"),
    con = NULL
) {
  
  aggregation_mode <- match.arg(aggregation_mode)
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
    remove_issues = remove_issues,
    con = con
  )
  
  if (nrow(raw_data) == 0) {
    cli::cli_alert_warning("No features found")
    return(tibble(id_data_individuals = integer()))
  }
  
  # 2. Separate by value type and aggregate with data.table
  cli::cli_h2("Aggregating features by individual")
  
  numeric_features <- aggregate_numeric_features_dt(
    data = raw_data %>% filter(valuetype == "numeric"),
    include_census = include_multi_census,
    mode = aggregation_mode
  )
  
  character_features <- aggregate_character_features_dt(
    data = raw_data %>% filter(valuetype %in% c("character", "ordinal", "categorical")),
    include_census = include_multi_census,
    mode = aggregation_mode
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
aggregate_numeric_features_dt <- function(data, include_census, mode) {
  
  if (nrow(data) == 0) return(NULL)
  
  cli::cli_alert_info("Aggregating {nrow(data)} numeric measurement(s)")
  
  # Convert to data.table
  if (!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Convert traitvalue to numeric
  data[, traitvalue_num := as.numeric(traitvalue)]
  
  if (include_census && "census_name" %in% names(data) && any(!is.na(data$census_name))) {
    
    # With census: keep census as separate columns
    # Aggregate by (individual, subplot, trait, census)
    dt_agg <- data[!is.na(census_name), 
                   .(value = mean(traitvalue_num, na.rm = TRUE)),
                   by = .(id_data_individuals, id_sub_plots, trait, census_name)]
    
    # Further aggregate by (individual, trait, census) - mean across subplots
    dt_ind <- dt_agg[, .(value = mean(value, na.rm = TRUE)),
                     by = .(id_data_individuals, trait, census_name)]
    
    # Pivot to wide format with trait_census columns
    result <- data.table::dcast(
      dt_ind,
      id_data_individuals ~ trait + census_name,
      value.var = "value",
      sep = "_"
    )
    
  } else {
    
    # Without census: aggregate by (individual, trait)
    if ("id_sub_plots" %in% names(data)) {
      # Aggregate by subplot first, then by individual
      dt_subplot <- data[, .(value = mean(traitvalue_num, na.rm = TRUE)),
                         by = .(id_data_individuals, id_sub_plots, trait)]
      
      dt_ind <- dt_subplot[, .(value = mean(value, na.rm = TRUE)),
                           by = .(id_data_individuals, trait)]
    } else {
      # Direct aggregation
      dt_ind <- data[, .(value = mean(traitvalue_num, na.rm = TRUE)),
                     by = .(id_data_individuals, trait)]
    }
    
    # Pivot to wide format
    result <- data.table::dcast(
      dt_ind,
      id_data_individuals ~ trait,
      value.var = "value"
    )
  }
  
  return(result)
}

#' Aggregate character features using data.table
#' @keywords internal
aggregate_character_features_dt <- function(data, include_census, mode) {
  
  if (nrow(data) == 0) return(NULL)
  
  cli::cli_alert_info("Aggregating {nrow(data)} character measurement(s)")
  
  # Convert to data.table
  if (!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Clean text values
  data[, traitvalue_char := str_squish(traitvalue_char)]
  
  if (include_census && "census_name" %in% names(data) && any(!is.na(data$census_name))) {
    
    # With census: keep census as separate columns
    # Aggregate by (individual, subplot, trait, census)
    if (mode == "mode") {
      dt_agg <- data[!is.na(traitvalue_char) & !is.na(census_name), 
                     .(value = get_mode_dt(traitvalue_char)),
                     by = .(id_data_individuals, id_sub_plots, trait, census_name)]
    } else if (mode == "last") {
      dt_agg <- data[!is.na(traitvalue_char) & !is.na(census_name),
                     .(value = last(traitvalue_char)),
                     by = .(id_data_individuals, id_sub_plots, trait, census_name)]
    } else {
      # concat or auto
      dt_agg <- data[!is.na(traitvalue_char) & !is.na(census_name),
                     .(value = paste(unique(traitvalue_char), collapse = ", ")),
                     by = .(id_data_individuals, id_sub_plots, trait, census_name)]
    }
    
    # Further aggregate by (individual, trait, census) across subplots
    if (mode == "mode") {
      dt_ind <- dt_agg[!is.na(value),
                       .(value = get_mode_dt(value)),
                       by = .(id_data_individuals, trait, census_name)]
    } else if (mode == "last") {
      dt_ind <- dt_agg[!is.na(value),
                       .(value = last(value)),
                       by = .(id_data_individuals, trait, census_name)]
    } else {
      dt_ind <- dt_agg[!is.na(value),
                       .(value = paste(unique(value), collapse = ", ")),
                       by = .(id_data_individuals, trait, census_name)]
    }
    
    # Pivot to wide with trait_census columns
    result <- data.table::dcast(
      dt_ind,
      id_data_individuals ~ trait + census_name,
      value.var = "value",
      sep = "_"
    )
    
  } else {
    
    # Without census: aggregate by (individual, trait)
    if ("id_sub_plots" %in% names(data)) {
      # Aggregate by subplot first
      if (mode == "mode") {
        dt_subplot <- data[!is.na(traitvalue_char),
                           .(value = get_mode_dt(traitvalue_char)),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        
        dt_ind <- dt_subplot[!is.na(value),
                             .(value = get_mode_dt(value)),
                             by = .(id_data_individuals, trait)]
      } else if (mode == "last") {
        dt_subplot <- data[!is.na(traitvalue_char),
                           .(value = last(traitvalue_char)),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        
        dt_ind <- dt_subplot[!is.na(value),
                             .(value = last(value)),
                             by = .(id_data_individuals, trait)]
      } else {
        dt_subplot <- data[!is.na(traitvalue_char),
                           .(value = paste(unique(traitvalue_char), collapse = ", ")),
                           by = .(id_data_individuals, id_sub_plots, trait)]
        
        dt_ind <- dt_subplot[!is.na(value),
                             .(value = paste(unique(value), collapse = ", ")),
                             by = .(id_data_individuals, trait)]
      }
    } else {
      # Direct aggregation
      if (mode == "mode") {
        dt_ind <- data[!is.na(traitvalue_char),
                       .(value = get_mode_dt(traitvalue_char)),
                       by = .(id_data_individuals, trait)]
      } else if (mode == "last") {
        dt_ind <- data[!is.na(traitvalue_char),
                       .(value = last(traitvalue_char)),
                       by = .(id_data_individuals, trait)]
      } else {
        dt_ind <- data[!is.na(traitvalue_char),
                       .(value = paste(unique(traitvalue_char), collapse = ", ")),
                       by = .(id_data_individuals, trait)]
      }
    }
    
    # Pivot to wide (NO PREFIX)
    result <- data.table::dcast(
      dt_ind,
      id_data_individuals ~ trait,
      value.var = "value"
    )
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
  
  # Use new function
  result <- get_individual_aggregated_features(
    individual_ids = src_individuals$id_n,
    trait_ids = traits,
    plot_ids = ids_plot,
    include_multi_census = show_multiple_census,
    remove_issues = remove_obs_with_issue,
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
#' @param include_multi_census Include multiple census data
#' @param format Output format: "wide" (pivot) or "long" (raw)
#' @param remove_issues Remove observations flagged with issues
#' @param include_metadata Include trait measurement features (only available with format="long")
#' @param include_individuals Include linked individual data
#' @param con Database connection (optional)
#' 
#' @return Tibble with individual features in requested format
#' @export
query_individual_features <- function(
    individual_ids = NULL,
    trait_ids = NULL,
    include_multi_census = FALSE,
    format = c("wide", "long"),
    remove_issues = TRUE,
    include_metadata = FALSE,
    include_individuals = FALSE,
    con = NULL
) {
  
  format <- match.arg(format)
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
    con = con
  )
  
  if (nrow(raw_data) == 0) {
    cli::cli_alert_warning("No trait measurements found")
    return(tibble())
  }
  
  # 2. Optional: Remove issues
  if (remove_issues) {
    n_before <- nrow(raw_data)
    raw_data <- raw_data %>% filter(is.na(issue))
    n_removed <- n_before - nrow(raw_data)
    if (n_removed > 0) {
      cli::cli_alert_info("Removed {n_removed} measurement(s) with issues")
    }
  }
  
  # 3. Optional: Add census information
  if (include_multi_census) {
    cli::cli_alert_info("Enriching with census information")
    raw_data <- enrich_census_info(raw_data, con)
  }
  
  # 4. Optional: Add measurement metadata (only for long format)
  if (include_metadata) {
    cli::cli_alert_info("Enriching with measurement metadata")
    raw_data <- enrich_measurement_features(data = raw_data, con)
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
      con = con
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
fetch_trait_measurements <- function(individual_ids, trait_ids, con) {
  
  # Chunk if necessary
  if (!is.null(individual_ids) && length(individual_ids) > 1000) {
    result <- fetch_with_chunking(
      ids = individual_ids,
      query_fun = function(ids, traits) {
        build_trait_query(ids, traits, con)
      },
      chunk_size = 1000,
      con = con,
      desc = "individual features"
    )
  } else {
    sql <- build_trait_query(individual_ids, trait_ids, con)
    result <- DBI::dbGetQuery(con, sql)
  }
  
  # Clean up columns
  result <- result %>%
    select(-starts_with("date_modif"), -any_of("id_diconame"))
  
  return(result)
}

#' Build appropriate SQL query based on parameters
#' @keywords internal
build_trait_query <- function(individual_ids, trait_ids, con) {
  
  base_query <- "
    SELECT tm.*, tl.*
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
  
  if (length(conditions) > 0) {
    base_query <- paste(base_query, "AND", paste(conditions, collapse = " AND "))
  }
  
  return(base_query)
}

#' Generic chunking function for large queries
#' @keywords internal
fetch_with_chunking <- function(ids, query_fun, chunk_size, con, desc = "data") {
  
  chunks <- split(ids, ceiling(seq_along(ids) / chunk_size))
  n_chunks <- length(chunks)
  
  cli::cli_alert_info("Processing {n_chunks} chunk(s) for {desc}")
  
  pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)
  
  results <- lapply(seq_along(chunks), function(i) {
    setTxtProgressBar(pb, i)
    sql <- query_fun(chunks[[i]], NULL)
    DBI::dbGetQuery(con, sql)
  })
  
  close(pb)
  
  bind_rows(results)
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
      census_info %>% select(id_sub_plots, census_name),
      by = "id_sub_plots"
    )
}

#' Enrich data with measurement-level features/metadata
#' @keywords internal
enrich_measurement_features <- function(data, con, src = "individuals") {
  
  measure_ids <- unique(data$id_trait_measures)
  
  if (length(measure_ids) == 0) return(data)
  
  # Récupérer les features liées aux mesures
  features <- query_traits_measures_features(
    id_trait_measures = measure_ids,
    src = src
  )
  
  if (is.null(features) || nrow(features) == 0) {
    return(data)
  }
  
  # Agréger les features par mesure
  id_col <- if (src == "individuals") "id_ind_meas_feat" else "id_taxa_trait_feat"
  
  features_agg <- features %>%
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
#' @param src Source type: "individuals" or "taxa"
#' @param format Output format: "wide" (pivoted) or "long" (raw)
#' @param con Database connection (optional)
#'
#' @return Tibble with measurement features in requested format
#' @export
query_traits_measures_features <- function(
    id_trait_measures = NULL,
    src = c("individuals", "taxa"),
    format = c("wide", "long"),
    con = NULL
) {
  
  src <- match.arg(src)
  format <- match.arg(format)
  
  # Get appropriate connection and table names
  config <- get_measurement_features_config(src)
  if (is.null(con)) con <- config$con
  
  # Quick count check
  n_records <- count_measurement_features(
    id_trait_measures = id_trait_measures,
    config = config,
    con = con
  )
  
  if (n_records == 0) {
    cli::cli_alert_info("No measurement features found")
    return(tibble())
  }
  
  cli::cli_alert_info("Found {n_records} measurement feature(s)")
  
  # Fetch raw data
  raw_data <- fetch_measurement_features_raw(
    id_trait_measures = id_trait_measures,
    config = config,
    con = con
  )
  
  # Return long format if requested
  if (format == "long") {
    return(raw_data)
  }
  
  # Pivot to wide format
  pivoted <- pivot_measurement_features(raw_data, config)
  
  return(pivoted)
}

#' Get configuration for measurement features
#' @keywords internal
get_measurement_features_config <- function(src) {
  
  if (src == "individuals") {
    list(
      con = mydb,
      table_name = "data_ind_measures_feat",
      trait_table = "traitlist",
      id_col = "id_ind_meas_feat"
    )
  } else if (src == "taxa") {
    list(
      con = mydb_taxa,
      table_name = "table_traits_measures_feat",
      trait_table = "table_traits",
      id_col = "id_taxa_trait_feat"
    )
  } else {
    stop("Unknown src: ", src)
  }
}

#' Count measurement features
#' @keywords internal
count_measurement_features <- function(id_trait_measures, config, con) {
  
  query <- glue::glue_sql("
    SELECT COUNT(*) AS n 
    FROM {`config$table_name`} 
    WHERE id_trait_measures IN ({id_trait_measures*})
  ", id_trait_measures = id_trait_measures, .con = con)
  
  result <- DBI::dbGetQuery(con, query)
  return(result$n)
}

#' Fetch raw measurement features data
#' @keywords internal
fetch_measurement_features_raw <- function(id_trait_measures, config, con) {
  
  # Get trait metadata
  trait_meta <- DBI::dbGetQuery(con, glue::glue_sql("
    SELECT id_trait, trait, valuetype, traitdescription
    FROM {`config$trait_table`}
  ", .con = con))
  
  # Get feature data
  feat_query <- glue::glue_sql("
    SELECT 
      id_trait_measures,
      id_trait,
      {`config$id_col`},
      typevalue,
      typevalue_char
    FROM {`config$table_name`}
    WHERE id_trait_measures IN ({id_trait_measures*})
  ", id_trait_measures = id_trait_measures, .con = con)
  
  feat_data <- DBI::dbGetQuery(con, feat_query)
  
  # Join with metadata
  result <- feat_data %>%
    distinct() %>%
    left_join(trait_meta, by = "id_trait") %>%
    mutate(!!sym(config$id_col) := as.character(!!sym(config$id_col)))
  
  return(result)
}

#' Pivot measurement features to wide format
#' @keywords internal
pivot_measurement_features <- function(data, config) {
  
  if (nrow(data) == 0) return(tibble())
  
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
      id_col = config$id_col
    )
  }
  
  # Combine all pivoted data
  if (length(pivoted_list) == 0) {
    return(tibble())
  }
  
  combined <- bind_rows(pivoted_list, .id = NULL)
  
  return(combined)
}

#' Pivot features by type with custom aggregation
#' @keywords internal
pivot_features_by_type <- function(data, value_col, id_col, agg_fun) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Convert to data.table for efficient pivoting
  dt <- data.table::as.data.table(data)
  
  # Create formula for dcast
  formula <- as.formula(paste("id_trait_measures +", id_col, "~ trait"))
  
  # Pivot
  pivoted <- data.table::dcast(
    dt,
    formula = formula,
    value.var = value_col,
    fun.aggregate = agg_fun
  )
  
  return(as_tibble(pivoted))
}

#' Pivot table-referenced features
#' @keywords internal
pivot_table_references <- function(data, id_col) {
  
  results <- list()
  
  for (vt in unique(data$valuetype)) {
    
    # Get lookup table info
    lookup_info <- get_lookup_table_info(vt)
    
    if (is.null(lookup_info)) {
      cli::cli_alert_warning("Unknown table valuetype: {vt}")
      next
    }
    
    # Get lookup values
    lookup_values <- DBI::dbGetQuery(
      mydb,
      glue::glue_sql("
        SELECT {`lookup_info$id_col`}, {`lookup_info$value_col`}
        FROM {`vt`}
      ", .con = mydb)
    )
    
    # Join and pivot
    tmp <- data %>%
      filter(valuetype == vt) %>%
      left_join(
        lookup_values,
        by = setNames(lookup_info$id_col, "typevalue")
      ) %>%
      mutate(typevalue_char = !!sym(lookup_info$value_col)) %>%
      select(id_trait_measures, trait, typevalue_char, !!sym(id_col))
    
    if (nrow(tmp) > 0) {
      results[[vt]] <- tmp %>%
        pivot_wider(
          names_from = trait,
          values_from = typevalue_char,
          values_fn = ~paste(., collapse = "|")
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
fetch_linked_individuals <- function(individual_ids, con, chunk_size = 30000) {
  
  if (length(individual_ids) == 0) {
    cli::cli_alert_warning("No individual IDs provided")
    return(tibble())
  }
  
  # If small number of IDs, fetch directly
  if (length(individual_ids) <= chunk_size) {
    return(merge_individuals_taxa(id_individual = individual_ids))
  }
  
  # For large number of IDs, use chunking
  chunks <- split(individual_ids, ceiling(seq_along(individual_ids) / chunk_size))
  n_chunks <- length(chunks)
  
  cli::cli_alert_info("Processing {n_chunks} chunk(s) for linked individuals")
  
  pb <- txtProgressBar(min = 0, max = n_chunks, style = 3)
  
  results <- lapply(seq_along(chunks), function(i) {
    setTxtProgressBar(pb, i)
    
    # merge_individuals_taxa returns a tibble directly
    chunk_result <- merge_individuals_taxa(id_individual = chunks[[i]])
    
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
  
  combined <- reduce(results, full_join, by = join_cols)
  
  return(combined)
}

#' Pivot numeric traits
#' @keywords internal
pivot_numeric_traits <- function(data, include_census) {
  
  # Séparer données avec et sans subplot
  data_no_subplot <- data %>% filter(is.na(id_sub_plots))
  data_with_subplot <- data %>% filter(!is.na(id_sub_plots))
  
  results <- list()
  
  # Pivot sans census
  if (nrow(data_no_subplot) > 0) {
    results$no_census <- data_no_subplot %>%
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
      rename_with(~str_remove(.x, "value_"), starts_with("value_")) %>%
      select(-starts_with("id_measure_"))
  }
  
  # Pivot avec census
  if (include_census && nrow(data_with_subplot) > 0) {
    results$with_census <- data_with_subplot %>%
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
      rename_with(~str_remove(.x, "value_"), starts_with("value_")) %>%
      select(-starts_with("id_measure_"))
  }
  
  if (length(results) > 0) {
    return(bind_rows(results))
  }
  
  return(tibble())
}

#' Pivot character traits
#' @keywords internal
pivot_character_traits <- function(data, include_census) {
  
  # Utiliser traitvalue_char pour les valeurs textuelles
  data <- data %>%
    mutate(traitvalue_char = str_squish(traitvalue_char))
  
  # Séparer données avec et sans subplot
  data_no_subplot <- data %>% filter(is.na(id_sub_plots))
  data_with_subplot <- data %>% filter(!is.na(id_sub_plots))
  
  results <- list()
  
  # Pivot sans census
  if (nrow(data_no_subplot) > 0) {
    results$no_census <- data_no_subplot %>%
      select(id_data_individuals, trait, traitvalue_char, id_trait_measures) %>%
      group_by(id_data_individuals, trait) %>%
      summarise(
        value = paste(traitvalue_char[!is.na(traitvalue_char)], collapse = ", "),
        id_measure = first(id_trait_measures),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = trait,
        values_from = value,
        names_prefix = "char_"
      ) %>%
      mutate(across(starts_with("char_"), ~na_if(.x, "")))
  }
  
  # Pivot avec census
  if (include_census && nrow(data_with_subplot) > 0) {
    results$with_census <- data_with_subplot %>%
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
      mutate(across(starts_with("char_"), ~na_if(.x, "")))
  }
  
  if (length(results) > 0) {
    return(bind_rows(results))
  }
  
  return(tibble())
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
      COUNT(DISTINCT tm.id_data_individuals) as n_individuals,
      COUNT(*) as n_measurements
    FROM traitlist tl
    LEFT JOIN data_traits_measures tm ON tl.id_trait = tm.traitid
    GROUP BY tl.trait, tl.valuetype, tl.description, tl.unit
    ORDER BY n_measurements DESC
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



#' Add trait
#'
#' @description
#' Add trait and associated descriptors in trait list table
#' 
#' @param new_trait A single string.
#' @param new_relatedterm Optional. A single string.
#' @param new_valuetype A single string, one of `"numeric"`, `"integer"`, `"categorical"`, `"ordinal"`, `"logical"`, `"character"`, `"table_data_liste_plots"`, or `"table_colnam"`.
#' @param new_maxallowedvalue Optional. if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue Optional. if valuetype is numeric, indicate the minimum allowed value
#' @param new_traitdescription Optional. A single string.
#' @param new_factorlevels Optional. Factor levels.
#' @param new_expectedunit Optional. A single string.
#' @param new_comments Optional. A single string.
#'
#' @returns 
#' The function writes to a database table if confirmed by the user. The function
#' will error if `new_trait` or `new_valuetype` are not provided, if `new_valuetype`
#' is not one of the allowed values, or if numeric/integer value types don't match
#' their corresponding min/max values.
#'
#' @export
add_trait <- function(new_trait = NULL,
                      new_relatedterm = NULL,
                      new_valuetype = NULL,
                      new_maxallowedvalue = NULL,
                      new_minallowedvalue = NULL,
                      new_traitdescription = NULL,
                      new_factorlevels = NULL,
                      new_expectedunit = NULL,
                      new_comments = NULL) {

  mydb <- call.mydb()

  if(is.null(new_trait)) stop("define new trait")
  if(is.null(new_valuetype)) stop("define new_valuetype")

  if (!any(
    new_valuetype == c(
      'numeric',
      'integer',
      'categorical',
      'ordinal',
      'logical',
      'character',
      'table_data_liste_plots',
      'table_colnam'
    )
  ))
  stop(
    "valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character', 'table_data_liste_plots' or 'table_colnam'"
  )
  
  if (new_valuetype == "numeric" | new_valuetype == "integer")
    if (!is.numeric(new_maxallowedvalue) &
        !is.integer(new_maxallowedvalue))
      stop("valuetype numeric of integer and max value not of this type")
  if (new_valuetype == "numeric" | new_valuetype == "integer")
    if (!is.numeric(new_minallowedvalue) &
        !is.integer(new_minallowedvalue))
      stop("valuetype numeric of integer and min value not of this type")
  
  new_data_renamed <- tibble(
    trait = new_trait,
    relatedterm = ifelse(is.null(new_relatedterm), NA, new_relatedterm),
    valuetype = new_valuetype,
    maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
    minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
    traitdescription = ifelse(is.null(new_traitdescription), NA, new_traitdescription),
    factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
    expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
    comments = ifelse(is.null(new_comments), NA, new_comments)
  )
  
  print(new_data_renamed)

  # Q <- utils::askYesNo("confirm adding this trait?")
  Q <- choose_prompt(message = "confirm adding this trait ?")

  if(Q) DBI::dbWriteTable(mydb, "traitlist", new_data_renamed, append = TRUE, row.names = FALSE)

}








add_traits_measures_features <- function(new_data,
                                         id_trait_measures = "id_trait_measures",
                                         features,
                                         allow_multiple_value = FALSE,
                                         add_data =FALSE) {

  for (i in 1:length(features))
    if (!any(colnames(new_data) == features[i]))
      stop(paste("features field provide not found in new_data", features[i]))

  new_data_renamed <- new_data

  ## removing entries with NA values for traits
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter_at(dplyr::vars(!!features), dplyr::any_vars(!is.na(.)))

  if (nrow(new_data_renamed) == 0)
    stop("no values for selected features(s)")

  new_data_renamed <-
    new_data_renamed %>%
    mutate(id_new_data = 1:nrow(.))

  new_data_renamed <-
    new_data_renamed %>%
    rename(id_trait_measures := all_of(id_trait_measures))

  link_trait_measures <-
    new_data_renamed %>%
    dplyr::left_join(
      try_open_postgres_table(table = "data_traits_measures", con = mydb) %>%
        dplyr::select(id_trait_measures) %>%
        dplyr::filter(id_trait_measures %in% !!unique(new_data_renamed$id_trait_measures)) %>%
        dplyr::collect() %>%
        dplyr::mutate(rrr = 1),
      by = c("id_trait_measures" = "id_trait_measures")
    )

  if (dplyr::filter(link_trait_measures, is.na(rrr)) %>%
      nrow() > 0) {
    print(dplyr::filter(link_trait_measures, is.na(rrr)))
    stop("provided trait_measures not found in data_traits_measures")
  }


  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(features))
  for (i in 1:length(features)) {

    feat <- features[i]
    if(!any(colnames(new_data_renamed) == feat))
      stop(paste("feat field not found", feat))

    data_feat <-
      new_data_renamed

    data_feat <-
      data_feat %>%
      dplyr::filter(!is.na(!!sym(feat)))

    if(nrow(data_feat) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_feat <-
        .link_trait(data_stand = data_feat, trait = feat)

      ## see what type of value numeric of character
      valuetype <-
        data_feat %>%
        dplyr::distinct(id_trait) %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "traitlist") %>%
            dplyr::select(valuetype, id_trait) %>%
            dplyr::collect(),
          by = c("id_trait" = "id_trait")
        )

      if(valuetype$valuetype == "table_colnam") {

        add_col_sep <-
          data_feat %>%
          tidyr::separate_rows(trait, sep = ",") %>%
          mutate(trait = stringr::str_squish(trait))

        add_col_sep <- .link_colnam(
          data_stand = add_col_sep,
          column_searched = "trait",
          column_name = "colnam",
          id_field = "trait",
          id_table_name = "id_table_colnam",
          db_connection = mydb,
          table_name = "table_colnam"
        )

        data_feat <-add_col_sep

      }

      if (any(data_feat$trait == 0)) {

        # add_0 <- utils::askYesNo("Some value are equal to 0. Do you want to add these values anyway ??")

        add_0 <- choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
        
        if(!add_0)
          data_feat <-
            data_feat %>%
            dplyr::filter(trait != 0)

      }



      cli::cli_h3(".add_modif_field")
      data_feat <-
        .add_modif_field(dataset = data_feat)


      if (valuetype$valuetype == "ordinal" |
          valuetype$valuetype == "character")
        val_type <- "character"

      if (valuetype$valuetype == "numeric" | valuetype$valuetype == "table_colnam")
        val_type <- "numeric"

      if (valuetype$valuetype == "integer")
        val_type <- "numeric"

      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          id_trait_measures = data_feat$id_trait_measures,
          id_trait = data_feat$id_trait,
          typevalue = ifelse(
            rep(val_type == "numeric", nrow(data_feat)),
            data_feat$trait,
            NA
          ),
          typevalue_char = ifelse(
            rep(val_type == "character", nrow(data_feat)),
            as.character(data_feat$trait),
            NA
          ),
          date_modif_d = data_feat$date_modif_d,
          date_modif_m = data_feat$date_modif_m,
          date_modif_y = data_feat$date_modif_y
        )

      list_add_data[[i]] <-
        data_to_add

      print(data_to_add)

      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {

        duplicates_lg <- duplicated(data_to_add)

        cli::cli_alert_warning("Duplicates in new data for {feat} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        # cf_merge <-
        #   askYesNo(msg = "confirm merging duplicates?")
        
        cf_merge <- 
          choose_prompt(message = "confirm merging duplicates ?")

        if (cf_merge) {

          # issues_dup <- data_to_add %>%
          #   filter(id_trait_measures %in% data_to_add[duplicates_lg, "id_trait_measures"]) %>%
          #   dplyr::select(issue, id_trait_measures)

          ## resetting issue
          if(any(grepl("identical value", issues_dup$issue))) {

            issues_dup_modif_issue <-
              issues_dup[grepl("identical value", issues_dup$issue),]

            data_to_add <-
              data_to_add %>%
              mutate(issue = replace(issue, id_trait_measures %in% issues_dup_modif_issue$id_trait_measures, NA))

          }

          data_to_add <- data_to_add %>% dplyr::distinct()
        } else {
          if (!allow_multiple_value) stop()
        }

      }

      # response <-
      #   utils::askYesNo("Confirm add these data to data_ind_measures_feat table?")
      
      response <- 
        choose_prompt(message = "Confirm add these data to data_ind_measures_feat table ?")

      if(add_data & response) {

        DBI::dbWriteTable(mydb, "data_ind_measures_feat",
                          data_to_add,
                          append = TRUE,
                          row.names = FALSE)

        cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")
      }

    } else{

      cli::cli_alert_info("no added data for {trait} - no values different of 0")

    }
  }


  return(list(list_features_add = list_add_data))

}




#' Add an observation in trait measurement table
#'
#' Add a trait measure in trait measurement table
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param collector_field string column name which contain the collector name
#' @param plot_name_field string column name which contain the plot_name for linking
#' @param individual_plot_field string column name which contain the individual tag for linking
#' @param id_plot_name string column name which contain the ID of plot_name
#' @param id_tag_plot string column name which contain the ID of individuals table
#' @param id_specimen string column name which contain the ID of specimen
#' @param traits_field string vector listing trait columns names in new_data
#' @param features_field string vector listing features (column names) to link to measurementsin new_data
#' @param add_data logical whether or not data should be added - by default FALSE
#' @param allow_multiple_value if multiple values linked to one individual can be uploaded at once
#'
#' @export
add_traits_measures <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = NULL,
                                collector_field = NULL,
                                plot_name_field = NULL,
                                individual_plot_field = NULL,
                                id_plot_name = NULL,
                                id_tag_plot = NULL,
                                id_specimen = NULL,
                                traits_field,
                                features_field = NULL,
                                allow_multiple_value = FALSE,
                                add_data = FALSE) {
  
  mydb <- call.mydb()
  
  for (i in 1:length(traits_field))
    if (!any(colnames(new_data) == traits_field[i]))
      stop(paste("traits_field provide not found in new_data", traits_field[i]))
  
  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))
  
  
  
  if (!is.null(col_names_select) & !is.null(col_names_corresp)) {
    new_data_renamed <-
      .rename_data(dataset = new_data,
                   col_old = col_names_select,
                   col_new = col_names_corresp)
  } else{
    
    new_data_renamed <- new_data
    
  }
  
  ## removing entries with NA values for traits
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter_at(dplyr::vars(!!traits_field), dplyr::any_vars(!is.na(.)))
  
  if (nrow(new_data_renamed) == 0)
    stop("no values for selected trait(s)")
  
  if (!any(col_names_corresp == "day")) {
    cli::cli_alert_info("no information collection day provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(day = NA) %>%
      mutate(day = as.numeric(day))
    
    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }
  
  if (!any(col_names_corresp == "year")) {
    cli::cli_alert_info("no information collection year provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(year = NA) %>%
      mutate(year = as.numeric(year))
    
    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }
  
  if (!any(col_names_corresp == "month")) {
    cli::cli_alert_info("no information collection month provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(month = NA) %>%
      mutate(month = as.numeric(month))
    
    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }
  
  if(!any(col_names_corresp == "country")) {
    cli::cli_alert_info("no country provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(country = NA) %>%
      mutate(country = as.character(country))
    
    if(is.null(plot_name_field) & is.null(individual_plot_field) &
       is.null(id_specimen) & is.null(id_plot_name) &
       is.null(id_tag_plot)) stop("no links provided (either plot, specimen or tag), thus country is mandatory")
    
  }
  
  if (!any(col_names_corresp == "decimallatitude")) {
    cli::cli_alert_info("no decimallatitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(decimallatitude = NA) %>%
      dplyr::mutate(decimallatitude = as.double(decimallatitude))
    
    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus decimallatitude is mandatory")
  }
  
  if (!any(col_names_corresp == "decimallongitude")) {
    cli::cli_alert_info("no decimallongitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(decimallongitude = NA) %>%
      dplyr::mutate(decimallongitude = as.double(decimallongitude))
    
    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus decimallongitude is mandatory")
  }
  
  new_data_renamed <-
    new_data_renamed %>%
    mutate(id_new_data = 1:nrow(.))
  
  ### Linking collectors names
  if(!is.null(collector_field)) {
    if(!any(colnames(new_data_renamed) == collector_field))
      stop("no collector_field found in new dataset")
    # new_data_renamed <-
    #   .link_colnam(data_stand = new_data_renamed, collector_field = collector_field)
    
    new_data_renamed <-
      .link_colnam(
        data_stand = new_data_renamed,
        column_searched = collector_field,
        column_name = "colnam",
        id_field = "id_colnam",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
    
  } else{
    new_data_renamed <-
      new_data_renamed %>%
      mutate(id_colnam = NA) %>%
      mutate(id_colnam = as.numeric(id_colnam))
    
    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus collector_field is mandatory")
  }
  
  ### Linking plot names
  if(!is.null(plot_name_field)) {
    if (!any(colnames(new_data_renamed) == plot_name_field))
      stop("plot_name_field not found in colnames")
    
    # new_data_renamed <-
    #   .link_plot_name(data_stand = new_data_renamed, plot_name_field = plot_name_field)
    
    new_data_renamed <-
      .link_table(data_stand = new_data_renamed,
                  column_searched = plot_name_field,
                  column_name = "plot_name",
                  id_field = "id_liste_plots",
                  id_table_name = "id_liste_plots",
                  db_connection = mydb,
                  table_name = "data_liste_plots")
    
  }
  
  if (!is.null(id_plot_name)) {
    id_plot_name <- "id_table_liste_plots_n"
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(id_plot_name), ~ id_plot_name)
    
    if (any(colnames(new_data_renamed) == "plot_name"))
      new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-plot_name)
    
    link_plot <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(plot_name, id_liste_plots) %>% dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )
    
    if (dplyr::filter(link_plot, is.na(plot_name)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_plot, is.na(plot_name)))
      cli::cli_alert_danger("provided id plot not found in plot metadata")
    }
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename(id_liste_plots = id_table_liste_plots_n)
  }
  
  ### linking individuals by id
  if(!is.null(id_tag_plot) & is.null(individual_plot_field)) {
    
    id_tag <-
      "id_n"
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(all_of(id_tag_plot)), ~ id_tag)
    
    
    link_individuals <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::select(idtax_n, 
                        id_n, 
                        # sous_plot_name
                        ) %>%
          dplyr::filter(id_n %in% !!unique(new_data_renamed$id_n)) %>%
          dplyr::collect() %>%
          dplyr::mutate(rrr = 1),
        by = c("id_n" = "id_n")
      )
    
    if (dplyr::filter(link_individuals, is.na(rrr)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_individuals
                          , 
                          is.na(rrr)
                          ))
      stop("provided id individuals not found in data_individuals")
    }
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename(id_data_individuals = id_n)
  } else{
    
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_data_individuals = NA) %>%
      dplyr::mutate(id_data_individuals = as.integer(id_data_individuals))
    
  }
  
  
  if (is.null(id_plot_name) & is.null(plot_name_field)) {
    
    if (!is.null(id_tag_plot) & is.null(individual_plot_field)) {
      
      queried_individuals <-
        query_plots(id_individual = new_data_renamed$id_data_individuals, remove_ids = F)
      
      new_data_renamed <-
        new_data_renamed %>%
        left_join(queried_individuals %>%
                    dplyr::select(id_n, id_table_liste_plots_n),
                  by = c("id_data_individuals" = "id_n")) %>%
        rename(id_liste_plots = id_table_liste_plots_n)
      
    } else {
      
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::mutate(id_liste_plots = NA) %>%
        dplyr::mutate(id_liste_plots = as.integer(id_liste_plots))
      
    }
  }
  
  ### check for different census for concerned plots
  multiple_census <- FALSE
  # census_check <- utils::askYesNo(msg = "Link trait measures to census (only for permanent plots) ?")
  
  census_check <- 
    choose_prompt(message = "Link trait measures to census (only for permanent plots) ?")
    
  if (census_check) {
    unique_ids_plots <- unique(new_data_renamed$id_liste_plots)
    censuses <-
      try_open_postgres_table(table = "data_liste_sub_plots", con = mydb) %>%
      dplyr::filter(id_table_liste_plots %in% unique_ids_plots, id_type_sub_plot==27) %>%
      dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                         dplyr::select(plot_name, id_liste_plots), by=c("id_table_liste_plots"="id_liste_plots")) %>%
      dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                         dplyr::select(type, id_subplotype), by=c("id_type_sub_plot"="id_subplotype")) %>%
      dplyr::left_join(dplyr::tbl(mydb, "table_colnam") %>%
                         dplyr::select(id_table_colnam, colnam), by=c("id_colnam"="id_table_colnam")) %>%
      dplyr::collect()
    
    if(nrow(censuses) > 0) { # & length(unique(censuses$typevalue))>1
      
      cli::cli_alert_info("Multiple census for concerned plots")
      censuses %>%
        dplyr::select(plot_name, id_table_liste_plots, year, month, day, typevalue, type, colnam, additional_people) %>%
        as.data.frame() %>%
        print()
      census_chosen <- readline(prompt="Choose census ")
      
      chosen_ids_subplots <-
        censuses %>%
        dplyr::filter(typevalue == as.numeric(census_chosen)) %>%
        dplyr::select(id_table_liste_plots, id_sub_plots)
      
      if(nrow(chosen_ids_subplots) == 0) stop("chosen census not available")
      
      missing_census <-
        new_data_renamed %>%
        dplyr::distinct(id_liste_plots) %>%
        dplyr::filter(!id_liste_plots %in% chosen_ids_subplots$id_table_liste_plots) %>%
        dplyr::filter(!is.na(id_liste_plots))
      
      if(nrow(missing_census)) {
        print(missing_census %>%
                dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                                   dplyr::select(id_liste_plots, plot_name) %>%
                                   dplyr::collect(),
                                 by=c("id_liste_plots"="id_liste_plots")) %>%
                as.data.frame())
        warning(paste("Missing census for", nrow(missing_census),"plots, census chosen :", census_chosen))
      }
      
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::left_join(chosen_ids_subplots,
                         by = c("id_liste_plots" = "id_table_liste_plots"))
      # %>%
      #   filter(id_liste_plots==824) %>%
      #   select(id_sub_plots)
      
      if (as.numeric(census_chosen) > 1)
        multiple_census <- TRUE
      
    } else {
      
      new_data_renamed <-
        new_data_renamed %>%
        tibble::add_column(id_sub_plots = NA) %>%
        dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))
      multiple_census <- FALSE
    }
  }else{
    
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_sub_plots = NA) %>%
      dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))
    
  }
  
  ### Linking specimens
  if(!is.null(id_specimen)) {
    
    id_tag <-
      "id_specimen"
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(id_specimen), ~ id_tag)
    
    link_specimen <-
      new_data_renamed %>%
      dplyr::filter(!is.na(id_specimen)) %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "specimens") %>%
          dplyr::select(id_diconame_n, id_specimen) %>% dplyr::collect(),
        by = c("id_specimen" = "id_specimen")
      )
    
    if(dplyr::filter(link_specimen, is.na(id_diconame_n)) %>%
       nrow()>0) {
      print(dplyr::filter(link_specimen, is.na(id_diconame_n)))
      stop("provided id specimens not found in specimens table")
    }
  }else{
    
    if (!any(colnames(new_data_renamed) == "id_specimen")) {
      
      new_data_renamed <-
        new_data_renamed %>%
        mutate(id_specimen = NA) %>%
        dplyr::mutate(id_specimen = as.integer(id_specimen))
      
    } else{
      
      warning("id_specimen column already in new_data, check if content is correct")
      
    }
    
  }
  
  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(traits_field))
  for (i in 1:length(traits_field)) {
    
    trait <- traits_field[i]
    if(!any(colnames(new_data_renamed) == trait))
      stop(paste("trait field not found", trait))
    
    data_trait <-
      new_data_renamed
    
    
    data_trait <-
      data_trait %>%
      dplyr::filter(!is.na(!!sym(trait)))
    
    
    if(nrow(data_trait) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_trait <-
        .link_trait(data_stand = data_trait, trait = trait)
      
      if (any(data_trait$trait == 0)) {
        
        # add_0 <- utils::askYesNo("Some value are equal to 0. Do you want to add these values anyway ??")
        
        add_0 <- 
          choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
        
        if(!add_0)
          data_trait <-
            data_trait %>%
            dplyr::filter(trait != 0)
        
      }
      
      ## see what type of value numeric of character
      valuetype <-
        data_trait %>%
        dplyr::distinct(id_trait) %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "traitlist") %>%
            dplyr::select(valuetype, id_trait) %>%
            dplyr::collect(),
          by = c("id_trait" = "id_trait")
        )
      
      ### Linking individuals
      if (!is.null(individual_plot_field)) {

        individual_plot <-
          "tag"

        data_trait <-
          data_trait %>%
          dplyr::rename_at(dplyr::vars(all_of(individual_plot_field)), ~ individual_plot)


        ## not numeric or missing individuals tag
        nbe_not_numeric <-
          suppressWarnings(which(is.na(as.numeric(data_trait$tag))))

        data_trait <-
          data_trait %>%
          dplyr::mutate(tag = as.numeric(tag))

        if(length(nbe_not_numeric) > 0) {
          cli::cli_alert_warning(
            "Number of non numeric (or missing) value in column indicating invividual number in plot : {length(nbe_not_numeric)}"
          )
          print(nbe_not_numeric)

          data_trait <-
            data_trait %>%
            filter(!is.na(tag))

          cli::cli_alert_warning("Number of non numeric (or missing) value REMOVED")
        }

        ## vector of id of all plots
        ids_plots_represented <-
          data_trait %>%
          dplyr::distinct(id_liste_plots) %>%
          dplyr::filter(!is.na(id_liste_plots)) %>%
          dplyr::pull()

        ## query of all individuals of these plots
        all_individual_selected_plot <-
          dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::select(tag, id_table_liste_plots_n,
                        id_n, id_diconame_n, id_specimen) %>%
          dplyr::filter(id_table_liste_plots_n %in% ids_plots_represented) %>%
          dplyr::collect()
        
        
        cli::cli_h3("Checking plot by plot if individuals already linked to selected trait")
        cli::cli_alert_info("Expected for some individuals if added traits measures are new census")
        
        linked_individuals_list <- vector('list', length(ids_plots_represented))
        linked_individuals_likely_dup <- vector('list', length(ids_plots_represented))
        for (j in 1:length(ids_plots_represented)) {
          
          ### getting all individuals of selected plot
          all_individual_selected_plot_subset <-
            all_individual_selected_plot %>%
            dplyr::filter(id_table_liste_plots_n == ids_plots_represented[j])
          
          new_data_renamed_subset <-
            data_trait %>%
            dplyr::filter(id_liste_plots == ids_plots_represented[j])
          
          ## individuals in new observations linked to data_individuals
          linked_individuals <-
            dplyr::left_join(new_data_renamed_subset,
                             all_individual_selected_plot_subset,
                             by=c("tag" = "tag"))

          ## getting individuals that have already observations traits_measures table
          individuals_already_traits <-
            dplyr::tbl(mydb, "data_traits_measures") %>%
            dplyr::filter(id_data_individuals %in% !!linked_individuals$id_n) %>%
            dplyr::collect()

          if(nrow(individuals_already_traits) > 0 &
             any(unique(data_trait$id_trait) %in%
                 unique(individuals_already_traits$traitid))) {

            cli::cli_alert_warning("Individuals of plot id {ids_plots_represented[j]} already linked to this trait - consistency should be checked")

            linked_individuals %>%
              dplyr::select(id_new_data,
                            id_trait,
                            id_table_liste_plots_n,
                            id_sub_plots,
                            tag,
                            id_n,
                            trait)
            
            ## traits measures linked to same individuals from same subplot and same trait
            possible_duplicates_measures <-
              individuals_already_traits %>%
              filter(
                traitid == unique(data_trait$id_trait),
                id_sub_plots %in% unique(data_trait$id_sub_plots)
              ) %>%
              dplyr::select(
                id_data_individuals,
                id_trait_measures,
                traitvalue) %>%
              dplyr::rename(traitvalue_exist = traitvalue)
            
            
            linked_individuals_already_db <-
              linked_individuals %>%
              dplyr::left_join(possible_duplicates_measures,
                               by = c("id_n" = "id_data_individuals")) %>%
              filter(!is.na(id_trait_measures)) %>%
              dplyr::select(id_new_data, trait, traitvalue_exist)
            
            linked_individuals_likely_dup[[j]] <-
              linked_individuals_already_db
            
            # linked_problems_individuals_list[[j]] <-
            #   individuals_already_traits
            
            # if(any(colnames(new_data_renamed)=="id_diconame_n") &
            #    any(traits_field=="id_diconame_n"))
            
            ### Which individuals show different information between what is provided and what is in the db
            # problems_individuals <-
            #   linked_individuals %>%
            #   dplyr::filter(id_diconame!=id_diconame_n | dbh.x!=dbh.y)
            #
            # if(nrow(problems_individuals)>0) {
            #   cat(paste(nrow(problems_individuals), "individuals with problematic matches\n",
            #             nrow(linked_individuals), "individuals in total"))
            #
            #   selected_tax <-
            #     dplyr::tbl(mydb,"diconame") %>%
            #     dplyr::filter(id_n %in% problems_individuals$id_diconame_n) %>%
            #     dplyr::collect()
            #
            #   problems_individuals <-
            #     problems_individuals %>%
            #     dplyr::left_join(selected_tax %>%
            #                        dplyr::select(id_n, full_name_no_auth),
            #                      by=c("id_diconame_n"="id_n"))
            #
            #   for (j in 1:nrow(problems_individuals)) {
            #     problems_individuals_selected <-
            #       problems_individuals %>%
            #       dplyr::slice(j)
            #
            #     print(problems_individuals_selected %>%
            #             dplyr::select(plot_name,
            #                           ind_num_sous_plot, dbh.x, dbh.y,
            #                           corrected.name, full_name_no_auth))
            #     response <-
            #       askYesNo("Do you want to still link these individuals?")
            #
            #     if(!response) {
            #       linked_individuals <-
            #         linked_individuals %>%
            #         dplyr::filter(!id_new_data %in% problems_individuals_selected$id_new_data)
            #
            #
            
            #     }
            #   }
            # }
          }
          
          linked_individuals_list[[j]] <-
            linked_individuals %>%
            dplyr::select(id_new_data, id_n, id_specimen)
          
        }
        
        linked_individuals_list <-
          dplyr::bind_rows(linked_individuals_list)
        
        linked_individuals_likely_dup <-
          dplyr::bind_rows(linked_individuals_likely_dup)
        
        if(nrow(linked_individuals_likely_dup) > 0) {
          
          cli::cli_alert_info("Found {nrow(linked_individuals_likely_dup)} measures likely already in db")
          
          # remove_dup <- askYesNo(msg = "Remove these measures?")
          remove_dup <- choose_prompt(message = "Remove these measures?")
          if(remove_dup)
            data_trait <-
            data_trait %>%
            filter(!id_new_data %in% linked_individuals_likely_dup$id_new_data)
          
        }
        
        
        
        # linked_problems_individuals_list <-
        #   dplyr::bind_rows(linked_problems_individuals_list)
        
        ## Adding link to individuals in plots
        data_trait <-
          data_trait %>%
          dplyr::left_join(linked_individuals_list)
        
        if (!any(colnames(data_trait) == "id_data_individuals")) {
          
          data_trait <-
            data_trait %>%
            dplyr::rename(id_data_individuals = id_n)
          
        } else {
          
          data_trait <-
            data_trait %>%
            dplyr::mutate(id_data_individuals = id_n)
          
        }
        
        not_linked_ind <-
          data_trait %>%
          dplyr::filter(is.na(id_data_individuals))
        
        if (nrow(not_linked_ind) > 0) {
          message("Measures not linked to individuals")
          print(paste(nrow(not_linked_ind), "measures"))
          print(not_linked_ind %>%
                  as.data.frame())
          # remove_not_link <-
          #   utils::askYesNo(msg = "Remove these measures ?")
          
          remove_not_link <-
            choose_prompt(message = "Remove these measures?")
          
          unlinked_individuals <-
            not_linked_ind
          
          if (remove_not_link)
            data_trait <-
            data_trait %>%
            dplyr::filter(!is.na(id_data_individuals))
          
          
        }else{
          unlinked_individuals <- NA
        }
        
        ## identify duplicated individuals i.e. observations linked to same individual
        ids_dup <-
          data_trait %>%
          dplyr::group_by(id_data_individuals) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1, !is.na(id_data_individuals))
        
        if (nrow(ids_dup) > 0) {
          cli::cli_alert_warning("More than one observation of selected trait for a given individual - {nrow(ids_dup)} individuals concerned - adding issue")
          
          obs_dup <-
            data_trait %>%
            dplyr::filter(id_data_individuals %in% dplyr::pull(ids_dup, id_data_individuals)) %>%
            dplyr::select(trait, plot_name, tag, id_data_individuals, id_new_data)
          
          issue_2 <- vector(mode = "character", length = nrow(data_trait))
          for (k in 1:nrow(ids_dup)) {
            obs_dup_sel <- obs_dup %>%
              dplyr::filter(id_data_individuals %in% ids_dup$id_data_individuals[k])
            if(length(unique(obs_dup_sel$trait))>1) {
              issue_2[data_trait$id_new_data %in% obs_dup_sel$id_new_data] <-
                rep("more than one observation for a single individual carrying different value", nrow(obs_dup_sel))
            }else{
              issue_2[data_trait$id_new_data %in% obs_dup_sel$id_new_data] <-
                rep("more than one observation for a single individual carrying identical value", nrow(obs_dup_sel))
            }
          }
          
          issue_2[issue_2 == ""] <- NA
          
          ## merging issue
          data_trait <-
            data_trait %>%
            tibble::add_column(issue_2 = issue_2) %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue), ifelse(is.na(issue_2), "", issue_2), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue ==", ", NA, issue)) %>%
            dplyr::select(-issue_2)
        }
      } # end Linking individuals
      
      ## adding id_diconame_n ONLY if no individuals or specimen linked
      # otherwise, identification retrieved from individual or specimen
      if (!any(colnames(data_trait) == "id_diconame")) {
        
        data_no_specimen_no_individual <-
          data_trait
        
        if (any(colnames(data_trait) == "id_data_individuals")) {
          data_no_specimen_no_individual <-
            data_no_specimen_no_individual %>%
            dplyr::filter(is.na(id_data_individuals))
        }
        
        if (any(colnames(data_trait) == "id_specimen")) {
          data_no_specimen_no_individual <-
            data_no_specimen_no_individual %>%
            dplyr::filter(is.na(id_specimen))
        }
        
        data_trait <-
          data_trait %>%
          dplyr::mutate(id_diconame = NA) %>%
          dplyr::mutate(id_diconame = as.integer(id_diconame))
        
      } else {
        
        data_no_specimen_no_individual <-
          data_trait %>%
          dplyr::filter(is.na(id_data_individuals) & is.na(id_specimen) & is.na(id_diconame))
        
        ids_ind <- data_trait$id_data_individuals
        
        ## retrieving taxonomic information for linked individuals
        founded_ind <-
          query_plots(extract_individuals = T, id_individual = ids_ind, remove_ids = FALSE)
        
        ids_diconames <- data_trait$id_diconame
        
        data_trait_compa_taxo <-
          data_trait %>%
          dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                             dplyr::filter(id_n %in% ids_diconames) %>%
                             dplyr::select(tax_fam, tax_gen, full_name_no_auth, id_n) %>%
                             dplyr::collect(),
                           by=c("id_diconame"="id_n"))
        
        data_trait_compa_taxo <-
          data_trait_compa_taxo %>%
          dplyr::left_join(founded_ind %>%
                             dplyr::select(id_n, tax_fam, tax_gen, full_name_no_auth) %>%
                             dplyr::rename(tax_fam_linked = tax_fam, tax_gen_linked = tax_gen, full_name_no_auth_linked = full_name_no_auth),
                           by=c("id_data_individuals"="id_n")) %>%
          dplyr::select(id_new_data, tax_fam, tax_fam_linked, tax_gen,
                        tax_gen_linked, full_name_no_auth, full_name_no_auth_linked)
        
        diff_fam <-
          data_trait_compa_taxo %>%
          dplyr::filter(tax_fam != tax_fam_linked)
        if (nrow(diff_fam) > 0) {
          message("Some measures linked to individuals carry different family")
          print(diff_fam)
          diff_fam <-
            diff_fam %>%
            dplyr::mutate(
              issue = paste(
                "ident. when measured and in DB)",
                full_name_no_auth,
                full_name_no_auth_linked
              )
            )
          ## merging issue
          data_trait <-
            data_trait %>%
            dplyr::left_join(
              diff_fam %>%
                dplyr::select(id_new_data, issue) %>%
                dplyr::rename(issue_tax = issue),
              by = c("id_new_data" = "id_new_data")
            )
          
          data_trait <-
            data_trait %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue),
                                        ifelse(is.na(issue_tax), "", issue_tax), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue == ", ", NA, issue)) %>%
            dplyr::select(-issue_tax)
        }
        
        diff_gen <-
          data_trait_compa_taxo %>%
          dplyr::filter(tax_gen != tax_gen_linked, !id_new_data %in% diff_fam$id_new_data)
        
        if(nrow(diff_gen)>0) {
          message("Some measures linked to individuals carry different genus")
          print(diff_gen)
          diff_gen <-
            diff_gen %>%
            dplyr::mutate(issue = paste("ident. when measured and in DB)",
                                        full_name_no_auth, full_name_no_auth_linked))
          
          ## merging issue
          data_trait <-
            data_trait %>%
            dplyr::left_join(diff_gen %>%
                               dplyr::select(id_new_data, issue) %>%
                               dplyr::rename(issue_tax = issue),
                             by=c("id_new_data"="id_new_data"))
          
          data_trait <-
            data_trait %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue),
                                        ifelse(is.na(issue_tax), "", issue_tax), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue ==", ", NA, issue)) %>%
            dplyr::select(-issue_tax)
        }
        
      }
      
      no_linked_measures <- FALSE
      if (nrow(data_no_specimen_no_individual) > 0) {
        print(data_no_specimen_no_individual)
        cli::cli_alert_danger(
          "no taxa identification, no link to specimen, no link to individuals for measures/observations"
        )
        no_linked_measures <- TRUE
      }
      
      ### choosing kind of measures
      cli::cli_h3("basis")
      if (!any(colnames(data_trait) == "basisofrecord")) {
        choices <-
          dplyr::tibble(
            basis =
              c(
                'LivingSpecimen',
                'PreservedSpecimen',
                'FossilSpecimen',
                'literatureData',
                'traitDatabase',
                'expertKnowledge'
              )
          )
        
        print(choices)
        selected_basisofrecord <-
          readline(prompt = "Choose basisofrecord : ")
        
        data_trait <-
          data_trait %>%
          dplyr::mutate(basisofrecord = rep(choices$basis[as.numeric(selected_basisofrecord)], nrow(.)))
      }
      
      
      ### comparing measures from previous census
      if(multiple_census &
         valuetype$valuetype == "numeric") {
        cli::cli_alert_info("Comparing measures from previous censuses")
        
        comparisons <-
          data_trait %>%
          dplyr::select(id_data_individuals, trait) %>%
          dplyr::left_join(dplyr::tbl(mydb, "data_traits_measures") %>%
                             dplyr::filter(traitid == !!unique(data_trait$id_trait)) %>%
                             dplyr::select(id_data_individuals, traitvalue) %>%
                             dplyr::collect(),
                           by=c("id_data_individuals"="id_data_individuals"),
                           relationship = "many-to-many") %>%
          filter(!is.na(traitvalue)) %>%
          dplyr::group_by(id_data_individuals) %>%
          dplyr::summarise(traitvalue = max(traitvalue, na.rm = TRUE),
                           trait = dplyr::first(trait)) %>%
          dplyr::mutate(traitvalue = replace(traitvalue, traitvalue == -Inf, NA))
        
        ## comparison with previous census if new values is lower than previous --> issue annotated
        if (any(!is.na(comparisons$traitvalue))) {
          # message("\n multiple data")
          finding_incoherent_values <-
            comparisons %>%
            dplyr::mutate(diff = trait - traitvalue) %>%
            dplyr::filter(diff < 0)
          
          if(any( finding_incoherent_values$diff < 0)) {
            cli::cli_alert_danger("Incoherent new values compared to previous censuses")
            finding_incoherent_values <-
              finding_incoherent_values %>%
              dplyr::mutate(issue_new =
                              ifelse(diff < 0, "value lower than previous census", NA))
            
            ### merging issues
            data_trait <-
              data_trait %>%
              dplyr::left_join(finding_incoherent_values %>%
                                 dplyr::select(id_data_individuals, issue_new),  by = c("id_data_individuals"="id_data_individuals")) %>%
              dplyr::mutate(issue = ifelse(!is.na(issue), paste(issue, issue_new, sep="|"), issue_new)) %>%
              dplyr::select(-issue_new)
            
          }
        }
      }
      
      
      ### identify if measures are already within DB
      cli::cli_alert_info("Identifying if imported values are already in DB")
      trait_id <- unique(data_trait$id_trait)
      selected_data_traits <-
        data_trait %>%
        dplyr::select(id_data_individuals,
                      id_trait,
                      id_liste_plots,
                      id_sub_plots,
                      trait,
                      issue)
      
      #### identify if duplicate values in the dataset to upload
      
      duplicated_rows <- selected_data_traits %>%
        group_by(id_data_individuals,
                 id_trait,
                 id_liste_plots,
                 id_sub_plots) %>%
        count() %>%
        filter(n > 1)
      
      if (nrow(duplicated_rows) > 0) {
        print(duplicated_rows)
        cli::cli_alert_warning("Duplicated values for dataset to upload")
        if (!choose_prompt(message = "Are you sure you want to continue ?")) stop("check duplicated value")
      }
      
      all_vals <-
        dplyr::tbl(mydb, "data_traits_measures") %>%
        dplyr::select(id_data_individuals, traitid, id_table_liste_plots, id_sub_plots,
                      traitvalue, traitvalue_char, issue, id_trait_measures) %>%
        dplyr::filter(traitid == trait_id, 
                      id_data_individuals %in% !!selected_data_traits$id_data_individuals) %>% #, !is.na(id_sub_plots)
        dplyr::collect()
      
      if (valuetype$valuetype == "numeric")
        all_vals <-
        all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue) %>%
        dplyr::select(-traitvalue_char)
      
      if (valuetype$valuetype == "character")
        all_vals <- all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue_char) %>%
        dplyr::select(-traitvalue) %>%
        dplyr::mutate(trait = stringr::str_trim(trait))
      
      if (valuetype$valuetype == "ordinal")
        all_vals <- all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue_char) %>%
        dplyr::select(-traitvalue) %>%
        dplyr::mutate(trait = stringr::str_trim(trait))
      
      if (nrow(all_vals) > 0) {
        duplicated_rows <-
          dplyr::bind_rows(selected_data_traits,
                           all_vals) %>%
          dplyr::filter(is.na(issue)) %>%
          dplyr::group_by(id_data_individuals,
                          id_trait,
                          id_liste_plots,
                          id_sub_plots,
                          issue) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1) %>%
          filter(id_data_individuals %in% selected_data_traits$id_data_individuals)
        # %>% # , id_data_individuals==73764
        # dplyr::filter(!grepl("more than one observation", issue))
        
        duplicated_rows_with_issue_no_double <-
          dplyr::bind_rows(selected_data_traits,
                           all_vals) %>%
          dplyr::filter(!is.na(issue),!grepl("more than one observation", issue)) %>%
          dplyr::select(-issue) %>%
          dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1)
        
        duplicated_rows_with_issue_double <-
          dplyr::bind_rows(selected_data_traits,
                           all_vals) %>%
          dplyr::filter(!is.na(issue), grepl("more than one observation", issue)) %>%
          dplyr::select(-issue) %>%
          dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots) %>%
          dplyr::count() %>%
          dplyr::filter(n > 2)
        
        # %>% #
        #   dplyr::filter(!grepl("more than one observation", issue))
        duplicated_rows <-
          dplyr::bind_rows(duplicated_rows,
                           duplicated_rows_with_issue_no_double,
                           duplicated_rows_with_issue_double)
        
        if (nrow(duplicated_rows) > 1) {
          cli::cli_alert_danger("Some values are already in DB or some values are duplicated in the dataset to upload")
          
          print(duplicated_rows %>%
                  dplyr::ungroup() %>%
                  dplyr::select(id_data_individuals, id_liste_plots, id_sub_plots))
          
          # rm_val <- askYesNo(msg = "Exclude these values ?")
          rm_val <- choose_prompt(message = "Exclude these values ?")
          
          if (rm_val) {
            
            data_trait <-
              data_trait %>%
              dplyr::filter(!id_data_individuals %in% duplicated_rows$id_data_individuals)
            
            cli::cli_alert_warning("{nrow(duplicated_rows)} values excluded values because already in DB")
          }
          
          if (!allow_multiple_value) if (nrow(data_trait) < 1) stop("no new values anymore to import after excluding duplicates")
        }
      }
      
      cli::cli_h3(".add_modif_field")
      data_trait <-
        .add_modif_field(dataset = data_trait)
      
      
      if (valuetype$valuetype == "ordinal" |
          valuetype$valuetype == "character")
        val_type <- "character"
      
      if (valuetype$valuetype == "numeric")
        val_type <- "numeric"
      
      if (valuetype$valuetype == "integer")
        val_type <- "numeric"
      
      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          id_table_liste_plots = data_trait$id_liste_plots,
          id_data_individuals = data_trait$id_data_individuals,
          id_specimen = data_trait$id_specimen,
          id_diconame = data_trait$id_diconame,
          id_colnam = data_trait$id_colnam,
          id_sub_plots = data_trait$id_sub_plots,
          country = data_trait$country,
          decimallatitude = data_trait$decimallatitude,
          decimallongitude = data_trait$decimallongitude,
          elevation = ifelse(rep(
            any(colnames(data_trait) == "elevation"), nrow(data_trait)
          ), data_trait$elevation, NA),
          verbatimlocality = ifelse(rep(
            any(colnames(data_trait) == "verbatimlocality"), nrow(data_trait)
          ), data_trait$verbatimlocality, NA),
          basisofrecord = data_trait$basisofrecord,
          references = ifelse(rep(
            any(colnames(data_trait) == "reference"), nrow(data_trait)
          ), data_trait$reference, NA),
          year = ifelse(rep(
            any(colnames(data_trait) == "year"), nrow(data_trait)
          ), data_trait$year, NA),
          month = ifelse(rep(
            any(colnames(data_trait) == "month"), nrow(data_trait)
          ), data_trait$month, NA),
          day = ifelse(rep(any(
            colnames(data_trait) == "day"
          ), nrow(data_trait)), data_trait$day, NA),
          measurementremarks = ifelse(rep(
            any(colnames(data_trait) == "measurementremarks"),
            nrow(data_trait)
          ), data_trait$measurementremarks, NA),
          measurementmethod = ifelse(rep(
            any(colnames(data_trait) == "measurementmethod"), nrow(data_trait)
          ), data_trait$measurementmethod, NA),
          traitid = data_trait$id_trait,
          traitvalue = ifelse(
            rep(val_type == "numeric", nrow(data_trait)),
            data_trait$trait,
            NA
          ),
          traitvalue_char = ifelse(
            rep(val_type == "character", nrow(data_trait)),
            as.character(data_trait$trait),
            NA
          ),
          original_tax_name = ifelse(rep(
            any(colnames(data_trait) == "original_tax_name"), nrow(data_trait)
          ), data_trait$original_tax_name, NA),
          original_plot_name = ifelse(rep(
            any(colnames(data_trait) == "original_plot_name"), nrow(data_trait)
          ), data_trait$original_plot_name, NA),
          original_specimen = ifelse(rep(
            any(colnames(data_trait) == "original_specimen"), nrow(data_trait)
          ), data_trait$original_specimen, NA),
          issue = data_trait$issue,
          date_modif_d = data_trait$date_modif_d,
          date_modif_m = data_trait$date_modif_m,
          date_modif_y = data_trait$date_modif_y
        )
      
      if(no_linked_measures)
        list_add_data[[i]] <-
        data_no_specimen_no_individual
      
      list_add_data[[i]] <-
        data_to_add
      
      print(data_to_add)
      
      # print(data_to_add %>%
      #         dplyr::left_join(tbl(mydb, "data_liste_sub_plots") %>%
      #                            select(typevalue, id_type_sub_plot, id_sub_plots) %>%
      #                            collect(), by=c("id_sub_plots"="id_sub_plots"))) %>%
      #   dplyr::left_join(tbl(mydb, "subplotype_list") %>%
      #                      select(id_subplotype, type ) %>%
      #                      collect(), by=c("id_type_sub_plot"="id_subplotype")) %>%
      #   View()
      
      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {
        
        duplicates_lg <- duplicated(data_to_add)
        
        cli::cli_alert_warning("Duplicates in new data for {trait} concerning {length(duplicates_lg[duplicates_lg])} id(s)")
        
        # cf_merge <-
        #   askYesNo(msg = "confirm merging duplicates?")
        cf_merge <-
          choose_prompt(message = "confirm merging duplicates?")
        
        if (cf_merge) {
          
          id_n_dup <- data_to_add[duplicates_lg, "id_data_individuals"] %>% pull()
          
          issues_dup <- data_to_add %>%
            filter(id_data_individuals %in% id_n_dup) %>%
            dplyr::select(issue, id_data_individuals)
          
          ## resetting issue
          if(any(grepl("identical value", issues_dup$issue))) {
            
            issues_dup_modif_issue <-
              issues_dup[grepl("identical value", issues_dup$issue),]
            
            data_to_add <-
              data_to_add %>%
              mutate(issue = replace(issue, id_data_individuals %in% issues_dup_modif_issue$id_data_individuals, NA))
            
          }
          
          data_to_add <- data_to_add %>% dplyr::distinct()
        } else{
          if (!allow_multiple_value) stop()
        }
        
      }
      
      # response <-
      #   utils::askYesNo("Confirm add these data to data_traits_measures table?")
      
      response <- 
        choose_prompt(message = "Confirm add these data to data_traits_measures table?")
      
      if (add_data & response) {
        
        DBI::dbWriteTable(mydb, "data_traits_measures",
                          data_to_add,
                          append = TRUE,
                          row.names = FALSE)
        
        cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")
        
        if (!is.null(features_field)) {
          
          imported_data <- tbl(mydb, "data_traits_measures") %>%
            filter(date_modif_d == !!data_to_add$date_modif_d[1],
                   date_modif_m == !!data_to_add$date_modif_m[1],
                   date_modif_y == !!data_to_add$date_modif_y[1]) %>%
            select(id_trait_measures, id_data_individuals) %>%
            collect() %>%
            arrange(id_trait_measures)
          
          ids <- imported_data %>% slice((nrow(imported_data)-nrow(data_to_add)+1):nrow(imported_data))
          
          data_feats <-
            data_trait %>% select(all_of(features_field), id_data_individuals) %>%
            mutate(id_trait_measures = ids$id_trait_measures,
                   id_data_individuals = ids$id_data_individuals)
          
          add_traits_measures_features(
            new_data = data_feats,
            id_trait_measures = "id_trait_measures",
            features = features_field , #
            add_data = T
          )
          
        }
        
      } else{
        
        cli::cli_alert_danger("No added data for {trait} - add_data is FALSE")
        
      }
      
    } else{
      
      cli::cli_alert_danger("No added data for {trait} - no values different of 0")
      
    }
  }
  
  if(exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))
  
  if(!exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data))
  
    
  
}


