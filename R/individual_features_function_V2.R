
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
  if (!is.data.table(data)) setDT(data)
  
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
    result <- dcast(
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
    result <- dcast(
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
  if (!is.data.table(data)) setDT(data)
  
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
    result <- dcast(
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
    
    # Pivot to wide
    result <- dcast(
      dt_ind,
      id_data_individuals ~ trait,
      value.var = "value"
    )
  }
  
  # Add prefix to distinguish from numeric
  setnames(result, 
           old = setdiff(names(result), "id_data_individuals"),
           new = paste0("char_", setdiff(names(result), "id_data_individuals")))
  
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
  if (!is.data.table(result)) setDT(result)
  
  # Split back into old format for compatibility
  char_cols <- names(result)[str_starts(names(result), "char_")]
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
#' @param include_metadata Include trait measurement features
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
    cli::cli_alert_info("Adding census information")
    raw_data <- add_census_info(raw_data, con)
  }
  
  # 4. Optional: Add measurement metadata
  if (include_metadata) {
    cli::cli_alert_info("Adding measurement metadata")
    raw_data <- add_measurement_features(raw_data, con)
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

#' Add census information to trait measurements
#' @keywords internal
add_census_info <- function(data, con) {
  
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

#' Add measurement-level features/metadata
#' @keywords internal
add_measurement_features <- function(data, con, src = "individuals") {
  
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
  dt <- as.data.table(data)
  
  # Create formula for dcast
  formula <- as.formula(paste("id_trait_measures +", id_col, "~ trait"))
  
  # Pivot
  pivoted <- dcast(
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
fetch_linked_individuals <- function(individual_ids, con) {
  
  if (length(individual_ids) > 30000) {
    result <- fetch_with_chunking(
      ids = individual_ids,
      query_fun = function(ids, ...) {
        merge_individuals_taxa(id_individual = ids)
      },
      chunk_size = 30000,
      con = con,
      desc = "linked individuals"
    )
  } else {
    result <- merge_individuals_taxa(id_individual = individual_ids)
  }
  
  return(result)
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
