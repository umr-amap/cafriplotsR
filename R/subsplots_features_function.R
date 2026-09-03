# =============================================================================
# EXTRACTION ET AGRÉGATION DES FEATURES PLOTS (REFACTORISÉ)
# =============================================================================

#' Query subplot features with improved architecture
#' 
#' Retrieves subplot features (measurements at plot level) with optional
#' aggregation and enrichment with subplot observation features.
#'
#' @param plot_ids Vector of plot IDs
#' @param subplot_ids Vector of subplot IDs (optional)
#' @param subplot_type Filter by subplot type (e.g., "census")
#' @param format Output format: "wide" (pivoted) or "long" (raw)
#' @param include_subplot_obs_features Include subplot observation features
#' @param con Database connection (optional)
#' 
#' @importFrom tidyr pivot_wider separate_rows
#'
#' @return List with components:
#'   - features_raw: Raw subplot features
#'   - features_aggregated: Aggregated features by plot (if format="wide")
#'   - census_info: Census-specific information (if census data present)
#'
#' @export
query_plot_features <- function(
    plot_ids = NULL,
    subplot_ids = NULL,
    subplot_type = NULL,
    format = c("wide", "long"),
    include_subplot_obs_features = FALSE,
    con = NULL
) {
  
  format <- match.arg(format)
  if (is.null(con)) con <- call.mydb()
  
  cli::cli_h2("Querying plot features")
  
  # 1. Fetch raw subplot features
  features_raw <- fetch_subplot_features(
    plot_ids = plot_ids,
    subplot_ids = subplot_ids,
    con = con
  )
  
  if (nrow(features_raw) == 0) {
    cli::cli_alert_warning("No subplot features found")
    return(list(
      features_raw = tibble(),
      features_aggregated = NA,
      census_info = NA
    ))
  }
  
  cli::cli_alert_success(
    "Found {nrow(features_raw)} feature(s) for {length(unique(features_raw$id_table_liste_plots))} plot(s)"
  )
  
  # 2. Optional: Filter by subplot type
  if (!is.null(subplot_type)) {
    features_raw <- features_raw %>%
      filter(grepl(subplot_type, type, ignore.case = TRUE))
    
    cli::cli_alert_info("Filtered to subplot type: {subplot_type}")
  }
  
  # 3. Optional: Add subplot observation features
  if (include_subplot_obs_features) {
    cli::cli_alert_info("Enriching with subplot observation features")
    features_raw <- enrich_subplot_obs_features(features_raw, con)
  }
  
  # 4. Extract census information if present
  census_info <- if (any(features_raw$type == "census")) {
    extract_census_info(features_raw)
  } else {
    NA
  }
  
  # 5. Format output
  if (format == "wide") {
    cli::cli_h2("Aggregating features to wide format")
    features_aggregated <- aggregate_plot_features(data = features_raw, con)
  } else {
    features_aggregated <- features_raw
  }
  
  cli::cli_alert_success("Query completed")
  
  return(list(
    features_raw = features_raw,
    features_aggregated = features_aggregated,
    census_info = census_info
  ))
}

# -----------------------------------------------------------------------------
# EXTRACTION DES DONNÉES BRUTES
# -----------------------------------------------------------------------------

#' Fetch subplot features from database
#' @keywords internal
fetch_subplot_features <- function(plot_ids, subplot_ids, con) {
  
  # Build query
  query <- "
    SELECT 
      sp.id_sub_plots,
      sp.id_table_liste_plots,
      sp.year,
      sp.month,
      sp.day,
      sp.typevalue,
      sp.typevalue_char,
      sp.original_subplot_name,
      sp.comment,
      spt.type,
      spt.valuetype,
      spt.typedescription
    FROM data_liste_sub_plots sp
    LEFT JOIN subplotype_list spt ON sp.id_type_sub_plot = spt.id_subplotype
    WHERE 1=1
  "
  
  conditions <- character()
  
  if (!is.null(plot_ids)) {
    conditions <- c(conditions,
                    glue::glue_sql("sp.id_table_liste_plots IN ({plot_ids*})", 
                                   plot_ids = plot_ids, .con = con)
    )
  }
  
  if (!is.null(subplot_ids)) {
    conditions <- c(conditions,
                    glue::glue_sql("sp.id_sub_plots IN ({subplot_ids*})", 
                                   subplot_ids = subplot_ids, .con = con)
    )
  }
  
  if (length(conditions) > 0) {
    query <- paste(query, "AND", paste(conditions, collapse = " AND "))
  }
  
  DBI::dbGetQuery(con, query)
}

#' Enrich with subplot observation features
#' @keywords internal
enrich_subplot_obs_features <- function(data, con) {
  
  subplot_ids <- unique(data$id_sub_plots)
  
  if (length(subplot_ids) == 0) return(data)
  
  # Fetch subplot observation features
  obs_features <- fetch_subplot_obs_features(subplot_ids, con)
  
  if (nrow(obs_features) == 0) {
    return(data)
  }
  
  # Pivot obs features to wide format to avoid column conflicts
  obs_features_wide <- obs_features %>%
    group_by(id_sub_plots, type) %>%
    summarise(
      value = if (first(valuetype) == "numeric") {
        mean(as.numeric(typevalue), na.rm = TRUE)
      } else {
        paste(unique(na.omit(typevalue_char)), collapse = ", ")
      },
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = type,
      values_from = value,
      names_prefix = "obs_"
    )
  
  # Join without conflicts
  data %>%
    left_join(obs_features_wide, by = "id_sub_plots")
}

#' Fetch subplot observation features
#' @keywords internal
fetch_subplot_obs_features <- function(subplot_ids, con) {
  
  query <- "
    SELECT 
      sf.id_sub_plots,
      sf.typevalue,
      sf.typevalue_char,
      sf.id_subplot_feat,
      spt.type,
      spt.valuetype
    FROM data_subplot_feat sf
    LEFT JOIN subplotype_list spt ON sf.id_type_sub_plot = spt.id_subplotype
    WHERE sf.id_sub_plots IN ({subplot_ids*})
  "
  
  sql <- glue::glue_sql(query, subplot_ids = subplot_ids, .con = con)
  
  result <- DBI::dbGetQuery(con, sql)
  
  # Handle table references (e.g., table_colnam)
  if (any(grepl("^table_", result$valuetype))) {
    result <- resolve_table_references(result, con)
  }
  
  return(result)
}

#' Resolve table references in features
#' @keywords internal
resolve_table_references <- function(data, con) {
  
  table_types <- unique(data$valuetype[grepl("^table_", data$valuetype)])
  
  for (table_type in table_types) {
    
    if (table_type == "table_colnam") {
      lookup <- DBI::dbGetQuery(con, "
        SELECT id_table_colnam, colnam
        FROM table_colnam
      ")
      
      data <- data %>%
        left_join(
          lookup,
          by = c("typevalue" = "id_table_colnam")
        ) %>%
        mutate(
          typevalue_char = if_else(valuetype == "table_colnam", colnam, typevalue_char)
        ) %>%
        select(-colnam)
    }
    # Add other table types as needed
  }
  
  return(data)
}

# -----------------------------------------------------------------------------
# AGRÉGATION DES FEATURES PAR PLOT
# -----------------------------------------------------------------------------

#' Aggregate plot features to wide format using data.table
#' @keywords internal
aggregate_plot_features <- function(data, con) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Separate by valuetype
  numeric_features <- aggregate_numeric_plot_features(
    data %>% dplyr::filter(valuetype == "numeric", type != "census")
  )
  
  character_features <- aggregate_character_plot_features(
    data %>% dplyr::filter(valuetype == "character", type != "census")
  )
  
  # Handle table references separately
  table_features <- if (any(grepl("^table_", data$valuetype))) {
    aggregate_table_plot_features(
      data %>% dplyr::filter(grepl("^table_", valuetype)),
      con
    )
  } else {
    NULL
  }
  
  # Extract census dates
  census_dates <- if (any(data$type == "census")) {
    extract_census_dates(data %>% dplyr::filter(type == "census"))
  } else {
    NULL
  }
  
  # Combine all
  all_features <- list(
    numeric_features,
    character_features,
    table_features,
    census_dates
  )
  
  all_features <- all_features[!sapply(all_features, is.null)]
  
  if (length(all_features) == 0) {
    return(tibble(id_table_liste_plots = unique(data$id_table_liste_plots)))
  }
  
  result <- purrr::reduce(all_features, dplyr::full_join, by = "id_table_liste_plots")
  
  return(result)
}

#' Aggregate numeric plot features
#' @keywords internal
aggregate_numeric_plot_features <- function(data) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Convert to data.table FIRST
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Convert to numeric
  data[, typevalue_num := as.numeric(typevalue)]
  
  # Aggregate by plot and type
  agg <- data[, .(value = mean(typevalue_num, na.rm = TRUE)),
              by = .(id_table_liste_plots, type)]
  
  # Pivot
  result <- data.table::dcast(
    agg,
    id_table_liste_plots ~ type,
    value.var = "value"
  )
  
  as_tibble(result)
}

#' Aggregate character plot features
#' @keywords internal
aggregate_character_plot_features <- function(data) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Convert to data.table FIRST
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Clean values
  data[, typevalue_char := str_squish(typevalue_char)]
  
  # Aggregate by plot and type
  agg <- data[!is.na(typevalue_char),
              .(value = paste(unique(typevalue_char), collapse = ", ")),
              by = .(id_table_liste_plots, type)]
  
  # Pivot
  result <- data.table::dcast(
    agg,
    id_table_liste_plots ~ type,
    value.var = "value"
  )
  
  as_tibble(result)
}

#' Aggregate table-referenced features
#' @keywords internal
aggregate_table_plot_features <- function(data, con) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Resolve table references first
  data <- resolve_table_references(data, con)
  
  # Convert to data.table FIRST
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }
  
  # Aggregate
  agg <- data[!is.na(typevalue_char),
              .(value = paste(unique(typevalue_char), collapse = ", ")),
              by = .(id_table_liste_plots, type)]
  
  # Pivot
  result <- data.table::dcast(
    agg,
    id_table_liste_plots ~ type,
    value.var = "value"
  )
  
  as_tibble(result)
}

# -----------------------------------------------------------------------------
# GESTION DES CENSUS
# -----------------------------------------------------------------------------

#' Extract census information
#' @keywords internal
extract_census_info <- function(data) {
  
  census_data <- data %>%
    filter(type == "census") %>%
    group_by(id_table_liste_plots) %>%
    summarise(
      number_of_census = max(as.numeric(typevalue), na.rm = TRUE),
      .groups = "drop"
    )
  
  return(census_data)
}

#' Extract and format census dates
#' @keywords internal
extract_census_dates <- function(census_data) {
  
  if (nrow(census_data) == 0) return(NULL)
  
  # Clean and construct dates
  dates_processed <- census_data %>%
    mutate(
      day = coalesce(day, 1),
      month = coalesce(month, 1),
      year = coalesce(year, NA_integer_),
      date = suppressWarnings(lubridate::dmy(paste(day, month, year, sep = "-"))),
      census_number = as.integer(typevalue)
    ) %>%
    filter(!is.na(date)) %>%
    select(id_table_liste_plots, census_number, date)
  
  # Pivot dates (one row per plot, one column per census)
  dates_wide <- dates_processed %>%
    group_by(id_table_liste_plots, census_number) %>%
    summarise(date = first(date), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = census_number,
      values_from = date,
      names_prefix = "date_census_"
    )
  
  # Julian dates (same pattern)
  dates_julian <- dates_processed %>%
    mutate(date_julian = as.numeric(date - as.Date("1800-01-01"))) %>%
    group_by(id_table_liste_plots, census_number) %>%
    summarise(date_julian = first(date_julian), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = census_number,
      values_from = date_julian,
      names_prefix = "date_census_julian_"
    )
  
  # Combine
  full_join(dates_wide, dates_julian, by = "id_table_liste_plots")
}

# -----------------------------------------------------------------------------
# LEGACY WRAPPER FOR BACKWARD COMPATIBILITY
# -----------------------------------------------------------------------------

#' Subplot features of a set of plots (superseded)
#'
#' `r lifecycle::badge("superseded")`
#'
#' Kept so that existing code and the census-information module keep running.
#' It is a thin wrapper: it calls [query_plot_features()] and renames the three
#' elements of the result to their old names.
#'
#' The plot-filtering arguments are gone. They were served by an internal
#' helper that the October 2025 rewrite of [query_plots()] deleted without
#' updating this call site, so they have raised
#' `could not find function ".build_plot_query"` ever since and never filtered
#' anything. Resolve the plots with [query_plots()] and pass the ids instead.
#'
#' @param ids_plots Integer vector of `data_liste_plots.id_liste_plots`.
#' @param ids_subplots Integer vector of `data_liste_sub_plots.id_sub_plots`.
#' @param plot_name,country,locality_name,method
#'   `r lifecycle::badge("deprecated")` Never applied; use [query_plots()] to
#'   resolve the plots and pass their ids as `ids_plots`.
#' @param subtype Character. Subplot type to keep, matched as a regular
#'   expression.
#' @param verbose Logical. Whether to announce that the legacy wrapper is in
#'   use.
#' @param extract_subplots_obs_features Logical. Also fetch the features
#'   attached to subplot observations.
#' @param con A DBI connection or pool. Opened with [call.mydb()] if `NULL`.
#'
#' @return A list of `all_subplots`, `all_subplot_pivot` and `census_features`,
#'   the old names of `features_raw`, `features_aggregated` and `census_info`.
#'
#' @seealso [query_plot_features()], which returns the same data under its
#'   current names.
#' @keywords internal
#' @export
query_subplots <- function(
    ids_plots = NULL,
    ids_subplots = NULL,
    plot_name = lifecycle::deprecated(),
    country = lifecycle::deprecated(),
    locality_name = lifecycle::deprecated(),
    method = lifecycle::deprecated(),
    subtype = NULL,
    verbose = TRUE,
    extract_subplots_obs_features = FALSE,
    con = NULL
) {

  if (verbose) {
    cli::cli_alert_info("Using legacy wrapper - consider migrating to query_plot_features()")
  }

  # These four never reached a working query, so there is no behaviour to keep
  # and nothing to warn about gently: stopping is the only honest answer.
  supplied <- c(
    plot_name     = lifecycle::is_present(plot_name),
    country       = lifecycle::is_present(country),
    locality_name = lifecycle::is_present(locality_name),
    method        = lifecycle::is_present(method)
  )

  if (any(supplied)) {
    lifecycle::deprecate_stop(
      when = "1.9.8",
      what = paste0("query_subplots(", names(supplied)[supplied][1], ")"),
      with = "query_plots()",
      details = paste(
        "Filtering plots here has never worked: the helper behind these",
        "arguments was removed in 1.9.0. Resolve the plots first, then pass",
        'their ids, e.g. query_subplots(ids_plots = query_plots(country =',
        '"Gabon")$id_liste_plots).'
      )
    )
  }

  # The removed branch used to turn "no ids and no filters" into every plot in
  # the database. Ask for that explicitly rather than by omission.
  if (is.null(ids_plots) && is.null(ids_subplots)) {
    cli::cli_abort(c(
      "{.arg ids_plots} or {.arg ids_subplots} is required.",
      i = "To fetch the features of every plot, call {.fn query_plot_features} directly."
    ))
  }

  # Call new function
  result <- query_plot_features(
    plot_ids = ids_plots,
    subplot_ids = ids_subplots,
    subplot_type = subtype,
    format = "wide",
    include_subplot_obs_features = extract_subplots_obs_features,
    con = con
  )

  # Return in old format
  list(
    all_subplots = result$features_raw,
    all_subplot_pivot = result$features_aggregated,
    census_features = result$census_info
  )
}

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

#' List all available subplot types
#' @export
subplot_list <- function(con = NULL) {
  
  if (is.null(con)) con <- call.mydb()
  
  DBI::dbGetQuery(con, "
    SELECT
      type,
      valuetype,
      typedescription,
      expectedunit,
      minallowedvalue,
      maxallowedvalue,
      id_subplotype,
      category
    FROM subplotype_list
    ORDER BY category, type
  ") %>%
    as_tibble()
}



# List of subplot features
#
# Provide list of subplot features of plot
#
# @return A tibble of all subplot features
#
# @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
# subplot_list <- function() {
#
#   mydb <- call.mydb()
#
#   nn <-
#     try_open_postgres_table(table = "subplotype_list", con = mydb) %>%
#     dplyr::collect()
#
#   # dbDisconnect(mydb)
#   return(nn)
# }
#
#
#
#.sql_query_subplot <- function(id_subplots, 
#                               mydb_ = mydb, 
#                               tbl = "data_liste_sub_plots", 
#                               tbl2 = "subplotype_list") {
#  
#  sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.id_type_sub_plot = {`tbl2`}.id_subplotype WHERE id_subplotype IN ({vals*})",
#                        vals = id_subplots, .con = mydb_)
#  return(sql)
#}
#
#
#
#.sql_join_subplots_clause <- function(tbl, tbl2, con) {
#  glue::glue_sql(
#    "{`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.id_type_sub_plot = {`tbl2`}.id_subplotype",
#    tbl = tbl,
#    tbl2 = tbl2,
#    .con = con
#  )
#}
#
#sql_query_subplots_by_plot_ids <- function(id_plots, 
#                                           mydb_ = mydb, 
#                                           tbl = "data_liste_sub_plots", 
#                                           tbl2 = "subplotype_list") {
#  stopifnot(length(id_plots) > 0)
#  
#  join_clause <- .sql_join_subplots_clause(tbl, tbl2, con = mydb_)
#  
#  glue::glue_sql(
#    "SELECT * FROM {join_clause} WHERE {`tbl`}.id_table_liste_plots IN ({vals*})",
#    join_clause = join_clause,
#    tbl = tbl,
#    vals = id_plots,
#    .con = mydb_
#  )
#}
#
#sql_query_subplots_by_plot_and_subplot_ids <- function(id_plots,
#                                                      id_subplots,
#                                                      mydb_ = mydb,
#                                                      tbl = "data_liste_sub_plots",
#                                                      tbl2 = "subplotype_list") {
#  stopifnot(length(id_plots) > 0, length(id_subplots) > 0)
#  
#  join_clause <- .sql_join_subplots_clause(tbl, tbl2, con = mydb_)
#  
#  glue::glue_sql(
#    "SELECT * FROM {join_clause} WHERE {`tbl`}.id_table_liste_plots IN ({vals*}) AND {`tbl`}.id_sub_plots IN ({vals2*})",
#    join_clause = join_clause,
#    tbl = tbl,
#    vals = id_plots,
#    vals2 = id_subplots,
#    .con = mydb_
#  )
#}
#
#
#.build_sql_query_subplots <- function(ids_plots, ids_subplots, con) {
#  if (!is.null(ids_plots) && is.null(ids_subplots)) {
#    return(sql_query_subplots_by_plot_ids(id_plots = ids_plots, mydb_ = con))
#  } else if (!is.null(ids_plots) && !is.null(ids_subplots)) {
#    return(sql_query_subplots_by_plot_and_subplot_ids(id_plots = ids_plots, id_subplots = ids_subplots, mydb_ = con))
#  } else if (is.null(ids_plots) && !is.null(ids_subplots)) {
#    return(.sql_query_subplot(id_subplots = ids_subplots, mydb_ = con))
#  } else {
#    cli::cli_abort("No values for plots or subplots queried")
#  }
#}
#
#
#
#


#query_subplots <- function(ids_plots = NULL,
#                           ids_subplots = NULL,
#                           plot_name = NULL,
#                           country = NULL,
#                           locality_name = NULL,
#                           method = NULL,
#                           subtype = NULL,
#                           verbose = TRUE,
#                           extract_subplots_obs_features = FALSE
#) {
#  
#  mydb <- call.mydb()
#  
#  if (is.null(ids_plots) & is.null(ids_subplots)) {
#    
#    queried_plots_sql <- 
#      .build_plot_query(con = mydb,
#                      plot_name = plot_name,
#                      country = country,
#                      locality_name = locality_name,
#                      method = method)
#    
#    queried_plots <- func_try_fetch(con = mydb, sql = queried_plots_sql)
#    
#    ids_plots <- queried_plots$id_liste_plots
#    
#  }
#  
#  sql <- .build_sql_query_subplots(ids_plots, ids_subplots, con = mydb)
#  
#  sub_plot_data <-
#    suppressMessages(func_try_fetch(con = mydb, sql = sql)
#    )
#  
#  nbe_subplot_data <- nrow(dplyr::distinct(sub_plot_data,
#                                           id_table_liste_plots))
#  
#  if (verbose) {
#    cli::cli_alert_info("{length(ids_plots)} plots selected")
#    cli::cli_alert_info("subplot_features found for {nbe_subplot_data} plots")}
#  
#  if (nbe_subplot_data > 0) {
#    
#    if (!is.null(subtype)) {
#      
#      sub_plot_data <-
#        sub_plot_data %>%
#        dplyr::filter(grepl(subtype, type))
#      
#      cli::cli_alert_info("Selected subplot features: {all_sub_type$type}")
#      
#      
#    }
#    
#    extracted_data <-
#      sub_plot_data %>%
#      # dplyr::left_join(all_sub_type,
#      #                  by = c("id_type_sub_plot" = "id_type_sub_plot")) %>%
#      # dplyr::collect() %>%
#      dplyr::select(id_table_liste_plots,
#                    year,
#                    month,
#                    day,
#                    type,
#                    valuetype,
#                    typevalue,
#                    typevalue_char,
#                    original_subplot_name,
#                    id_sub_plots,
#                    comment)
#    
#    if (extract_subplots_obs_features) {
#      
#      feats <- 
#        query_subplot_observations_feat(id_sub_plots = extracted_data$id_sub_plots)
#      
#      if (any(!is.na(feats$all_feat_pivot))) {
#        
#        feats_unique <-
#          feats$all_feat_pivot %>%
#          mutate(id_subplot_feat = as.character(id_subplot_feat)) %>%
#          group_by(id_sub_plots) %>%
#          summarise(across(where(is.numeric), ~mean(., na.rm = T)),
#                    across(where(is.character), ~paste(.[!is.na(.)], collapse = "|"))) %>%
#          mutate(across(where(is.character), ~na_if(.x, "")))
#        
#        extracted_data <-
#          extracted_data %>%
#          dplyr::left_join(feats_unique,
#                           by = c("id_sub_plots" = "id_sub_plots"))
#      }
#    }
#    
#    numeric_subplots_pivot <- 
#      build_numeric_pivot(dataset = extracted_data)
#    
#    character_subplots_pivot <- 
#      build_character_pivot(dataset = extracted_data)
#    
#    if (any(extracted_data$type == "census")) {
#      
#      census_plots <-
#        extracted_data %>%
#        dplyr::filter(type == "census")
#   
#      
#      # max census for each plot
#      census_plots_nbr <-
#        census_plots %>%
#        dplyr::group_by(id_table_liste_plots) %>%
#        dplyr::summarise(number_of_census = max(typevalue, na.rm = T))
#      
#      census_features <-
#        census_plots 
#      
#      census_dates_lists <- 
#        build_census_dates(census_features = census_features)
#      
#      census_plots_nbr <- 
#        census_features %>%
#        group_by(id_table_liste_plots) %>%
#        summarise(number_of_census = max(typevalue, na.rm = TRUE), .groups = "drop")
#      
#      # nbr_census <- dplyr::distinct(census_features, typevalue)
#      # census_dates_lists <- vector('list', nrow(nbr_census))
#      # for (i in 1:nrow(nbr_census)) {
#      #   
#      #   census_features_selected <-
#      #     census_features %>%
#      #     dplyr::filter(typevalue == i)
#      #   
#      #   census_features_selected <-
#      #     census_features_selected %>%
#      #     dplyr::mutate(date =
#      #                     paste(ifelse(!is.na(month),
#      #                                  month, 1), # if day is missing, by default 1
#      #                           ifelse(!is.na(day),
#      #                                  day, 1), # if month is missing, by default 1
#      #                           ifelse(!is.na(year),
#      #                                  year, ""),
#      #                           sep = "/")) %>%
#      #     dplyr::mutate(date_julian = date::as.date(date))
#      #   # %>%
#      #   #   dplyr::select(date, date_julian, id_table_liste_plots)
#      #   
#      #   date_name <- paste0("date_census_", i)
#      #   date_name_enquo1 <-
#      #     rlang::parse_expr(rlang::quo_name(rlang::enquo(date_name)))
#      #   date_name <- paste0("date_census_julian_", i)
#      #   date_name_enquo2 <-
#      #     rlang::parse_expr(rlang::quo_name(rlang::enquo(date_name)))
#      #   
#      #   census_features_selected <-
#      #     census_features_selected %>%
#      #     dplyr::rename(!!date_name_enquo1 := date) %>%
#      #     dplyr::rename(!!date_name_enquo2 := date_julian)
#      #   
#      #   census_dates_lists[[i]] <- 
#      #     census_features_selected %>% 
#      #     dplyr::select(id_table_liste_plots, 
#      #                   starts_with("date_census_"))
#      #   
#      # }
#      
#    } else {
#      census_dates_lists <- NULL
#      census_plots_nbr <- NULL
#      census_features <- NA
#    }
#    
#    if (any(grepl("table_", extracted_data$valuetype))) {
#      
#      table_valutype_list <- 
#        build_table_pivot(extracted_data = extracted_data, con = mydb)
#      
#    } else {
#      table_valutype_list <- NULL
#    }
#    
#    all_subplot_pivot <-
#      c(list(census_dates_lists),
#        list(table_valutype_list),
#        list(census_plots_nbr),
#        list(character_subplots_pivot),
#        list(numeric_subplots_pivot))
#    
#    all_subplot_pivot <-
#      purrr::reduce(all_subplot_pivot[!unlist(lapply(all_subplot_pivot, is.null))],
#                    dplyr::full_join,
#                    by = 'id_table_liste_plots')
#    
#    return(list(all_subplots = extracted_data,
#                all_subplot_pivot = all_subplot_pivot,
#                census_features = census_features))
#    
#  } else {
#    
#    return(list(all_subplots = NA,
#                all_subplot_pivot = NA,
#                census_features = NA))
#  }
#}
#
#build_numeric_pivot <- function(dataset) {
#  if (!any(dataset$valuetype == "numeric")) return(NULL)
#  dataset %>%
#    filter(valuetype == "numeric", type != "census") %>%
#    select(id_table_liste_plots, typevalue, type) %>%
#    tidyr::pivot_wider(names_from = "type",
#                values_from = "typevalue",
#                values_fn = ~ mean(.x, na.rm = TRUE))
#}
#
#build_character_pivot <- function(dataset) {
#  if (!any(dataset$valuetype == "character")) return(NULL)
#  dataset %>%
#    filter(valuetype == "character", type != "census") %>%
#    select(id_table_liste_plots, typevalue_char, type) %>%
#    tidyr::pivot_wider(names_from = "type",
#                values_from = "typevalue_char",
#                values_fn = ~ paste(.x, collapse = "|"))
#}
#
#
#build_table_pivot <- function(extracted_data, con) {
#  table_rows <- extracted_data %>%
#    filter(grepl("^table_", valuetype))
#  
#  if (nrow(table_rows) == 0) return(NULL)
#  
#  # Définir dynamiquement les correspondances valuetype → (ID, label)
#  table_metadata <- list(
#    table_colnam = list(id = "id_table_colnam", label = "colnam")
#    # Tu peux en ajouter d'autres ici
#    # table_species = list(id = "id_species", label = "latin_name")
#  )
#  
#  # Résultat final
#  all_pivots <- list()
#  
#  for (valtype in unique(table_rows$valuetype)) {
#    if (!valtype %in% names(table_metadata)) {
#      warning(glue::glue("Unknown valuetype: {valtype}, skipping..."))
#      next
#    }
#    
#    ids_col <- table_metadata[[valtype]]$id
#    label_col <- table_metadata[[valtype]]$label
#    
#    table_df <- tbl(con, valtype) %>% 
#      select(all_of(c(ids_col, label_col))) %>% 
#      collect()
#    
#    joined <- table_rows %>%
#      filter(valuetype == valtype) %>%
#      left_join(table_df, by = c("typevalue" = ids_col)) %>%
#      mutate(typevalue_char = .data[[label_col]])
#    
#    pivot <- joined %>%
#      select(id_table_liste_plots, typevalue_char, type) %>%
#      tidyr::pivot_wider(
#        names_from = type,
#        values_from = typevalue_char,
#        values_fn = ~ paste(unique(.x[!is.na(.x)]), collapse = ", ")
#      )
#    
#    all_pivots[[valtype]] <- pivot
#  }
#  
#  # Combine tous les pivots
#  Reduce(function(x, y) full_join(x, y, by = "id_table_liste_plots"),
#         all_pivots)
#}




#' Add a type in subplot table
#'
#' Add feature and associated descriptors in subplot list table
#'
#' @return Invisibly returns the new feature data (tibble)
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_type string value with new type descriptors - try to avoid space
#' @param new_valuetype string one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character', 'table_colnam'
#' @param new_maxallowedvalue numeric if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue numeric if valuetype is numeric, indicate the minimum allowed value
#' @param new_typedescription string full description of type/feature
#' @param new_factorlevels string a vector of all possible value if valuetype is categorical or ordinal
#' @param new_expectedunit string expected unit (unitless if none)
#' @param new_comments string any comments
#' @param new_category string category for grouping features in the UI (e.g., "People", "Sampling", "Environment", "Observation"). Defaults to "Other".
#' @param con Database connection (optional). If NULL, will create a new connection.
#' @param interactive Logical. If TRUE (default), will ask for confirmation. Set to FALSE when calling from Shiny.
#'
#' @export
add_subplottype <- function(new_type = NULL,
                            new_valuetype = NULL,
                            new_maxallowedvalue = NULL,
                            new_minallowedvalue = NULL,
                            new_typedescription = NULL,
                            new_factorlevels = NULL,
                            new_expectedunit = NULL,
                            new_comments = NULL,
                            new_category = NULL,
                            con = NULL,
                            interactive = TRUE) {

  if(is.null(new_type)) stop("define new type")
  if(is.null(new_valuetype)) stop("define new_valuetype")

  # Use provided connection or create new one
  if (is.null(con)) {
    mydb <- call.mydb()
    should_disconnect <- TRUE
  } else {
    mydb <- con
    should_disconnect <- FALSE
  }

  # Handle pool connections
  actual_con <- if (inherits(mydb, "Pool")) {
    pool::poolCheckout(mydb)
  } else {
    mydb
  }

  # Ensure we return the connection to pool if needed
  on.exit({
    if (inherits(mydb, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Check if type already exists
  if(try_open_postgres_table(table = "subplotype_list", con = actual_con) %>%
     dplyr::distinct(type) %>%
     dplyr::filter(type == !!new_type) %>%
     dplyr::collect() %>%
     nrow()>0)  stop("new type already in table")

  # Validate valuetype
  if (!any(new_valuetype==c('numeric',
                            'integer',
                            'categorical',
                            'ordinal',
                            'logical',
                            'character',
                            'table_colnam'))) stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character' or 'table_colnam'")

  # Validate min/max for numeric types
  if (new_valuetype=="numeric" | new_valuetype=="integer")
    if (!is.null(new_maxallowedvalue) && !is.numeric(new_maxallowedvalue) & !is.integer(new_maxallowedvalue))
      stop("valuetype numeric or integer and max value not of this type")
  if (new_valuetype=="numeric" | new_valuetype=="integer")
    if (!is.null(new_minallowedvalue) && !is.numeric(new_minallowedvalue) & !is.integer(new_minallowedvalue))
      stop("valuetype numeric or integer and min value not of this type")

  # Build new row
  new_data_renamed <- dplyr::tibble(
    type = new_type,
    valuetype = new_valuetype,
    maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
    minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
    typedescription = ifelse(is.null(new_typedescription), NA, new_typedescription),
    factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
    expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
    comments = ifelse(is.null(new_comments), NA, new_comments),
    category = ifelse(is.null(new_category), "Other", new_category)
  )

  # Only print and ask for confirmation if interactive
  if (interactive) {
    print(new_data_renamed)
    Q <- choose_prompt(message = "confirm adding this type?")
  } else {
    Q <- TRUE  # Auto-confirm when non-interactive (e.g., from Shiny)
  }

  if(Q) {
    DBI::dbWriteTable(actual_con, "subplotype_list", new_data_renamed, append = TRUE, row.names = FALSE)
  }

  invisible(new_data_renamed)
}
# 
# 
# 
# 
# 
# 
# 



# # List, selected trait measures features
# #
# # Table of srait measures features
# #
# # @return A tibble of all subplots
# #
# # @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
# #
# #
# # @param id_trait_measures integer
# #
# #
# # @export
# query_subplot_observations_feat <- function(id_sub_plots  = NULL,
#                                             pivot_table = TRUE) {
# 
#   mydb <- call.mydb()
# 
#   feat_data <-
#     try_open_postgres_table(table = "data_subplot_feat", con = mydb) %>%
#     dplyr::filter(id_sub_plots %in% !!id_sub_plots)
# 
#   nbe_feat_data <- nrow(feat_data %>%
#                           dplyr::collect())
# 
#   if (nbe_feat_data  > 0) {
# 
#     all_sub_type <-
#       feat_data %>%
#       dplyr::distinct(id_type_sub_plot) %>%
#       dplyr::left_join(
#         dplyr::tbl(mydb, "subplotype_list") %>%
#           dplyr::select(type, valuetype, typedescription, id_subplotype),
#         by = c("id_type_sub_plot" = "id_subplotype")
#       )
# 
#     extracted_data <-
#       feat_data %>%
#       dplyr::left_join(all_sub_type,
#                        by = c("id_type_sub_plot" = "id_type_sub_plot")) %>%
#       dplyr::collect() %>%
#       dplyr::select(id_sub_plots,
#                     type,
#                     valuetype,
#                     typevalue,
#                     typevalue_char,
#                     id_subplot_feat)
# 
#     # extracted_data <- extracted_data %>%
#     #   group_by(id_trait_measures, trait) %>%
#     #   summarise(valuetype = first(valuetype),
#     #             typevalue = mean(typevalue, na.rm = T),
#     #             n = n()) %>%
#     #   filter(n > 1)
# 
#     if (pivot_table) {
# 
#       if (any(extracted_data$valuetype == "numeric")) {
# 
#         numeric_subplots_pivot <-
#           extracted_data %>%
#           filter(valuetype == "numeric") %>%
#           select(id_sub_plots, typevalue, type, id_subplot_feat) %>%
#           tidyr::pivot_wider(
#             names_from = "type",
#             values_from = "typevalue",
#             values_fn = ~ mean(.x, na.rm = TRUE)
#           ) %>%
#           mutate(id_subplot_feat = as.character(id_subplot_feat))
# 
#       } else {
# 
#         numeric_subplots_pivot <- NULL
# 
#       }
# 
#       if (any(extracted_data$valuetype == "character")) {
# 
#         character_feat_pivot <-
#           extracted_data %>%
#           filter(valuetype == "character") %>%
#           select(id_sub_plots, typevalue_char, type, id_subplot_feat)  %>%
#           tidyr::pivot_wider(
#             names_from = "type",
#             values_from = "typevalue_char",
#             values_fn = ~ paste(.x, collapse = "|")
#           ) %>%
#           mutate(id_subplot_feat = as.character(id_subplot_feat))
# 
#       } else {
#         character_feat_pivot <- NULL
#       }
# 
#       if (any(extracted_data$valuetype == "ordinal")) {
#         ordinal_subplots_pivot <-
#           extracted_data %>%
#           filter(valuetype == "ordinal") %>%
#           select(id_sub_plots, typevalue_char, type, id_subplot_feat)  %>%
#           tidyr::pivot_wider(
#             names_from = "type",
#             values_from = "typevalue_char",
#             values_fn = ~ paste(.x, collapse = "|")
#           ) %>%
#           mutate(id_subplot_feat = as.character(id_subplot_feat))
#       } else {
#         ordinal_subplots_pivot <- NULL
#       }
# 
# 
#       if (any(grepl("table_colnam", extracted_data$valuetype))) {
# 
#         table_ids_subplots <- extracted_data %>%
#           filter(grepl("table_", valuetype))
# 
#         allvalutype <- distinct(table_ids_subplots, type)
# 
#         table_valutype_list <- vector('list', nrow(allvalutype))
#         for (i in 1:nrow(allvalutype)) {
# 
#           table_ids_subplots_filt <-
#             table_ids_subplots %>%
#             filter(type == allvalutype$type[i])
# 
#           ids_ <-
#             case_when(
#               table_ids_subplots_filt$valuetype[i] == "table_colnam" ~ "id_table_colnam"
#             )
# 
#           col_to_keep_ <-
#             case_when(
#               table_ids_subplots_filt$valuetype[i] == "table_colnam" ~ "colnam"
#             )
# 
#           table_collected <-
#             tbl(mydb, table_ids_subplots_filt$valuetype[i]) %>%
#             collect()
# 
#           table_ids_subplots_filt <-
#             table_ids_subplots_filt %>%
#             left_join(table_collected %>%
#                         dplyr::select(all_of(c(col_to_keep_, ids_))),
#                       by = c("typevalue" = ids_)) %>%
#             mutate(typevalue_char = !!rlang::parse_expr(col_to_keep_)) %>%
#             dplyr::select(-all_of(col_to_keep_))
# 
#           table_ids_subplots_piv <-
#             table_ids_subplots_filt %>%
#             select(id_sub_plots, typevalue_char, type, id_subplot_feat) %>%
#             mutate(id_subplot_feat = as.character(id_subplot_feat)) %>%
#             tidyr::pivot_wider(
#               names_from = "type",
#               values_from = c("typevalue_char", "id_subplot_feat"),
#               values_fn = ~ paste(., collapse = "|")
#             )
# 
#           names(table_ids_subplots_piv) <-
#             gsub("typevalue_char_", "", names(table_ids_subplots_piv))
# 
#           names(table_ids_subplots_piv)[which(grepl("id_subplot_feat", names(table_ids_subplots_piv)))] <-
#             "id_subplot_feat"
# 
#           table_valutype_list[[i]] <-
#             table_ids_subplots_piv
#         }
# 
#       } else {
#         table_valutype_list <- NULL
#       }
# 
#       # all_feat_pivot <-
#       #   c(list(character_subplots_pivot),
#       #     list(numeric_subplots_pivot),
#       #     table_valutype_list)
# 
#       all_feat_pivot <-
#         bind_rows(list(list(character_feat_pivot),
#                        list(numeric_subplots_pivot),
#                        list(ordinal_subplots_pivot),
#                        table_valutype_list))
# 
#     }else {
# 
#       all_feat_pivot <- extracted_data
#     }
# 
# 
# 
#     # all_feat_pivot <-
#     #   purrr::reduce(all_feat_pivot[!unlist(lapply(all_feat_pivot, is.null))],
#     #                 dplyr::full_join,
#     #                 by = c('id_trait_measures'))
# 
# 
#   } else {
# 
#     all_feat_pivot <- NA
# 
#   }
# 
#   return(list(all_feat_pivot = all_feat_pivot))
# 
# }
# 
# 
# 
# build_census_dates <- function(census_features) {
#   # Nettoyage + construction des dates
#   census_features <- census_features %>%
#     mutate(
#       day = coalesce(day, 1),
#       month = coalesce(month, 1),
#       year = coalesce(year, NA_integer_),
#       date = suppressWarnings(lubridate::dmy(paste(day, month, year, sep = "-"))),
#       census_number = as.integer(typevalue)
#     ) %>%
#     filter(!is.na(date)) %>%
#     select(id_table_liste_plots, census_number, date)
# 
#   # Création des colonnes dynamiques
#   census_dates_wide <- census_features %>%
#     mutate(
#       col_date = paste0("date_census_", census_number),
#       col_julian = paste0("date_census_julian_", census_number)
#     ) %>%
#     group_by(id_table_liste_plots) %>%
#     summarise(
#       across(c(col_date, col_julian), ~ list(unique(date)), .names = "{.col}"),
#       .groups = "drop"
#     )
# 
#   # Remise en forme : une colonne par recensement
#   dates_wide <- census_features %>%
#     tidyr::pivot_wider(
#       names_from = census_number,
#       values_from = date,
#       names_prefix = "date_census_"
#     )
# 
#   dates_julian_wide <-
#     census_features %>%
#     mutate(date_julian = as.numeric(date - as.Date("1800-01-01"))) %>%
#     tidyr::pivot_wider(
#       names_from = census_number,
#       values_from = date_julian,
#       names_prefix = "date_census_julian_"
#     )
# 
#   dates_julian_wide <-
#     dates_julian_wide %>%
#     group_by(id_table_liste_plots) %>%
#     summarise(across(where(is.numeric), ~first(., na_rm = T))) %>%
#     ungroup()
# 
#   # Jointure finale
#   full_join(dates_wide, dates_julian_wide, by = "id_table_liste_plots")
# }



