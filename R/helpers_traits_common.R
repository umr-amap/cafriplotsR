# =============================================================================
# HELPERS COMMUNS POUR L'EXTRACTION ET AGRÉGATION DE TRAITS
# Utilisés à la fois pour traits individuels et traits taxonomiques
# =============================================================================

#' Resolve taxonomic synonyms
#' 
#' Replaces taxon IDs with their accepted names based on synonym resolution.
#' Based on the logic from merge_individuals_taxa.
#'
#' @param idtax Vector of taxon IDs (can be synonyms)
#' @param include_synonyms If TRUE, also returns traits for all synonyms
#' @param con_taxa Connection to taxa database
#' @return Tibble with columns: idtax, idtax_good
#' @keywords internal
#' @export
resolve_taxon_synonyms <- function(idtax = NULL, 
                                   include_synonyms = TRUE,
                                   con_taxa = NULL) {
  
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  
  # Get synonym resolution table
  synonym_table <- dplyr::tbl(con_taxa, "table_taxa") %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::collect()
  
  # Create resolved mapping
  idtax_mapping <- synonym_table %>%
    dplyr::mutate(idtax_resolved = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n))
  
  if (is.null(idtax)) {
    # No filter - return all mappings
    return(idtax_mapping %>%
             dplyr::select(idtax = idtax_n, idtax_good = idtax_resolved))
  }
  
  # Filter to requested taxa
  result <- idtax_mapping %>%
    dplyr::filter(idtax_n %in% !!idtax)
  
  # If include_synonyms, also get all taxa that resolve to the same good names
  if (include_synonyms) {
    # Get the resolved IDs for input taxa
    resolved_ids <- unique(result$idtax_resolved)
    
    # Find all taxa (including synonyms) that resolve to these IDs
    all_related <- idtax_mapping %>%
      dplyr::filter(idtax_resolved %in% !!resolved_ids)
    
    cli::cli_alert_info(
      "Including synonyms: {nrow(result)} taxa expanded to {nrow(all_related)} taxa"
    )
    
    result <- all_related
  }
  
  result %>%
    dplyr::select(idtax = idtax_n, idtax_good = idtax_resolved) %>%
    dplyr::distinct()
}

#' Pivot numeric trait data to wide format with statistics
#' 
#' Generic function to pivot numeric traits and calculate statistics (mean, sd, n).
#' Uses data.table for performance.
#'
#' @param data Data frame with columns: id_col, trait, traitvalue
#' @param id_col Name of ID column (e.g., "idtax", "id_data_individuals")
#' @param include_stats If TRUE, calculates mean, sd, and n for each trait
#' @param include_id_measures If TRUE, concatenates id_trait_measures
#' @param name_prefix Prefix to add to trait names
#' @return Tibble in wide format
#' 
#' @importFrom data.table setDT is.data.table dcast setnames
#' @keywords internal
#' @export
pivot_numeric_traits_generic <- function(
    data,
    id_col,
    include_stats = TRUE,
    include_id_measures = TRUE,
    name_prefix = ""
) {
  
  if (nrow(data) == 0) return(NULL)
  
  # Convert to data.table for performance
  if (!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Convert traitvalue to numeric
  data[, traitvalue_num := as.numeric(traitvalue)]
  
  if (!include_stats) {
    # Simple pivot without stats
    result <- data.table::dcast(
      data,
      as.formula(paste(id_col, "~ trait")),
      value.var = "traitvalue_num",
      fun.aggregate = function(x) mean(x, na.rm = TRUE)
    )
    
    # Add prefix
    data.table::setnames(
      result,
      setdiff(names(result), id_col),
      paste0(name_prefix, setdiff(names(result), id_col))
    )
    
    return(as_tibble(result))
  }
  
  # Calculate statistics (mean, sd, n)
  stats_dt <- data[, .(
    mean = mean(traitvalue_num, na.rm = TRUE),
    sd = sd(traitvalue_num, na.rm = TRUE),
    n = sum(!is.na(traitvalue_num))
  ), by = c(id_col, "trait")]
  
  # Pivot statistics
  stats_wide <- data.table::dcast(
    stats_dt,
    as.formula(paste(id_col, "~ trait")),
    value.var = c("mean", "sd", "n"),
    sep = "_"
  )
  
  # Add prefix to all columns except id_col
  data.table::setnames(
    stats_wide,
    setdiff(names(stats_wide), id_col),
    paste0(name_prefix, setdiff(names(stats_wide), id_col))
  )
  
  # Add concatenated id_trait_measures if requested
  if (include_id_measures && "id_trait_measures" %in% names(data)) {
    id_measures <- data[, .(
      ids = paste(na.omit(unique(as.character(id_trait_measures))), collapse = ", ")
    ), by = c(id_col, "trait")]
    
    id_wide <- data.table::dcast(
      id_measures,
      as.formula(paste(id_col, "~ trait")),
      value.var = "ids"
    )
    
    data.table::setnames(
      id_wide,
      setdiff(names(id_wide), id_col),
      paste0("id_trait_measures_", name_prefix, setdiff(names(id_wide), id_col))
    )
    
    stats_wide <- merge(stats_wide, id_wide, by = id_col, all = TRUE)
  }
  
  as_tibble(stats_wide)
}

#' Pivot categorical trait data to wide format
#' 
#' Generic function to pivot categorical/character traits.
#' Uses data.table for performance.
#'
#' @param data Data frame with columns: id_col, trait, traitvalue_char
#' @param id_col Name of ID column
#' @param aggregation_mode "mode" (most frequent) or "concat" (all unique values)
#' @param include_id_measures If TRUE, concatenates id_trait_measures
#' @param name_prefix Prefix to add to trait names
#' @return Tibble in wide format
#' @keywords internal
#' @export
pivot_categorical_traits_generic <- function(
    data,
    id_col,
    aggregation_mode = c("mode", "concat"),
    include_id_measures = TRUE,
    name_prefix = ""
) {
  
  if (nrow(data) == 0) return(NULL)
  
  aggregation_mode <- match.arg(aggregation_mode)
  
  if (!data.table::is.data.table(data)) data.table::setDT(data)
  
  # Clean values
  data[, traitvalue_char := str_squish(traitvalue_char)]
  
  # Concatenate id_trait_measures first
  if (include_id_measures && "id_trait_measures" %in% names(data)) {
    id_measures <- data[, .(
      ids = paste(na.omit(unique(as.character(id_trait_measures))), collapse = ", ")
    ), by = c(id_col, "trait")]
    
    id_wide <- data.table::dcast(
      id_measures,
      as.formula(paste(id_col, "~ trait")),
      value.var = "ids"
    )
    
    data.table::setnames(
      id_wide,
      setdiff(names(id_wide), id_col),
      paste0("id_trait_measures_", name_prefix, setdiff(names(id_wide), id_col))
    )
  }
  
  # Aggregate values based on mode
  if (aggregation_mode == "mode") {
    # Get most frequent value per (id, trait)
    agg_dt <- data[!is.na(traitvalue_char), .N, by = c(id_col, "trait", "traitvalue_char")]
    agg_dt <- agg_dt[order(get(id_col), trait, -N)]
    agg_dt <- agg_dt[, .SD[1], by = c(id_col, "trait")]
    
  } else {
    # Concatenate all unique values
    agg_dt <- data[!is.na(traitvalue_char), .(
      traitvalue_char = paste(unique(traitvalue_char), collapse = ", ")
    ), by = c(id_col, "trait")]
  }
  
  # Pivot
  result <- data.table::dcast(
    agg_dt,
    as.formula(paste(id_col, "~ trait")),
    value.var = "traitvalue_char"
  )
  
  # Add prefix
  data.table::setnames(
    result,
    setdiff(names(result), id_col),
    paste0(name_prefix, setdiff(names(result), id_col))
  )
  
  # Merge with id_measures
  if (include_id_measures && exists("id_wide")) {
    result <- merge(result, id_wide, by = id_col, all = TRUE)
  }
  
  as_tibble(result)
}

#' Enrich trait data with measurement features (generic)
#' 
#' Adds measurement-level metadata/features to trait data.
#' Only works with long format data.
#'
#' @param data Trait data frame
#' @param src Source: "individuals" or "taxa"
#' @param format Data format: "long" or "wide"
#' @return Enriched data frame
#' @keywords internal
#' @export
enrich_traits_with_measurement_features <- function(
    data,
    src = c("individuals", "taxa"),
    format = c("long", "wide")
) {
  
  src <- match.arg(src)
  format <- match.arg(format)
  
  if (nrow(data) == 0) return(data)
  
  # Cannot add features in wide format
  if (format == "wide") {
    cli::cli_alert_warning(
      "Cannot add measurement features in wide format. Use format='long' instead."
    )
    return(data)
  }
  
  feats <- query_traits_measures_features(
    id_trait_measures = unique(data$id_trait_measures),
    src = src,
    format = "wide"
  )
  
  if (is.null(feats) || nrow(feats) == 0) {
    return(data)
  }
  
  data %>%
    left_join(feats, by = "id_trait_measures")
}

