
#' Add growth forms to a single taxa
#'
#' Add growth form information to a single taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A tibble
#' @export
add_growth_form_taxa <- function(idtax, con = NULL) {

  # Ensure main database connection exists
  if (is.null(con)) {
    if (exists("mydb", envir = .GlobalEnv)) {
      mydb <- get("mydb", envir = .GlobalEnv)
    } else {
      mydb <- call.mydb()
      assign("mydb", mydb, envir = .GlobalEnv)
    }
  } else {
    mydb <- con
  }

  if (length(idtax) > 1)
    stop("Only one taxa at the same time")

  queried_tax <- query_taxa(ids = idtax, class = NULL)

  all_growth_form <- choose_growth_form()

  all_growth_form <- all_growth_form %>%
    dplyr::mutate(idtax = idtax)

  all_growth_form_pivot <-
    all_growth_form %>%
    tidyr::pivot_wider(names_from = trait,
                       values_from = value)

  add_sp_traits_measures(new_data = all_growth_form_pivot,
                         traits_field = names(all_growth_form_pivot)[2:ncol(all_growth_form_pivot)],
                         idtax = "idtax",
                         add_data = TRUE,
                         con = mydb)

}


#' List of trait
#'
#' Provide list of traits available
#'
#' @return A tibble of all traits
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
traits_taxa_list <- function(id_trait = NULL, con = NULL) {

  if (is.null(con)) con <- call.mydb()

  all_colnames_ind <-
    try_open_postgres_table(table = "traitlist", con = con) %>%
    dplyr::select(trait,
                  id_trait,
                  traitdescription,
                  maxallowedvalue,
                  minallowedvalue,
                  expectedunit,
                  valuetype)

  if (is.null(id_trait)) {

    all_colnames_ind <- all_colnames_ind %>%
      dplyr::collect()

  } else {

    all_colnames_ind <- all_colnames_ind %>%
      filter(id_trait == !!id_trait) %>%
      dplyr::collect()

  }

  return(all_colnames_ind)
}



























# =============================================================================
# EXTRACTION DES TRAITS AU NIVEAU TAXONOMIQUE (REFACTORISÉ)
# =============================================================================

#' Query traits at the taxonomic level
#' 
#' Retrieves trait measurements associated with taxa, with automatic resolution
#' of taxonomic synonyms. Traits linked to synonyms are consolidated under the
#' accepted taxon name.
#'
#' @param idtax Vector of taxon IDs to query
#' @param include_synonyms If TRUE, includes traits from all synonyms
#' @param add_taxa_info Add taxonomic information (family, genus, species)
#' @param trait_ids Vector of trait IDs to filter (NULL = all traits)
#' @param categorical_mode How to aggregate categorical traits: "mode" (most frequent) or "concat" (all unique values)
#' @param format Output format: "wide" (pivoted) or "long" (raw measurements)
#' @param include_remarks Include measurement remarks
#' @param include_measurement_features Add measurement-level features/metadata
#' @param con Connection to main database (optional, defaults to call.mydb()).
#'   Used for trait measurements (taxa_traits_measures, traitlist).
#' @param con_taxa Connection to taxa database (optional, defaults to call.mydb.taxa()).
#'   Used for synonym resolution (table_taxa) and taxonomic info enrichment.
#'
#' @return List with components:
#'   - traits_raw: Raw trait measurements with resolved taxonomy
#'   - traits_numeric: Numeric traits (aggregated if format="wide")
#'   - traits_categorical: Categorical traits (aggregated if format="wide")
#'
#' @export
query_taxa_traits <- function(
    idtax = NULL,
    include_synonyms = TRUE,
    add_taxa_info = FALSE,
    trait_ids = NULL,
    categorical_mode = c("mode", "concat"),
    format = c("wide", "long"),
    include_remarks = FALSE,
    include_measurement_features = FALSE,
    include_citation = FALSE,
    con = NULL,
    con_taxa = NULL
) {

  categorical_mode <- match.arg(categorical_mode)
  format <- match.arg(format)

  # Main DB for trait measurements; taxa DB for synonym resolution
  if (is.null(con)) con <- call.mydb()
  if (is.null(con_taxa)) con_taxa <- call.mydb.taxa()
  
  cli::cli_h2("Querying taxa-level traits")
  
  # 1. Fetch raw trait measurements FIRST (to know which taxa have the traits)
  if (!is.null(trait_ids)) {
    cli::cli_alert_info("Fetching trait measurements for trait(s): {paste(trait_ids, collapse=', ')}")
  } else if (!is.null(idtax)) {
    cli::cli_alert_info("Fetching trait measurements for {length(idtax)} taxon/taxa")
  } else {
    cli::cli_alert_info("Fetching all trait measurements")
  }
  
  traits_raw <- fetch_taxa_trait_measurements(
    idtax = idtax,
    trait_ids = trait_ids,
    con = con,
    include_citation = include_citation
  )
  
  if (nrow(traits_raw) == 0) {
    cli::cli_alert_warning("No trait measurements found")
    return(list(
      traits_raw = tibble(),
      traits_numeric = NA,
      traits_categorical = NA
    ))
  }
  
  cli::cli_alert_success("Found {nrow(traits_raw)} measurement(s) for {length(unique(traits_raw$idtax))} taxa")
  
  # 2. Resolve taxonomic synonyms for the taxa that actually have traits
  cli::cli_alert_info("Resolving taxonomic synonyms")
  taxon_mapping <- resolve_taxon_synonyms(
    idtax = unique(traits_raw$idtax),  # ← Only taxa with traits
    include_synonyms = include_synonyms,
    con_taxa = con_taxa
  )
  
  if (nrow(taxon_mapping) == 0) {
    cli::cli_alert_warning("No taxa found after synonym resolution")
    return(list(
      traits_raw = traits_raw %>% as_tibble(),
      traits_numeric = NA,
      traits_categorical = NA
    ))
  }
  
  # 3. Replace taxon IDs with accepted names
  traits_raw <- traits_raw %>%
    left_join(taxon_mapping, by = "idtax") %>%
    mutate(idtax = idtax_good) %>%
    select(-idtax_good)
  
  # 4. Optional: Enrich with measurement features
  if (include_measurement_features) {
    cli::cli_alert_info("Enriching with measurement features")
    traits_raw <- enrich_traits_with_measurement_features(
      traits_raw,
      src = "taxa",
      format = format
    )
  }
  
  # 5. Optional: Add taxonomic information
  if (add_taxa_info) {
    cli::cli_alert_info("Adding taxonomic information")
    traits_raw <- enrich_with_taxa_info(traits_raw, con_taxa) %>% as_tibble()
  }
  
  # 6. Remove remarks if not requested
  if (!include_remarks && "measurementremarks" %in% names(traits_raw)) {
    traits_raw <- traits_raw %>% select(-measurementremarks)
  }
  
  # 7. Process by format
  if (format == "wide") {
    
    cli::cli_h2("Processing traits to wide format")
    
    # Numeric traits
    traits_numeric <- if (any(traits_raw$valuetype == "numeric")) {
      cli::cli_alert_info("Aggregating numeric traits")
      
      
      
      tmp <- pivot_numeric_traits_generic(
        data = traits_raw %>% filter(valuetype == "numeric"),
        id_col = "idtax",
        include_stats = TRUE,
        include_id_measures = TRUE,
        name_prefix = "taxa_"
      )
      
      # 5. Optional: Add taxonomic information
      if (add_taxa_info) {
        cli::cli_alert_info("Adding taxonomic information")
        tmp <- enrich_with_taxa_info(tmp, con_taxa) %>% as_tibble()
      }
      tmp
      
      
    } else {
      NA
    }
    
    # Categorical traits
    traits_categorical <- if (any(traits_raw$valuetype == "categorical")) {
      cli::cli_alert_info("Aggregating categorical traits ({categorical_mode})")
      
      tmp <- pivot_categorical_traits_generic(
        data = traits_raw %>% filter(valuetype == "categorical"),
        id_col = "idtax",
        aggregation_mode = categorical_mode,
        include_id_measures = TRUE,
        name_prefix = "taxa_"
      )
      
      # 5. Optional: Add taxonomic information
      if (add_taxa_info) {
        cli::cli_alert_info("Adding taxonomic information")
        tmp <- enrich_with_taxa_info(tmp, con_taxa) %>% as_tibble()
      }
      tmp
      
    } else {
      NA
    }
    
  } else {
    # Long format - separate by valuetype
    traits_numeric <- traits_raw %>% filter(valuetype == "numeric")
    traits_categorical <- traits_raw %>% filter(valuetype == "categorical")
    
    if (nrow(traits_numeric) == 0) traits_numeric <- NA
    if (nrow(traits_categorical) == 0) traits_categorical <- NA
  }
  
  cli::cli_alert_success("Query completed")
  
  return(list(
    traits_raw = traits_raw %>% as_tibble(),
    traits_numeric = traits_numeric,
    traits_categorical = traits_categorical
  ))
}



#' Fetch raw trait measurements for taxa
#' @keywords internal
fetch_taxa_trait_measurements <- function(idtax, trait_ids = NULL, con,
                                          include_citation = FALSE) {

  # Build query with explicit column selection to avoid duplicates
  citation_select <- if (include_citation) {
    ",
      tc.citation_key,
      tc.authors       AS citation_authors,
      tc.year          AS citation_year,
      tc.title         AS citation_title,
      tc.journal       AS citation_journal,
      tc.doi           AS citation_doi,
      tc.dataset_name  AS citation_dataset_name"
  } else {
    ""
  }

  citation_join <- if (include_citation) {
    "LEFT JOIN table_citations tc ON tm.id_citation = tc.id_citation"
  } else {
    ""
  }

  query <- paste0("
    SELECT
      tm.id_trait_measures,
      tm.idtax,
      tm.traitvalue,
      tm.traitvalue_char,
      tm.basisofrecord,
      tm.measurementremarks,
      tm.fk_id_trait,
      tm.id_citation,
      tl.id_trait,
      tl.trait,
      tl.valuetype,
      tl.traitdescription,
      tl.expectedunit,
      tl.minallowedvalue,
      tl.maxallowedvalue",
    citation_select, "
    FROM taxa_traits_measures tm
    LEFT JOIN traitlist tl ON tm.fk_id_trait = tl.id_trait
    ", citation_join, "
    WHERE 1=1
  ")

  conditions <- character()

  if (!is.null(idtax)) {
    conditions <- c(conditions,
                    glue::glue_sql("tm.idtax IN ({idtax*})", idtax = idtax, .con = con)
    )
  }

  if (!is.null(trait_ids)) {
    conditions <- c(conditions,
                    glue::glue_sql("tl.id_trait IN ({trait_ids*})", trait_ids = trait_ids, .con = con)
    )
  }

  if (length(conditions) > 0) {
    query <- paste(query, "AND", paste(conditions, collapse = " AND "))
  }

  # Use func_try_fetch for automatic retry on connection failures
  func_try_fetch(con = con, sql = query, verbose = FALSE)
}

#' Enrich trait data with taxonomic information
#' @keywords internal
enrich_with_taxa_info <- function(data, con) {
  
  taxa_ids <- unique(data$idtax)
  
  # Use existing add_taxa_table_taxa function
  taxa_info <- add_taxa_table_taxa(ids = taxa_ids) %>%
    dplyr::collect()
  
  data %>%
    left_join(
      taxa_info %>% 
        select(idtax_n, idtax_good_n, tax_fam, tax_gen, tax_esp, 
               tax_sp_level, tax_infra_level, tax_infra_level_auth),
      by = c("idtax" = "idtax_n")
    )
}

# -----------------------------------------------------------------------------
# LEGACY WRAPPER FOR BACKWARD COMPATIBILITY
# -----------------------------------------------------------------------------

#' Legacy function - wrapper for backward compatibility
#' @keywords internal
#' @export
query_traits_measures <- function(
    idtax = NULL,
    idtax_good = NULL,
    add_taxa_info = FALSE,
    id_trait = NULL,
    trait_cat_mode = "most_frequent",
    verbose = TRUE,
    pivot_table = TRUE,
    include_remarks = FALSE,
    extract_trait_measures_features = FALSE,
    con = NULL,
    con_taxa = NULL
) {

  if (verbose) {
    cli::cli_alert_info("Using legacy wrapper - consider migrating to query_taxa_traits()")
  }

  # Map old parameters to new
  categorical_mode <- if (trait_cat_mode == "most_frequent") "mode" else "concat"
  format <- if (pivot_table) "wide" else "long"

  # Call new function
  result <- query_taxa_traits(
    idtax = idtax,
    include_synonyms = is.null(idtax_good),
    add_taxa_info = add_taxa_info,
    trait_ids = id_trait,
    categorical_mode = categorical_mode,
    format = format,
    include_remarks = include_remarks,
    include_measurement_features = extract_trait_measures_features,
    con = con,
    con_taxa = con_taxa
  )
  
  # Return in old format
  list(
    traits_found = result$traits_raw,
    traits_idtax_num = result$traits_numeric,
    traits_idtax_char = result$traits_categorical
  )
}











#' Choose growth forms
#'
#' Return a tibble of growth form chosen by hierarchy
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A tibble
#' @export
choose_growth_form <- function() {
  
  growth_form_cat <- query_trait(pattern = "growth")

  condition_hierarchical <- sapply(strsplit(growth_form_cat$traitdescription, 'if '), `[`, 2)
  condition_hierarchical <- sapply(strsplit(condition_hierarchical, '[.]'), `[`, 1)
  
  growth_form_cat <-
    growth_form_cat %>%
    mutate(condition_hierarchical = condition_hierarchical)
  
  all_growth_form <- vector('list', 10)
  
  first_level <- choice_trait_cat(id_trait =  growth_form_cat %>%
                                    filter(trait == "growth_form_level_1") %>%
                                    pull(id_trait))
  
  if (!any(is.na(first_level))) {
    
    all_growth_form[[1]] <- first_level
    
    second_level <- choice_trait_cat(id_trait = growth_form_cat %>%
                                       filter(condition_hierarchical == first_level$value) %>%
                                       pull(id_trait))
    
    if (!all(is.na(second_level))) if(!is.na(second_level$value)) all_growth_form[[2]] <- second_level
    
    if (!any(is.na(second_level))) {
      
      id_t <- growth_form_cat %>%
        filter(condition_hierarchical == second_level$value) %>%
        pull(id_trait)
      
      if (length(id_t) > 0) {
        
        third_level <- choice_trait_cat(id_trait = id_t)
        
      } else {
        
        third_level <- NA
        
      }
      
      
      if (!any(is.na(third_level))) {
        
        all_growth_form[[3]] <- third_level
        
        filtered_growth_form <-
          growth_form_cat %>%
          filter(condition_hierarchical == third_level$value)
        
        if (nrow(filtered_growth_form)  > 0) {
          
          fourth_level <- choice_trait_cat(id_trait =  filtered_growth_form %>%
                                             pull(id_trait))
          
          all_growth_form[[4]] <- fourth_level
          
        } else {
          
          fourth_level <- NA
          
        }
        
        if (!any(is.na(fourth_level))) {
          
          filtered_growth_form <-
            growth_form_cat %>%
            filter(condition_hierarchical == fourth_level$value)
          
          if (nrow(filtered_growth_form)  > 0) {
            
            fith_level <- choice_trait_cat(id_trait =  filtered_growth_form %>%
                                             pull(id_trait))
            
            all_growth_form[[5]] <- fith_level
            
          } else {
            
            fith_level <- NA
            
          }
        }
      }
    }
  }
  
  all_growth_form <-
    bind_rows(all_growth_form[unlist(lapply(all_growth_form, function(x) !is.null(x)))])
  
  return(all_growth_form)
  
}



choice_trait_cat <- function(id_trait) {
  
  trait_selected <-
    query_trait(id_trait = id_trait)
  
  print(tibble(description = unlist(stringr::str_split(trait_selected$traitdescription, pattern = "[.]"))) %>%
          kableExtra::kable(format = "html", escape = F) %>%
          kableExtra::kable_styling("striped", full_width = F) %>%
          print())
  
  print(trait_selected$list_factors[[1]])
  
  cli::cli_alert_info("Choose any {trait_selected$trait}")
  first_level_choice <-
    readline(prompt = "")
  
  if (first_level_choice != "") {
    
    suppressWarnings(if(is.na(as.numeric(first_level_choice)))
      stop(paste("Choose a number for selecting", trait_selected$trait)))
    
    selected_value <-
      trait_selected$list_factors[[1]] %>%
      slice(as.numeric(first_level_choice)) %>%
      mutate(trait = trait_selected$trait)
    
  } else {
    
    selected_value <- NA
    
  }
  
  return(selected_value)
  
}



#' Query in taxa trait table
#'
#' Query in taxa trait table by id or pattern
#'
#' @return tibble with query results
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param id_trait integer id of trait to select
#' @param pattern string vector trait to look for in the table
#'
#' @export
query_trait <- function(id_trait = NULL, pattern = NULL, con = NULL) {

  if (is.null(con)) {
    if (exists("mydb", envir = .GlobalEnv)) {
      con <- get("mydb", envir = .GlobalEnv)
    } else {
      con <- call.mydb()
      assign("mydb", con, envir = .GlobalEnv)
    }
  }

  if (!is.null(id_trait)) {
    cli::cli_alert_info("query trait by id")

    trait_tbl <- try_open_postgres_table(table = "traitlist", con = con)

    valuetype <-
      trait_tbl %>%
      dplyr::filter(id_trait == !!id_trait) %>%
      dplyr::collect()
  }

  if (is.null(id_trait) & !is.null(pattern)) {

    cli::cli_alert_info("query trait by string pattern")

    sql <- glue::glue_sql(paste0("SELECT * FROM traitlist WHERE trait ILIKE '%", pattern, "%'"))

    valuetype <- func_try_fetch(con = con, sql = sql)
    
    
  }
  
  valuetype <-
    valuetype %>%
    dplyr::mutate(list_factors = purrr::pmap(
      .l = .,
      .f = function(factorlevels,
                    ...) {
        
        as_tibble(unlist(stringr::str_split(factorlevels, ", ")))
        
      }
    ))
  
  return(valuetype)
  
}













