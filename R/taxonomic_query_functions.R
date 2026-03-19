# Taxonomic Query Functions
#
# This file contains functions for querying and matching taxonomic data from the taxa database.
# These functions handle synonym resolution, trait aggregation, and hierarchical trait matching.
#
# Main functions:
# - query_taxa(): Query taxa by taxonomic hierarchy (class, family, genus, species)
# - match_tax(): Match taxa and aggregate traits from species to genus level
# - add_taxa_table_taxa(): Helper to add formatted taxa information
#
# Dependencies: DBI, dplyr, tidyr, cli, stringr, rlang, data.table, glue

#' List, extract taxa
#'
#' Query taxa from the taxonomic backbone database using hierarchical matching.
#' For species queries, if exact matching fails, the function automatically falls
#' back to intelligent fuzzy matching. For higher taxonomic ranks (family, genus, order),
#' exact matching is used by default.
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param class character string of class
#' @param family string or character vector of family names
#' @param genus string or character vector of genus names
#' @param order string or character vector of order names
#' @param species string or character vector of full species names (genus + species)
#' @param tax_nam01 string (currently not used in matching)
#' @param tax_nam02 string (currently not used in matching)
#' @param only_genus logical whether to return only genus-level taxa
#' @param only_family logical whether to return only family-level taxa
#' @param only_class logical whether to return only class-level taxa
#' @param ids integer vector of idtax_n to retrieve directly
#' @param verbose logical whether to show progress messages
#' @param exact_match logical if TRUE (default), only exact matches returned. If FALSE, uses
#'   intelligent fuzzy matching. Note: fuzzy matching is generally only useful for species names;
#'   for family/genus/order queries, exact matching is recommended.
#' @param check_synonymy logical whether to resolve synonyms and include them
#' @param extract_traits logical whether to add trait information
#' @param include_children logical if TRUE, recursively include all descendant
#'   taxa (e.g. species within a genus, infraspecific taxa within a species)
#'   via the `id_parent` foreign key. Default FALSE.
#' @param min_similarity numeric (0-1) minimum similarity score for fuzzy matching (default: 0.3)
#'
#'
#' @return A tibble of taxa with taxonomic hierarchy and optionally traits
#' @export
query_taxa <-
  function(
    class = NULL, # c("Magnoliopsida", "Pinopsida", "Lycopsida", "Pteropsida")
    family = NULL,
    genus = NULL,
    order = NULL,
    species = NULL,
    only_genus = FALSE,
    only_family = FALSE,
    only_class = FALSE,
    ids = NULL,
    verbose = TRUE,
    exact_match = TRUE,
    check_synonymy = TRUE,
    extract_traits = TRUE,
    include_children = FALSE,
    min_similarity = 0.3
  ) {

    mydb_taxa <- call.mydb.taxa()

    # If IDs are provided directly, retrieve them
    if (!is.null(ids)) {
      return(.query_taxa_by_ids(
        ids = ids,
        class = class,
        mydb_taxa = mydb_taxa,
        only_genus = only_genus,
        only_family = only_family,
        only_class = only_class,
        check_synonymy = check_synonymy,
        extract_traits = extract_traits,
        include_children = include_children,
        verbose = verbose
      ))
    }

    # Handle class filtering
    res_class <- NULL
    if (!is.null(class)) {
      res_class <- .match_class(class, mydb_taxa)
    }

    # Match taxonomic names using new intelligent matching
    matched_ids <- NULL

    if (!is.null(order)) {
      matched_ids <- .match_taxonomic_level(
        names = order,
        level = "order",
        field = "tax_order",
        exact_match = exact_match,
        min_similarity = min_similarity,
        mydb_taxa = mydb_taxa,
        verbose = verbose
      )
    }

    if (!is.null(family)) {
      matched_ids <- .match_taxonomic_level(
        names = family,
        level = "family",
        field = "tax_fam",
        exact_match = exact_match,
        min_similarity = min_similarity,
        mydb_taxa = mydb_taxa,
        verbose = verbose
      )
    }

    if (!is.null(genus)) {
      matched_ids <- .match_taxonomic_level(
        names = genus,
        level = "genus",
        field = "tax_gen",
        exact_match = exact_match,
        min_similarity = min_similarity,
        mydb_taxa = mydb_taxa,
        verbose = verbose
      )
    }

    if (!is.null(species)) {
      # Use full intelligent matching for species names
      matches <- match_taxonomic_names(
        names = species,
        method = if (exact_match) "exact" else "hierarchical",
        max_matches = 1,
        min_similarity = min_similarity,
        include_synonyms = FALSE,
        return_scores = FALSE,
        con = mydb_taxa,
        verbose = verbose
      )

      if (nrow(matches) > 0 && any(!is.na(matches$idtax_n))) {
        matched_ids <- matches %>%
          filter(!is.na(idtax_n)) %>%
          pull(idtax_n) %>%
          unique()
      } else {
        # No match found
        if (verbose) {
          if (exact_match) {
            cli::cli_alert_danger("No exact match found for species '{species}'")
          } else {
            cli::cli_alert_danger("No match found for species '{species}'")
          }
        }
        return(NULL)
      }
    }

    # If no matching criteria provided
    if (is.null(matched_ids)) {
      if (verbose) cli::cli_alert_danger("No matching names found")
      return(NULL)
    }

    # Build final query
    res <- tbl(mydb_taxa, "table_taxa")

    # Apply class filter if specified
    if (!is.null(class) && !is.null(res_class)) {
      res <- res %>% filter(idtax_n %in% !!res_class$idtax_n)
    }

    # Apply matched IDs filter
    res <- res %>% filter(idtax_n %in% !!matched_ids)

    # Collect results
    res <- res %>% collect()

    if (is.null(res) || nrow(res) == 0) {
      if (verbose) cli::cli_alert_danger("No matching taxa after filtering")
      return(NULL)
    }

    # IMPORTANT: Prioritize accepted taxa (idtax_good_n IS NULL) over synonyms in results
    # This ensures when duplicates exist (like multiple Fabaceae entries), the accepted one appears first
    res <- res %>%
      dplyr::arrange(
        dplyr::case_when(
          is.na(idtax_good_n) ~ 0,  # Accepted taxa first
          TRUE ~ 1                   # Synonyms after
        ),
        idtax_n
      )

    # Include child taxa (recursive via id_parent)
    if (include_children && nrow(res) > 0) {
      res <- .include_children(res, mydb_taxa, verbose)
    }

    # Apply hierarchical filters using tax_level field
    if (only_genus) {
      res <- res %>% dplyr::filter(tax_level == "genus")
    }

    if (only_family) {
      res <- res %>% dplyr::filter(tax_level == "family")
    }

    if (only_class) {
      res <- res %>% dplyr::filter(tax_level == "higher")
    }

    # Handle synonymy resolution
    if (check_synonymy) {
      res <- .resolve_synonyms(res, mydb_taxa, verbose)
    } else {
      # When check_synonymy = FALSE, exclude synonyms (keep only accepted taxa)
      res <- res %>% dplyr::filter(is.na(idtax_good_n))
      if (verbose && nrow(res) > 0) {
        cli::cli_alert_info("Filtered to {nrow(res)} accepted taxa (synonyms excluded)")
      }
    }

    # Format taxonomic names
    res <- .format_taxa_names(res, mydb_taxa)

    # Add trait information if requested
    if (extract_traits && nrow(res) > 0) {
      res <- .add_traits_to_taxa(res)
    }

    # Clean up unwanted columns
    res <- .clean_taxa_columns(res)

    # Print results if verbose
    if (verbose && nrow(res) > 0) {
      .print_taxa_results(res)
    }

    return(res)
  }



# Internal helper functions for query_taxa() -----------------------------------

#' Query taxa by IDs (internal helper)
#' @keywords internal
.query_taxa_by_ids <- function(ids, class, mydb_taxa, only_genus, only_family,
                                only_class, check_synonymy, extract_traits,
                                include_children = FALSE, verbose) {

  # Filter by class if specified
  if (!is.null(class)) {
    res_class <- .match_class(class, mydb_taxa)
    ids <- ids[ids %in% res_class$idtax_n]

    if (length(ids) == 0) {
      stop("IDs provided not found in the class queried")
    }
  }

  # Fetch taxa by IDs
  tbl <- "table_taxa"
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax_n IN ({vals*})",
                        vals = ids, .con = mydb_taxa)
  res <- func_try_fetch(con = mydb_taxa, sql = sql)

  if (is.null(res) || nrow(res) == 0) {
    return(NULL)
  }

  # Include child taxa (recursive via id_parent)
  if (include_children && nrow(res) > 0) {
    res <- .include_children(res, mydb_taxa, verbose)
  }

  # Apply hierarchical filters
  if (only_genus) {
    res <- res %>% dplyr::filter(is.na(tax_esp))
  }
  if (only_family) {
    res <- res %>% dplyr::filter(is.na(tax_esp), is.na(tax_gen))
  }
  if (only_class) {
    res <- res %>%
      dplyr::filter(is.na(tax_esp), is.na(tax_gen),
                    is.na(tax_order), is.na(tax_fam))
  }

  # Handle synonymy
  if (check_synonymy) {
    res <- .resolve_synonyms(res, mydb_taxa, verbose)
  }

  # Format and add traits
  res <- .format_taxa_names(res, mydb_taxa)

  if (extract_traits && nrow(res) > 0) {
    res <- .add_traits_to_taxa(res)
  }

  res <- .clean_taxa_columns(res)

  if (verbose && nrow(res) > 0) {
    .print_taxa_results(res)
  }

  return(res)
}


#' Match class (internal helper)
#' @keywords internal
.match_class <- function(class, mydb_taxa) {
  # Use exact match for class (kept from original for compatibility)
  sql <- glue::glue_sql(
    "SELECT * FROM table_tax_famclass WHERE lower(tax_famclass) IN ({vals*})",
    vals = tolower(class), .con = mydb_taxa
  )
  res_class_tbl <- func_try_fetch(con = mydb_taxa, sql = sql)

  if (nrow(res_class_tbl) == 0) {
    cli::cli_alert_warning("No match for class: {class}")
    return(NULL)
  }

  res_class <- tbl(mydb_taxa, "table_taxa") %>%
    filter(id_tax_famclass %in% !!res_class_tbl$id_tax_famclass) %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    collect()

  return(res_class)
}


#' Recursively include all child taxa via id_parent
#' @param res data.frame of initial taxa results
#' @param mydb_taxa database connection
#' @param verbose logical
#' @return data.frame with children appended
#' @keywords internal
.include_children <- function(res, mydb_taxa, verbose) {
  parent_ids     <- res$idtax_n
  all_children   <- data.frame()
  iteration      <- 0L
  max_iterations <- 10L

  if (verbose) cli::cli_alert_info("Including child taxa...")


  while (length(parent_ids) > 0 && iteration < max_iterations) {
    iteration <- iteration + 1L

    sql <- glue::glue_sql(
      "SELECT * FROM table_taxa WHERE id_parent IN ({vals*})",
      vals = parent_ids, .con = mydb_taxa
    )
    children <- func_try_fetch(con = mydb_taxa, sql = sql)

    if (is.null(children) || nrow(children) == 0) break

    # Keep only genuinely new children
    new_kids <- children[!children$idtax_n %in% c(res$idtax_n,
                                                    all_children$idtax_n), ]
    if (nrow(new_kids) == 0) break

    all_children <- dplyr::bind_rows(all_children, new_kids)
    parent_ids   <- new_kids$idtax_n

    if (verbose) {
      cli::cli_alert_success(
        "  Iteration {iteration}: found {nrow(new_kids)} child taxon/taxa"
      )
    }
  }

  if (nrow(all_children) > 0) {
    if (verbose) {
      cli::cli_alert_success(
        "Total child taxa added: {nrow(all_children)}"
      )
    }
    res <- dplyr::bind_rows(res, all_children)
  } else if (verbose) {
    cli::cli_alert_info("No child taxa found")
  }

  res
}


#' Match taxonomic level (order/family/genus) using new matching functions
#' @keywords internal
.match_taxonomic_level <- function(names, level, field, exact_match,
                                   min_similarity, mydb_taxa, verbose) {

  if (exact_match) {
    # Use direct SQL for exact matching (faster)
    # Note: We don't filter by tax_level here to allow matching at any level
    # The only_genus/only_family filters are applied later
    # IMPORTANT: Prioritize accepted taxa (idtax_good_n IS NULL) over synonyms
    # Use subquery to handle DISTINCT + ORDER BY correctly in PostgreSQL
    sql <- glue::glue_sql(
      "SELECT idtax_n FROM (
         SELECT DISTINCT idtax_n, idtax_good_n
         FROM table_taxa
         WHERE lower({`field`}) IN ({vals*})
       ) sub
       ORDER BY CASE WHEN idtax_good_n IS NULL THEN 0 ELSE 1 END, idtax_n",
      vals = tolower(names), .con = mydb_taxa
    )
    res <- func_try_fetch(con = mydb_taxa, sql = sql)

    if (nrow(res) == 0) {
      if (verbose) cli::cli_alert_danger("No exact match for {level}")
      return(NULL)
    }

    return(res$idtax_n)

  } else {
    # Use SQL-side fuzzy matching
    # Note: We match by the field value, not necessarily at the specific tax_level
    # This allows finding all taxa in that family/genus/order
    if (verbose) cli::cli_alert_info("Matching {length(names)} {level} name(s)...")

    matched_ids <- c()

    for (name in names) {
      # IMPORTANT: Prioritize accepted taxa (idtax_good_n IS NULL) over synonyms
      # When sim_score is tied, put accepted taxa first
      sql <- glue::glue_sql("
        SELECT idtax_n,
               SIMILARITY(lower({`field`}), lower({search_name})) AS sim_score
        FROM table_taxa
        WHERE {`field`} IS NOT NULL
          AND SIMILARITY(lower({`field`}), lower({search_name})) >= {min_sim}
        ORDER BY sim_score DESC,
                 CASE WHEN idtax_good_n IS NULL THEN 0 ELSE 1 END
        LIMIT 5
      ", search_name = name, min_sim = min_similarity, .con = mydb_taxa)

      matches <- func_try_fetch(con = mydb_taxa, sql = sql)

      if (nrow(matches) > 0) {
        matched_ids <- c(matched_ids, matches$idtax_n)
        if (verbose && matches$sim_score[1] < 1.0) {
          cli::cli_alert_info("Fuzzy match for '{name}' (score: {round(matches$sim_score[1], 2)})")
        }
      } else {
        if (verbose) cli::cli_alert_warning("No match for {level}: {name}")
      }
    }

    if (length(matched_ids) == 0) {
      if (verbose) cli::cli_alert_danger("No matches found for {level}")
      return(NULL)
    }

    return(unique(matched_ids))
  }
}


#' Resolve synonyms (internal helper)
#' @keywords internal
.resolve_synonyms <- function(res, mydb_taxa, verbose) {

  if (is.null(res) || nrow(res) == 0) return(res)

  # If selected taxa are synonyms, get their accepted names
  if (any(!is.na(res$idtax_good_n) & res$idtax_good_n > 1)) {

    if (verbose) {
      n_syn <- sum(!is.na(res$idtax_good_n) & res$idtax_good_n > 1)
      cli::cli_alert_info("{n_syn} taxa selected is/are synonym(s)")
      cli::cli_alert_info("{nrow(res)} taxa before resolving synonyms")
    }

    # Get accepted IDs
    idtax_accepted <- res %>%
      dplyr::select(idtax_n, idtax_good_n) %>%
      dplyr::mutate(idtax_f = ifelse(!is.na(idtax_good_n) & idtax_good_n > 1,
                                     idtax_good_n, idtax_n)) %>%
      dplyr::distinct(idtax_f) %>%
      dplyr::rename(idtax_n = idtax_f)

    # Get accepted taxa not already in results
    idtax_already_extracted <- res %>%
      filter(idtax_n %in% idtax_accepted$idtax_n)

    idtax_missing <- idtax_accepted %>%
      filter(!idtax_n %in% idtax_already_extracted$idtax_n)

    if (nrow(idtax_missing) > 0) {
      missing_taxa <- tbl(mydb_taxa, "table_taxa") %>%
        dplyr::filter(idtax_n %in% !!idtax_missing$idtax_n) %>%
        collect()

      res <- bind_rows(res, missing_taxa)
    }

    if (verbose) cli::cli_alert_info("{nrow(res)} taxa after resolving synonyms")
  }

  # Get all synonyms of selected taxa
  id_synonyms <- tbl(mydb_taxa, "table_taxa") %>%
    filter(idtax_good_n %in% !!res$idtax_n) %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    collect()

  if (nrow(id_synonyms) > 0) {
    if (verbose) {
      cli::cli_alert_info("{nrow(id_synonyms)} synonym(s) found for selected taxa")
    }

    # Recursively fetch synonyms (without checking synonymy again)
    synonyms <- query_taxa(
      ids = id_synonyms$idtax_n,
      check_synonymy = FALSE,
      verbose = FALSE,
      class = NULL,
      extract_traits = FALSE
    )

    if (!is.null(synonyms)) {
      res <- bind_rows(res, synonyms)
    }
  }

  return(res %>% distinct())
}


#' Format taxonomic names (internal helper)
#' @keywords internal
.format_taxa_names <- function(res, mydb_taxa) {

  if (is.null(res) || nrow(res) == 0) return(res)

  res <- res %>%
    mutate(
      tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA),
      tax_infra_level = ifelse(
        !is.na(tax_esp),
        paste0(
          tax_gen, " ", tax_esp,
          ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
          ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
          ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
          ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")
        ),
        NA
      ),
      tax_infra_level_auth = ifelse(
        !is.na(tax_esp),
        paste0(
          tax_gen, " ", tax_esp,
          ifelse(!is.na(author1), paste0(" ", author1), ""),
          ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
          ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
          ifelse(!is.na(author2), paste0(" ", author2), ""),
          ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
          ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
          ifelse(!is.na(author3), paste0(" ", author3), "")
        ),
        NA
      )
    ) %>%
    dplyr::mutate(
      introduced_status = stringr::str_trim(introduced_status),
      tax_sp_level = as.character(tax_sp_level),
      tax_infra_level = as.character(tax_infra_level),
      tax_infra_level_auth = as.character(tax_infra_level_auth)
    )

  # Add family class information
  if ("tax_famclass" %in% names(res)) {
    res <- res %>% dplyr::select(-tax_famclass)
  }

  res <- res %>%
    left_join(
      dplyr::tbl(mydb_taxa, "table_tax_famclass") %>% dplyr::collect(),
      by = c("id_tax_famclass" = "id_tax_famclass")
    ) %>%
    dplyr::relocate(tax_famclass, .after = tax_order) %>%
    dplyr::relocate(year_description, .after = citation) %>%
    dplyr::relocate(data_modif_d, .after = morpho_species) %>%
    dplyr::relocate(data_modif_m, .after = morpho_species) %>%
    dplyr::relocate(data_modif_y, .after = morpho_species) %>%
    dplyr::relocate(tax_sp_level, .before = idtax_n) %>%
    dplyr::relocate(id_tax_famclass, .after = morpho_species)

  return(res)
}


#' Add traits to taxa (internal helper)
#' @keywords internal
.add_traits_to_taxa <- function(res) {

  if (is.null(res) || nrow(res) == 0) return(res)

  # Use query_taxa_traits (new function) instead of deprecated query_traits_measures
  traits_result <- query_taxa_traits(
    idtax = res$idtax_n,
    format = "wide",
    add_taxa_info = FALSE,
    include_synonyms = TRUE,
    categorical_mode = "mode",
    con_taxa = NULL
  )

  # Check what we got back
  has_numeric <- !is.null(traits_result$traits_numeric) &&
                 !inherits(traits_result$traits_numeric, "logical")
  has_categorical <- !is.null(traits_result$traits_categorical) &&
                     !inherits(traits_result$traits_categorical, "logical")

  # Join numeric traits if available
  # Note: query_taxa_traits returns column named "idtax", not "idtax_n"
  if (has_numeric && nrow(traits_result$traits_numeric) > 0) {
    res <- res %>%
      dplyr::left_join(
        traits_result$traits_numeric,
        by = c("idtax_n" = "idtax")
      )
  }

  # Join categorical traits if available
  if (has_categorical && nrow(traits_result$traits_categorical) > 0) {
    res <- res %>%
      dplyr::left_join(
        traits_result$traits_categorical,
        by = c("idtax_n" = "idtax")
      )
  }

  return(res)
}


#' Clean unwanted columns (internal helper)
#' @keywords internal
.clean_taxa_columns <- function(res) {

  if (is.null(res) || nrow(res) == 0) return(res)

  # Remove legacy/unwanted columns if they exist
  cols_to_remove <- c("a_habit", "a_habit_secondary", "fktax", "id_good", "tax_tax")

  for (col in cols_to_remove) {
    if (col %in% names(res)) {
      res <- res %>% dplyr::select(-!!col)
    }
  }

  return(res)
}


#' Print taxa results (internal helper)
#' @keywords internal
.print_taxa_results <- function(res) {

  if (is.null(res) || nrow(res) == 0) return(invisible(NULL))

  if (nrow(res) < 50) {
    res_print <- res %>%
      dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
      dplyr::relocate(idtax_n, .before = tax_order) %>%
      dplyr::relocate(idtax_good_n, .before = tax_order)

    print_table(res_print)
  } else {
    cli::cli_alert_info("Not showing table because too many taxa ({nrow(res)} rows)")
  }

  invisible(NULL)
}




#' Query and standardize taxonomy
#'
#' Query and standardize taxonomy for synonymies and add traits information at species and genus levels
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param idtax vector of idtax_n to be search
#' @param queried_tax tibble, output of query_taxa
#' @param verbose logical whether results should be shown in viewer
#'
#' @examples
#' match_tax(idtax = c(3095, 219))
#'
#' @export
match_tax <- function(idtax, queried_tax = NULL, verbose = TRUE) {

  if (is.null(queried_tax)) {

    queried_tax <- query_taxa(ids = idtax,
                               class = NULL,
                               verbose = FALSE)

  } else {

    idtax <- unique(queried_tax$idtax_n)

  }

  queried_taxa_updated <- queried_tax

  list_genera <-
    queried_tax %>%
    dplyr::filter(!is.na(tax_gen), is.na(idtax_good_n)) %>%
    dplyr::distinct(tax_gen) %>%
    dplyr::pull(tax_gen)

  all_sp_genera <- query_taxa(
    genus = list_genera,
    class = NULL,
    extract_traits = FALSE,
    verbose = FALSE,
    exact_match = TRUE
  )

  all_sp_genera <- all_sp_genera %>%
    filter(!is.na(idtax_good_n)) %>%
    left_join(all_sp_genera %>%
                filter(is.na(idtax_good_n)) %>%
                dplyr::select(idtax_n, tax_gen) %>%
                dplyr::rename(tax_gen_good = tax_gen),
              by = c("idtax_good_n" = "idtax_n")) %>%
    mutate(tax_gen = ifelse(!is.na(tax_gen_good), tax_gen_good, tax_gen))

  all_sp_genera <-
    all_sp_genera %>%
    filter(tax_gen %in% list_genera,
           !is.na(tax_infra_level))

  all_val_sp <- query_traits_measures(idtax = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_n),
                                      idtax_good = all_sp_genera %>%
                                        filter(!is.na(tax_esp)) %>%
                                        pull(idtax_good_n),
                                      add_taxa_info = T)

  # level_trait <- rep("species", nrow(res))

  if (any(class(all_val_sp$traits_idtax_char) == "data.frame")) {

    traits_idtax_char <-
      all_val_sp$traits_found %>%
      dplyr::filter(valuetype == "categorical") %>%
      dplyr::select(idtax,
                    trait,
                    traitvalue_char,
                    basisofrecord,
                    id_trait_measures) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = trait,
        values_from = c(traitvalue_char, basisofrecord, id_trait_measures)
      ) %>%
      dplyr::select(-rn) %>%
      left_join(all_val_sp$traits_idtax_char %>%
                  dplyr::select(idtax, tax_gen),
                by = c("idtax" = "idtax"))

    names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))

    traits_idtax_concat <-
      traits_idtax_char %>%
      dplyr::select(tax_gen, starts_with("id_trait_")) %>%
      dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ stringr::str_c(.[!is.na(.)],
                                                   collapse = ", "))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    if (verbose) cli::cli_alert_info("Extracting most frequent value for categorical traits at genus level")

    traits_idtax_char <-
      traits_idtax_char %>%
      dplyr::select(-starts_with("id_trait_")) %>%
      group_by(tax_gen, across(where(is.character))) %>%
      count() %>%
      arrange(tax_gen, desc(n)) %>%
      ungroup() %>%
      group_by(tax_gen) %>%
      dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))

    traits_idtax_char <-
      left_join(traits_idtax_char,
                traits_idtax_concat, by = c("tax_gen" = "tax_gen"))

    colnames_traits <- names(traits_idtax_char %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))

    for (j in 1:length(colnames_traits)) {

      if (colnames_traits[j] %in% names(queried_taxa_updated)) {

        var1 <- paste0(colnames_traits[j], ".y")
        var2 <- paste0(colnames_traits[j], ".x")

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_char %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 "genus"),
                          "species")) %>%
          dplyr::select(-paste0(colnames_traits[j], ".x"),
                        -paste0(colnames_traits[j], ".y"))

      } else {

        var1 <- colnames_traits[j]

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_char %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          NA,
                          "genus"))

      }
    }


  }

  if (any(class(all_val_sp$traits_idtax_num) == "data.frame")) {

    traits_idtax_num <-
      all_val_sp$traits_found %>%
      dplyr::filter(valuetype == "numeric") %>%
      dplyr::select(idtax,
                    trait,
                    traitvalue,
                    basisofrecord,
                    id_trait_measures) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = trait,
        values_from = c(traitvalue, basisofrecord, id_trait_measures)
      ) %>%
      dplyr::select(-rn) %>%
      dplyr::left_join(all_val_sp$traits_idtax_num %>%
                         dplyr::select(idtax, tax_gen),
                       by = c("idtax" = "idtax"))

    names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))

    traits_idtax_concat <-
      traits_idtax_num %>%
      dplyr::select(tax_gen, starts_with("id_trait_")) %>%
      dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::mutate(dplyr::across(where(is.character),
                                  ~ stringr::str_c(.[!is.na(.)],
                                                   collapse = ", "))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

    traits_idtax_num <-
      traits_idtax_num %>%
      dplyr::select(-starts_with("id_trait_"), -idtax) %>%
      dplyr::group_by(tax_gen) %>%
      dplyr::summarise(dplyr::across(where(is.numeric),
                                     .fns= list(mean = mean,
                                                sd = sd,
                                                n = length),
                                     .names = "{.col}_{.fn}"))


    colnames_traits <- names(traits_idtax_num %>%
                               dplyr::select(
                                 -tax_gen,
                                 -starts_with("id_trait_"),
                                 -starts_with("basisofrecord_")
                               ))

    for (j in 1:length(colnames_traits)) {

      if (colnames_traits[j] %in% names(queried_taxa_updated)) {

        var1 <- paste0(colnames_traits[j], ".y")
        var2 <- paste0(colnames_traits[j], ".x")

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_num %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                          ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                 NA,
                                 "genus"),
                          "species")) %>%
          dplyr::select(-paste0(colnames_traits[j], ".x"),
                        -paste0(colnames_traits[j], ".y"))


      } else {

        var1 <- colnames_traits[j]

        queried_taxa_updated <-
          queried_taxa_updated %>%
          left_join(
            traits_idtax_num %>%
              dplyr::select(tax_gen, colnames_traits[j]),
            by = c("tax_gen" = "tax_gen")
          ) %>%
          mutate("source_{colnames_traits[j]}" :=
                   ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                          NA,
                          "genus"))

      }
    }

  }


  queried_taxa_syn_sub <-
    queried_taxa_updated %>%
    filter(!is.na(idtax_good_n)) %>%
    dplyr::select(idtax_n, idtax_good_n, tax_infra_level) %>%
    filter(idtax_n %in% idtax) %>%
    rename(taxa_submitted = tax_infra_level) %>%
    left_join(queried_taxa_updated, by = c("idtax_good_n" = "idtax_n")) %>%
    relocate(tax_infra_level, .after = "taxa_submitted")

  queried_taxa_not_syn_sub <-
    queried_taxa_updated %>%
    filter(is.na(idtax_good_n)) %>%
    dplyr::select(idtax_n, tax_infra_level) %>%
    filter(idtax_n %in% idtax) %>%
    rename(taxa_submitted = tax_infra_level) %>%
    left_join(queried_taxa_updated, by = c("idtax_n" = "idtax_n")) %>%
    relocate(tax_infra_level, .after = "taxa_submitted")

  results <- bind_rows(queried_taxa_syn_sub, queried_taxa_not_syn_sub) %>%
    arrange(tax_fam, tax_gen, tax_esp)


  if (verbose & nrow(results) < 100) {

    res_print <-
      results %>%
      # dplyr::select(-fktax,-id_good,-tax_tax) %>%
      dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
      dplyr::relocate(idtax_n, .before = tax_order) %>%
      dplyr::relocate(idtax_good_n, .before = tax_order)

    print_table(res_print)

    # res_print <-
    #   res_print %>%
    #   mutate_all(~ as.character(.)) %>%
    #   mutate_all(~ tidyr::replace_na(., ""))
    #
    # as_tibble(cbind(columns = names(res_print), record = t(res_print))) %>%
    #   kableExtra::kable(format = "html", escape = F) %>%
    #   kableExtra::kable_styling("striped", full_width = F) %>%
    #   print()

  }


  if(verbose & nrow(results) >= 100)
    message("\n Not showing html table because too many taxa")

  return(results)

}




#' Add formatted taxa information
#'
#' Helper function to add formatted taxonomic names (species, infraspecific, with authors)
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param ids vector of idtax_n to retrieve
#'
#' @export
add_taxa_table_taxa <- function(ids = NULL) {

  mydb_taxa <- call.mydb.taxa()

  table_taxa <-
    try_open_postgres_table(table = "table_taxa", con = mydb_taxa)

  table_taxa <-
    table_taxa %>%
    mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
    mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                    paste0(tax_gen,
                                           " ",
                                           tax_esp,
                                           ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                           ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                           ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                           ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                    NA)) %>%
    mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                         paste0(tax_gen,
                                                " ",
                                                tax_esp,
                                                ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                ifelse(!is.na(author3), paste0(" ", author3), "")),
                                         NA)) %>%
    dplyr::select(-year_description,
                  -tax_rankinf)

  if (!is.null(ids)) {

    table_taxa <-
      table_taxa %>%
      filter(idtax_n %in% ids)

  }



  return(table_taxa)
}


# ============================================================================
# Hierarchy Navigation Functions (using id_parent)
# ============================================================================

#' Get All Children of a Taxon
#'
#' Returns all taxa that have the specified taxon as their parent (directly or recursively).
#' Uses the id_parent column for hierarchy traversal.
#'
#' @param idtax_n The taxon ID to find children for
#' @param con Database connection (optional, will create if NULL)
#' @param recursive If TRUE (default), get all descendants; if FALSE, only direct children
#' @param include_self If TRUE, include the taxon itself in results
#' @param collect If TRUE (default), collect results to data frame
#'
#' @return Data frame of child taxa
#' @export
get_taxon_children <- function(idtax_n, con = NULL, recursive = TRUE,
                               include_self = FALSE, collect = TRUE) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  if (recursive) {
    # Use recursive CTE for all descendants
    sql <- glue::glue_sql("
      WITH RECURSIVE descendants AS (
        -- Base case: direct children
        SELECT * FROM table_taxa WHERE id_parent = {idtax}
        UNION ALL
        -- Recursive case: children of children
        SELECT t.* FROM table_taxa t
        INNER JOIN descendants d ON t.id_parent = d.idtax_n
      )
      SELECT * FROM descendants
    ", idtax = idtax_n, .con = actual_con)

    if (include_self) {
      sql <- glue::glue_sql("
        WITH RECURSIVE descendants AS (
          -- Include self
          SELECT * FROM table_taxa WHERE idtax_n = {idtax}
          UNION ALL
          -- Recursive children
          SELECT t.* FROM table_taxa t
          INNER JOIN descendants d ON t.id_parent = d.idtax_n
        )
        SELECT * FROM descendants
      ", idtax = idtax_n, .con = actual_con)
    }

    result <- func_try_fetch(con = actual_con, sql = sql)

  } else {
    # Just direct children
    result <- dplyr::tbl(actual_con, "table_taxa") %>%
      dplyr::filter(id_parent == !!idtax_n)

    if (include_self) {
      self <- dplyr::tbl(actual_con, "table_taxa") %>%
        dplyr::filter(idtax_n == !!idtax_n)
      result <- dplyr::union_all(self, result)
    }

    if (collect) {
      result <- dplyr::collect(result)
    }
  }

  return(result)
}


#' Get All Ancestors of a Taxon
#'
#' Walks up the id_parent chain to return all ancestor taxa from the given
#' taxon up to the root (class level).
#'
#' @param idtax_n The taxon ID to find ancestors for
#' @param con Database connection (optional, will create if NULL)
#' @param include_self If TRUE, include the taxon itself in results
#'
#' @return Data frame of ancestor taxa, ordered from root to taxon
#' @export
get_taxon_ancestors <- function(idtax_n, con = NULL, include_self = TRUE) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Use recursive CTE to walk up the hierarchy
  sql <- glue::glue_sql("
    WITH RECURSIVE ancestors AS (
      -- Base case: the taxon itself
      SELECT *, 0 as depth FROM table_taxa WHERE idtax_n = {idtax}
      UNION ALL
      -- Recursive case: parent of current
      SELECT t.*, a.depth + 1 FROM table_taxa t
      INNER JOIN ancestors a ON t.idtax_n = a.id_parent
    )
    SELECT * FROM ancestors ORDER BY depth DESC
  ", idtax = idtax_n, .con = actual_con)

  result <- func_try_fetch(con = actual_con, sql = sql)

  if (!include_self && nrow(result) > 0) {
    # Remove the last row (self)
    result <- result[-nrow(result), ]
  }

  # Remove depth column
  if ("depth" %in% names(result)) {
    result <- result %>% dplyr::select(-depth)
  }

  return(result)
}


#' Get Full Taxonomy Hierarchy for a Taxon
#'
#' Returns a structured list containing the full hierarchical path from
#' class to the given taxon, with each level's information.
#'
#' @param idtax_n The taxon ID to get hierarchy for
#' @param con Database connection (optional, will create if NULL)
#'
#' @return List with hierarchy levels: class, order, family, genus, species, infraspecific
#' @export
get_taxon_hierarchy <- function(idtax_n, con = NULL) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Get all ancestors including self
  ancestors <- get_taxon_ancestors(idtax_n, con = actual_con, include_self = TRUE)

  if (is.null(ancestors) || nrow(ancestors) == 0) {
    return(NULL)
  }

  # Build hierarchy structure
  hierarchy <- list(
    class = NULL,
    order = NULL,
    family = NULL,
    genus = NULL,
    species = NULL,
    infraspecific = NULL,
    current_level = NULL,
    current_taxon = NULL
  )

  for (i in seq_len(nrow(ancestors))) {
    row <- ancestors[i, ]

    # Determine level using tax_level column (preferred) or fallback to inference
    # Helper to safely check if field has value
    has_value <- function(x) {
      !is.null(x) && !is.na(x) && length(x) > 0 && nchar(as.character(x)) > 0
    }

    if ("tax_level" %in% names(row) && has_value(row$tax_level)) {
      # Use tax_level column directly
      level <- as.character(row$tax_level)
      # Map "higher" to "class" for consistency
      if (!is.na(level) && level == "higher") level <- "class"
    } else {
      # Fallback: infer level based on what's populated
      if (has_value(row$tax_nam01)) {
        level <- "infraspecific"
      } else if (has_value(row$tax_esp)) {
        level <- "species"
      } else if (has_value(row$tax_gen)) {
        level <- "genus"
      } else if (has_value(row$tax_fam)) {
        level <- "family"
      } else if (has_value(row$tax_order)) {
        level <- "order"
      } else {
        level <- "class"
      }
    }

    # Create entry for this level
    entry <- list(
      idtax_n = row$idtax_n,
      name = switch(
        level,
        "class" = row$tax_famclass,
        "order" = row$tax_order,
        "family" = row$tax_fam,
        "genus" = row$tax_gen,
        "species" = paste(row$tax_gen, row$tax_esp),
        "infraspecific" = paste(row$tax_gen, row$tax_esp, row$tax_rank01, row$tax_nam01)
      ),
      full_row = row
    )

    hierarchy[[level]] <- entry

    # Track the current (target) taxon
    if (row$idtax_n == idtax_n) {
      hierarchy$current_level <- level
      hierarchy$current_taxon <- entry
    }
  }

  return(hierarchy)
}


#' Find Parent Entry for a Given Taxonomic Level
#'
#' Finds the parent entry for a taxon at a specific level. Used when inserting
#' new taxa to find or create the appropriate parent.
#'
#' Uses the tax_level column to identify parent entries.
#'
#' @param con Database connection
#' @param tax_gen Genus name
#' @param tax_fam Family name
#' @param tax_order Order name
#' @param tax_famclass Class name
#' @param tax_esp Species epithet (for infraspecific taxa)
#' @param level The level of the taxon being inserted ("species", "genus", "family", "order")
#'
#' @return Parent taxon ID (idtax_n) or NULL if not found
#' @keywords internal
.find_parent_entry <- function(con, tax_gen = NULL, tax_fam = NULL,
                               tax_order = NULL, tax_famclass = NULL,
                               tax_esp = NULL, level = "species") {

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  parent_query <- dplyr::tbl(actual_con, "table_taxa")

  if (level == "infraspecific") {
    # Parent is species (tax_level = "species")
    parent_query <- parent_query %>%
      dplyr::filter(
        tax_gen == !!tax_gen,
        tax_fam == !!tax_fam,
        tax_esp == !!tax_esp,
        tax_level == "species"
      )
  } else if (level == "species") {
    # Parent is genus (tax_level = "genus")
    parent_query <- parent_query %>%
      dplyr::filter(
        tax_gen == !!tax_gen,
        tax_fam == !!tax_fam,
        tax_level == "genus"
      )
  } else if (level == "genus") {
    # Parent is family (tax_level = "family")
    parent_query <- parent_query %>%
      dplyr::filter(
        tax_fam == !!tax_fam,
        tax_level == "family"
      )
  } else if (level == "family") {
    # Parent is order (tax_level = "order")
    parent_query <- parent_query %>%
      dplyr::filter(
        tax_order == !!tax_order,
        tax_level == "order"
      )
  } else if (level == "order") {
    # Parent is class (tax_level IN ("class", "higher"))
    parent_query <- parent_query %>%
      dplyr::filter(
        tax_famclass == !!tax_famclass,
        tax_level %in% c("class", "higher")
      )
  } else {
    # Class level has no parent
    return(NULL)
  }

  result <- parent_query %>%
    dplyr::select(idtax_n) %>%
    dplyr::collect()

  if (nrow(result) > 0) {
    return(result$idtax_n[1])
  }

  return(NULL)
}


#' Find or Create Parent Entry
#'
#' Finds the parent entry for a taxon, creating it if it doesn't exist.
#' Recursively ensures the full hierarchy exists.
#'
#' @param con Database connection
#' @param tax_gen Genus name
#' @param tax_fam Family name
#' @param tax_order Order name
#' @param tax_famclass Class name
#' @param tax_esp Species epithet (for infraspecific taxa)
#' @param level The level of the taxon being inserted
#'
#' @return Parent taxon ID (idtax_n)
#' @keywords internal
.find_or_create_parent_entry <- function(con, tax_gen = NULL, tax_fam = NULL,
                                         tax_order = NULL, tax_famclass = NULL,
                                         tax_esp = NULL, level = "species") {

  # First try to find existing parent
  parent_id <- .find_parent_entry(
    con, tax_gen, tax_fam, tax_order, tax_famclass, tax_esp, level
  )

  if (!is.null(parent_id)) {
    return(parent_id)
  }

  # Need to create parent - first ensure parent's parent exists
  parent_level <- switch(
    level,
    "infraspecific" = "species",
    "species" = "genus",
    "genus" = "family",
    "family" = "order",
    "order" = "class",
    NULL
  )

  if (is.null(parent_level)) {
    return(NULL)  # Class has no parent
  }

  # Recursively ensure grandparent exists
  grandparent_id <- NULL
  if (parent_level != "class") {
    grandparent_id <- .find_or_create_parent_entry(
      con, tax_gen, tax_fam, tax_order, tax_famclass, NULL,
      level = parent_level
    )
  }

  # Now create the parent entry
  parent_id <- .create_hierarchy_entry_for_parent(
    con,
    tax_gen = if (parent_level %in% c("genus", "species", "infraspecific")) tax_gen else NA,
    tax_esp = if (parent_level == "species") tax_esp else NA,
    tax_fam = if (parent_level %in% c("family", "genus", "species", "infraspecific")) tax_fam else NA,
    tax_order = if (parent_level %in% c("order", "family", "genus", "species", "infraspecific")) tax_order else NA,
    tax_famclass = tax_famclass,
    tax_level = parent_level,  # Use tax_level column
    id_parent = grandparent_id
  )

  return(parent_id)
}


#' Create a Hierarchy Entry for Parent (internal helper)
#'
#' Creates a new entry at the specified taxonomic level with proper id_parent.
#' Uses tax_level column to indicate the taxonomic rank.
#'
#' @param con Database connection
#' @param tax_gen Genus name
#' @param tax_esp Species epithet
#' @param tax_fam Family name
#' @param tax_order Order name
#' @param tax_famclass Class name
#' @param tax_level Taxonomic level: "class", "order", "family", "genus", "species"
#' @param id_parent Parent taxon ID
#'
#' @keywords internal
.create_hierarchy_entry_for_parent <- function(con, tax_gen = NA, tax_esp = NA,
                                               tax_fam = NA, tax_order = NA,
                                               tax_famclass = NA, tax_level = NA,
                                               id_parent = NA) {
  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Get id_tax_famclass if needed (for backward compatibility)
  id_tax_famclass <- NA
  if (!is.na(tax_famclass)) {
    class_row <- tryCatch({
      DBI::dbGetQuery(
        actual_con,
        "SELECT id_tax_famclass FROM table_tax_famclass WHERE tax_famclass = $1",
        params = list(tax_famclass)
      )
    }, error = function(e) data.frame())
    if (nrow(class_row) > 0) {
      id_tax_famclass <- class_row$id_tax_famclass[1]
    }
  }

  new_entry <- tibble::tibble(
    tax_gen = if (is.na(tax_gen)) NA_character_ else tax_gen,
    tax_esp = if (is.na(tax_esp)) NA_character_ else tax_esp,
    tax_fam = if (is.na(tax_fam)) NA_character_ else tax_fam,
    tax_order = if (is.na(tax_order)) NA_character_ else tax_order,
    tax_famclass = if (is.na(tax_famclass)) NA_character_ else tax_famclass,
    tax_rank01 = NA_character_,
    tax_nam01 = NA_character_,
    tax_rank02 = NA_character_,
    tax_nam02 = NA_character_,
    tax_source = "AUTO_HIERARCHY",
    tax_level = if (is.na(tax_level)) NA_character_ else tax_level,
    idtax_good_n = NA_integer_,
    id_tax_famclass = id_tax_famclass,
    morpho_species = FALSE,
    id_parent = if (is.na(id_parent)) NA_integer_ else as.integer(id_parent)
  )

  # Add modification fields
  new_entry <- .add_modif_field(new_entry)
  new_entry <- new_entry %>%
    dplyr::rename(
      data_modif_m = date_modif_m,
      data_modif_y = date_modif_y,
      data_modif_d = date_modif_d
    )

  DBI::dbWriteTable(actual_con, "table_taxa", new_entry, append = TRUE, row.names = FALSE)

  # Get the new ID
  rs <- DBI::dbSendQuery(actual_con, "SELECT MAX(idtax_n) FROM table_taxa")
  lastval <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)

  cli::cli_alert_info("Created {tax_level} entry: {coalesce(tax_gen, tax_fam, tax_order, tax_famclass)} (ID: {lastval$max})")

  return(lastval$max)
}


#' Count Children at Each Level
#'
#' Returns counts of children taxa at each hierarchical level for a given taxon.
#' Uses tax_level column to identify levels.
#'
#' @param idtax_n The taxon ID to count children for
#' @param con Database connection (optional)
#'
#' @return Named vector with counts per level
#' @export
count_taxon_children <- function(idtax_n, con = NULL) {
  if (is.null(con)) {
    con <- call.mydb.taxa()
  }

  # Handle pool connections
  actual_con <- if (inherits(con, "Pool")) {
    pool::poolCheckout(con)
  } else {
    con
  }

  on.exit({
    if (inherits(con, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

  # Get all descendants
  children <- get_taxon_children(idtax_n, con = actual_con, recursive = TRUE)

  if (is.null(children) || nrow(children) == 0) {
    return(c(
      total = 0,
      orders = 0,
      families = 0,
      genera = 0,
      species = 0,
      infraspecific = 0
    ))
  }

  # Count by tax_level column (preferred) or fallback to inference
  if ("tax_level" %in% names(children)) {
    counts <- children %>%
      dplyr::mutate(
        level = dplyr::case_when(
          tax_level == "infraspecific" ~ "infraspecific",
          tax_level == "species" ~ "species",
          tax_level == "genus" ~ "genera",
          tax_level == "family" ~ "families",
          tax_level == "order" ~ "orders",
          tax_level %in% c("class", "higher") ~ "classes",
          TRUE ~ "other"
        )
      ) %>%
      dplyr::count(level) %>%
      tibble::deframe()
  } else {
    # Fallback: infer from NULL fields
    counts <- children %>%
      dplyr::mutate(
        level = dplyr::case_when(
          !is.na(tax_nam01) ~ "infraspecific",
          !is.na(tax_esp) ~ "species",
          !is.na(tax_gen) ~ "genera",
          !is.na(tax_fam) ~ "families",
          !is.na(tax_order) ~ "orders",
          TRUE ~ "other"
        )
      ) %>%
      dplyr::count(level) %>%
      tibble::deframe()
  }

  result <- c(
    total = nrow(children),
    orders = counts["orders"] %||% 0,
    families = counts["families"] %||% 0,
    genera = counts["genera"] %||% 0,
    species = counts["species"] %||% 0,
    infraspecific = counts["infraspecific"] %||% 0
  )

  return(result)
}
