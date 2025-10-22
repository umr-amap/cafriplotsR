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
        # If exact match failed and we were using exact matching, try fuzzy matching
        if (exact_match) {
          if (verbose) {
            cli::cli_alert_warning("No exact match found for species")
            cli::cli_alert_info("Attempting fuzzy matching...")
          }

          matches <- match_taxonomic_names(
            names = species,
            method = "hierarchical",
            max_matches = 1,
            min_similarity = min_similarity,
            include_synonyms = FALSE,
            return_scores = TRUE,
            con = mydb_taxa,
            verbose = verbose
          )

          if (nrow(matches) > 0 && any(!is.na(matches$idtax_n))) {
            matched_ids <- matches %>%
              filter(!is.na(idtax_n)) %>%
              pull(idtax_n) %>%
              unique()

            if (verbose) {
              best_score <- matches %>%
                filter(!is.na(idtax_n)) %>%
                slice(1) %>%
                pull(match_score)
              cli::cli_alert_success("Found fuzzy match (score: {round(best_score, 2)})")
            }
          } else {
            if (verbose) cli::cli_alert_danger("No match found for species (exact or fuzzy)")
            return(NULL)
          }
        } else {
          # Already tried fuzzy matching, no results
          if (verbose) cli::cli_alert_danger("No match for species")
          return(NULL)
        }
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
                                only_class, check_synonymy, extract_traits, verbose) {

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


#' Match taxonomic level (order/family/genus) using new matching functions
#' @keywords internal
.match_taxonomic_level <- function(names, level, field, exact_match,
                                   min_similarity, mydb_taxa, verbose) {

  if (exact_match) {
    # Use direct SQL for exact matching (faster)
    # Note: We don't filter by tax_level here to allow matching at any level
    # The only_genus/only_family filters are applied later
    sql <- glue::glue_sql(
      "SELECT DISTINCT idtax_n FROM table_taxa WHERE lower({`field`}) IN ({vals*})",
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
      sql <- glue::glue_sql("
        SELECT idtax_n,
               SIMILARITY(lower({`field`}), lower({search_name})) AS sim_score
        FROM table_taxa
        WHERE {`field`} IS NOT NULL
          AND SIMILARITY(lower({`field`}), lower({search_name})) >= {min_sim}
        ORDER BY sim_score DESC
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
  traits_wide <- query_taxa_traits(
    idtax = res$idtax_n,
    format = "wide",
    add_taxa_info = FALSE,
    resolve_synonyms = TRUE,
    categorical_format = "mode",
    con = NULL
  )

  # Join traits with taxa results
  if (!is.null(traits_wide) && nrow(traits_wide) > 0) {
    res <- res %>%
      dplyr::left_join(
        traits_wide,
        by = c("idtax_n" = "idtax_n")
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
