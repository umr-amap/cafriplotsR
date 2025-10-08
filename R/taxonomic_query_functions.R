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
#' Provide list of selected taxa
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param class character string of class
#' @param family string
#' @param genus string
#' @param order string
#' @param species string genus followed by species name separated by one space
#' @param tax_nam01 string
#' @param tax_nam02 string
#' @param only_genus logical
#' @param only_family logical
#' @param only_class logical
#' @param ids integer id of searched taxa
#' @param verbose logical
#' @param exact_match logical
#' @param check_synonymy logical
#' @param extract_traits logical
#'
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
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
    exact_match = FALSE,
    check_synonymy =TRUE,
    extract_traits = TRUE
  ) {

    # if(!exists("mydb")) call.mydb()

    mydb_taxa <- call.mydb.taxa()

    if(!is.null(class)) {

      res_q <- query_exact_match(
        tbl = "table_tax_famclass",
        field = "tax_famclass",
        values_q = class,
        con = mydb_taxa
      )

      res_class <-
        tbl(mydb_taxa, "table_taxa") %>%
        filter(id_tax_famclass %in% !!res_q$res_q$id_tax_famclass) %>%
        dplyr::select(idtax_n, idtax_good_n) %>%
        collect()

    }

    if(is.null(ids)) {

      if(!is.null(order)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = "tax_order",
          values_q = order,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for order for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = "tax_order",
              values_q = query_tb_miss$tax_order[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_order <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_order <- q_res$res_q
        }

      }

      if(!is.null(family)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = "tax_fam",
          values_q = family,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for family for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = "tax_fam",
              values_q = query_tb_miss$tax_fam[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_family <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_family <- q_res$res_q
        }

      }

      if(!is.null(genus)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = "tax_gen",
          values_q = genus,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for genus for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = "tax_gen",
              values_q = query_tb_miss$tax_gen[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_genus <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_genus <- q_res$res_q
        }

      }

      if(!is.null(species)) {

        q_res <- query_exact_match(
          tbl = "table_taxa",
          field = c("tax_gen", "tax_esp"),
          values_q = species,
          con = mydb_taxa
        )

        if (!exact_match & any(is.na(q_res$query_tb$id))) {

          if (verbose) cli::cli_alert_info("Fuzzy search for species for {sum(is.na(q_res$query_tb$id))} name(s)")

          query_tb_miss <-
            q_res$query_tb %>%
            filter(is.na(id))

          fuz_list <- vector('list', nrow(query_tb_miss))
          for (i in 1:nrow(query_tb_miss)) {

            fuz_list[[i]] <- query_fuzzy_match(
              tbl = "table_taxa",
              field = c("tax_gen", "tax_esp"),
              values_q = query_tb_miss$species[i],
              con = mydb_taxa
            )

          }

          fuz_res <- bind_rows(fuz_list) %>% distinct()

        }

        if (!exact_match & any(is.na(q_res$query_tb$id))) {
          res_species <- bind_rows(fuz_res, q_res$res_q)
        } else {
          res_species <- q_res$res_q
        }

      }

      no_match <- FALSE
      res <-
        tbl(mydb_taxa, "table_taxa")

      if(!is.null(class))
        res <- res %>%
        filter(idtax_n %in% !!res_class$idtax_n)

      if (!is.null(order))
        if (nrow(res_order) > 0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_order$idtax_n)

        } else {

          if (verbose) cli::cli_alert_danger("no match for order")
          no_match <- TRUE
        }

      if(!is.null(family))
        if(nrow(res_family)>0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_family$idtax_n)
        }else{
          if (verbose) cli::cli_alert_danger("no match for family")
          no_match <- TRUE
        }

      if(!is.null(genus))
        if(nrow(res_genus)>0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_genus$idtax_n)
        }else{
          if (verbose) cli::cli_alert_danger("no match for genus")
          no_match <- TRUE
        }

      if (!is.null(species))
        if (nrow(res_species) > 0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_species$idtax_n)
        } else{
          if (verbose) cli::cli_alert_danger("no match for species")
          no_match <- TRUE
        }

      if(!no_match) {
        res <- res %>% collect()
      } else {
        res <- NULL
        if (verbose) cli::cli_alert_danger("no matching names")
      }

    } else {

      if(!is.null(class)) {

        ids <-
          ids[ids %in% res_class$idtax_n]

        if (length(ids) == 0) {

          stop("id provided not found in the class queried")

        }

      }

      tbl <- "table_taxa"
      sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax_n IN ({vals*})",
                           vals = ids, .con = mydb_taxa)

      res <- func_try_fetch(con = mydb_taxa, sql = sql)

      # rs <- DBI::dbSendQuery(mydb_taxa, sql)
      # res <- DBI::dbFetch(rs)
      # DBI::dbClearResult(rs)
      # res <- dplyr::as_tibble(res)

    }

    if (only_genus)
      res <-
        res %>%
        dplyr::filter(is.na(tax_esp))

    if (only_family)
      res <-
        res %>%
        dplyr::filter(is.na(tax_esp),
                      is.na(tax_gen))

    if (only_class)
      res <-
        res %>%
        dplyr::filter(is.na(tax_esp),
                      is.na(tax_gen),
                      is.na(tax_order),
                      is.na(tax_fam))

    ## checking synonymies
    if(!is.null(res) & check_synonymy) {

      ## if selected taxa are synonyms
      if(any(!is.na(res$idtax_good_n))) {

        if (any(res$idtax_good_n > 1)) {

          if (verbose) {

            cli::cli_alert_info("{sum(res$idtax_good_n > 1, na.rm = TRUE)} taxa selected is/are synonym(s)")
            cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")

          }

          ## retrieving good idtax_n if selected ones are considered synonyms
          idtax_accepted <-
            res %>%
            dplyr::select(idtax_n, idtax_good_n) %>%
            dplyr::mutate(idtax_f = ifelse(!is.na(idtax_good_n),
                                           idtax_good_n, idtax_n)) %>%
            dplyr::distinct(idtax_f) %>%
            dplyr::rename(idtax_n = idtax_f)

          idtax_already_extracted <-
            res %>%
            filter(idtax_n %in% idtax_accepted$idtax_n)

          idtax_missing <- idtax_accepted %>%
            filter(!idtax_n %in% idtax_already_extracted$idtax_n)

          res <-
            tbl(mydb_taxa, "table_taxa") %>%
            dplyr::filter(idtax_n %in% !!idtax_missing$idtax_n) %>%
            collect() %>%
            bind_rows(idtax_already_extracted)

          if (verbose) cli::cli_alert_info("{nrow(res)} selected taxa after checking synonymies")

        }
      }

      ## retrieving all synonyms from selected taxa
      id_synonyms <-
        tbl(mydb_taxa, "table_taxa") %>%
        filter(idtax_good_n %in% !!res$idtax_n) %>% ## all taxa synonyms of selected taxa
        # filter(idtax_n %in% !!res$idtax_n) %>% ## excluding taxa already in extract
        dplyr::select(idtax_n, idtax_good_n) %>%
        collect()

      if(nrow(id_synonyms) > 0) {

        if(verbose) {
          cli::cli_alert_info("{sum(id_synonyms$idtax_good_n > 0, na.rm = TRUE)} taxa selected have synonym(s)")
          cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")
        }

        synonyms <- query_taxa(ids = id_synonyms$idtax_n,
                               check_synonymy = FALSE,
                               verbose = FALSE,
                               class = NULL,
                               extract_traits = FALSE)

        res <-
          res %>%
          bind_rows(synonyms)

      }
    }

    if (!is.null(res)) {

      if (nrow(res) > 0) {

        res <-
          res %>%
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
          dplyr::mutate(introduced_status = stringr::str_trim(introduced_status)) %>%
          dplyr::mutate(tax_sp_level = as.character(tax_sp_level),
                        tax_infra_level = as.character(tax_infra_level),
                        tax_infra_level_auth = as.character(tax_infra_level_auth)) %>%
          dplyr::select(-tax_famclass) %>%
          left_join(dplyr::tbl(mydb_taxa, "table_tax_famclass") %>%
                      dplyr::collect(),
                    by = c("id_tax_famclass" = "id_tax_famclass")) %>%
          dplyr::relocate(tax_famclass, .after = tax_order) %>%
          dplyr::relocate(year_description, .after = citation) %>%
          dplyr::relocate(data_modif_d, .after = morpho_species) %>%
          dplyr::relocate(data_modif_m, .after = morpho_species) %>%
          dplyr::relocate(data_modif_y, .after = morpho_species) %>%
          dplyr::relocate(tax_sp_level, .before = idtax_n) %>%
          dplyr::relocate(id_tax_famclass, .after = morpho_species)

        if (extract_traits) {

          traitsqueried <-
            query_traits_measures(idtax = res$idtax_n, idtax_good = res$idtax_good_n)

          if (length(traitsqueried$traits_idtax_num) > 1)
            res <-
              res %>%
              left_join(traitsqueried$traits_idtax_num,
                        by = c("idtax_n" = "idtax"))

          if (length(traitsqueried$traits_idtax_char) > 1)
            res <-
              res %>%
              left_join(traitsqueried$traits_idtax_char,
                        by = c("idtax_n" = "idtax"))

          # if (any(class(traitsqueried$traits_idtax_num) == "data.frame"))
          #   res <-
          #     res %>%
          #     left_join(traitsqueried$traits_idtax_num,
          #               by = c("idtax_n" = "idtax"))
          #
          # if (any(class(traitsqueried$traits_idtax_char) == "data.frame"))
          #   res <-
          #     res %>%
          #     left_join(traitsqueried$traits_idtax_char,
          #               by = c("idtax_n" = "idtax"))

        }

        if (any(names(res) == "a_habit"))
          res <-
            res %>%
            dplyr::select(-a_habit,-a_habit_secondary)

        if (any(names(res) == "fktax"))
          res <-
            res %>%
            dplyr::select(-fktax)

        if (any(names(res) == "id_good"))
          res <-
            res %>%
            dplyr::select(-id_good)

        if (any(names(res) == "tax_tax"))
          res <-
            res %>%
            dplyr::select(-tax_tax)
      }




    }

    if(!is.null(res)) {
      if (verbose & nrow(res) < 50 & nrow(res) > 0) {

        res_print <-
          res %>%
          dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
          dplyr::relocate(idtax_n, .before = tax_order) %>%
          dplyr::relocate(idtax_good_n, .before = tax_order)

        print_table(res_print)

        # res_print <-
        #   res_print %>%
        #   mutate(across(where(is.character), ~ tidyr::replace_na(., "")))
        #
        # res_print <- suppressMessages(as_tibble(cbind(columns = names(res_print), record = t(res_print)),
        #                                         .name_repair = "universal"))
        #
        # res_print %>%
        #   kableExtra::kable(format = "html", escape = F) %>%
        #   kableExtra::kable_styling("striped", full_width = F) %>%
        #   print()

      }

      if(verbose & nrow(res) >= 50)
        cli::cli_alert_info("Not showing html table because too many taxa")
    }

    if(!is.null(res)) return(res)
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
