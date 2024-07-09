

query_individual_features <- function(id = NULL,
                                      multiple_census = FALSE,
                                      id_traits = NULL,
                                      pivot_table = TRUE,
                                      extract_trait_measures_features = FALSE) {

  tbl <- "data_traits_measures"
  tbl2 <- "traitlist"

  if (!is.null(id) & is.null(id_traits))
    sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_data_individuals IN ({vals*})",
                        vals = id, .con = mydb)

  if (!is.null(id) & !is.null(id_traits))
    sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_data_individuals IN ({vals*}) AND id_trait IN ({vals2*})",
                          vals = id, vals2 = id_traits, .con = mydb)

  traits_measures <-
    suppressMessages(func_try_fetch(con = mydb, sql = sql))
  traits_measures <-
    traits_measures %>% select(-starts_with("date_modif"), -id_specimen,-id_diconame)

  if (extract_trait_measures_features) {

    feats <- query_traits_measures_features(id_trait_measures = traits_measures$id_trait_measures)

    if (any(!is.na(feats$all_feat_pivot))) {

      feats_unique <- feats$all_feat_pivot %>%
        mutate(id_ind_meas_feat = as.character(id_ind_meas_feat)) %>%
        group_by(id_trait_measures) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T)),
                  across(where(is.character), ~paste(., collapse = "|")))

      traits_measures <-
        traits_measures %>%
        dplyr::left_join(feats_unique,
                         by = c("id_trait_measures" = "id_trait_measures"))
    }

  }



  # if (!is.null(id_traits))
  #   traits_measures <-
  #   traits_measures %>%
  #   filter(id_trait %in% id_traits)

  if (multiple_census) {

    ids <- unique(traits_measures$id_sub_plots)
    ids <- ids[!is.na(ids)]

    subs_plots_concerned <-
      dplyr::tbl(mydb, "data_liste_sub_plots") %>%
      dplyr::filter(id_sub_plots %in% !!ids) %>%
      dplyr::select(id_sub_plots,
                    id_table_liste_plots,
                    id_type_sub_plot,
                    typevalue,
                    month,
                    year) %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "subplotype_list") %>%
          dplyr::select(type, id_subplotype),
        by = c("id_type_sub_plot" = "id_subplotype")
      ) %>%
      dplyr::select(-id_type_sub_plot) %>%
      dplyr::collect()

    subs_plots_concerned <-
      subs_plots_concerned %>%
      mutate(census_name = paste(type, typevalue, sep = "_"))

    traits_measures <- traits_measures %>%
      left_join(subs_plots_concerned %>% dplyr::select(id_sub_plots, census_name))


  }

  traits_char_multiple <- vector('list', 2)
  # issue_char_multiple <- vector('list', 2)
  if (any(traits_measures$valuetype == "character") | any(traits_measures$valuetype == "ordinal") | any(traits_measures$valuetype == "categorical")) {

    traits_char_list <- vector('list', 2)

    if (multiple_census) {

      traits_char_list[[1]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "character" | valuetype == "ordinal" | valuetype == "categorical") %>%
        filter(is.na(id_sub_plots))

      traits_char_list[[2]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "character" | valuetype == "ordinal" | valuetype == "categorical") %>%
        filter(!is.na(id_sub_plots))

    } else {

      traits_char_list[[1]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "character" | valuetype == "ordinal" | valuetype == "categorical")

    }

    if (nrow(traits_char_list[[1]]) > 0 & pivot_table) {

      traits_char_multiple[[1]] <- .pivot_tab(dataset = traits_char_list[[1]],
                                              cat = "char", census = multiple_census)
      names(traits_char_multiple[[1]]) <-
        gsub("traitvalue_char_", "", names(traits_char_multiple[[1]]))

    } else {

      if (nrow(traits_char_list[[1]]) > 0)
        traits_char_multiple[[1]] <- traits_char_list[[1]]

    }

    # traits_char <- .pivot_tab(dataset = traits_measures %>%
    #                             dplyr::filter(valuetype == "character"),
    #                           cat = "char",
    #                           census = multiple_census)
    # names(traits_char) <- gsub("traitvalue_char_", "", names(traits_char))

    if (multiple_census) {
      if (nrow(traits_char_list[[2]]) > 0 & pivot_table) {

        traits_char_multiple[[2]] <- .pivot_tab(dataset = traits_char_list[[2]], cat = "char", census = multiple_census)

        traits_char_multiple[[2]] <-
          traits_char_multiple[[2]] %>%
          group_by(id_data_individuals, id_sub_plots) %>%
          summarise(across(starts_with("id_trait"), ~ .x[!is.na(.x)][1]),
                    across(!starts_with("id_trait"), ~ paste(.x[!is.na(.x)], collapse = ",")))

        names(traits_char_multiple[[2]]) <-
          gsub("traitvalue_char_", "", names(traits_char_multiple[[2]]))

        # issue_num_multiple[[2]] <-
        #   traits_num_list[[2]]  %>%
        #   dplyr::select(id_sub_plots,
        #                 id_data_individuals,
        #                 trait,
        #                 issue,
        #                 id_trait_measures,
        #                 census_name) %>%
        #   dplyr::mutate(issue = str_squish(issue)) %>%
        #   dplyr::mutate(rn = data.table::rowid(trait)) %>%
        #   tidyr::pivot_wider(
        #     names_from = c(trait, census_name),
        #     values_from = c(issue, id_trait_measures)
        #   ) %>%
        #   dplyr::select(-rn)

        # issue_num_multiple[[2]] <-
        #   issue_num_multiple[[2]] %>%
        #   group_by(id_data_individuals, id_sub_plots) %>%
        #   summarise(across(starts_with("id_trait"), ~ .x[!is.na(.x)][1]),
        #             across(!starts_with("id_trait"), ~ .x[!is.na(.x)][1]))

      } else {

        if (nrow(traits_char_list[[2]]) > 0)
          traits_char_multiple[[2]] <- traits_char_list[[2]]

      }
    }


      # issue_char <-
      #   traits_measures %>%
      #   dplyr::filter(valuetype == "character") %>%
      #   dplyr::select(id_sub_plots,
      #                 id_data_individuals,
      #                 trait,
      #                 issue,
      #                 id_trait_measures) %>%
      #   dplyr::mutate(issue = str_squish(issue)) %>%
      #   dplyr::mutate(rn = data.table::rowid(trait)) %>%
      #   tidyr::pivot_wider(
      #     names_from = trait,
      #     values_from = c(issue, id_trait_measures)
      #   ) %>%
      #   dplyr::select(-rn)


  } else {

    traits_char_multiple[[1]] <- NA
    # issue_char <- NA

  }

  traits_num_multiple <- vector('list', 2)
  issue_num_multiple <- vector('list', 2)
  if (any(traits_measures$valuetype == "numeric")) {

    traits_num_list <- vector('list', 2)

    if (multiple_census) {

      traits_num_list[[1]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "numeric") %>%
        filter(is.na(id_sub_plots))

      traits_num_list[[2]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "numeric") %>%
        filter(!is.na(id_sub_plots))

    } else {

      traits_num_list[[1]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "numeric")

    }

    if (nrow(traits_num_list[[1]]) > 0 & pivot_table) {

      traits_num_multiple[[1]] <- .pivot_tab(dataset = traits_num_list[[1]], cat = "num", census = FALSE)
      names(traits_num_multiple[[1]]) <-
        gsub("traitvalue_", "", names(traits_num_multiple[[1]]))

      issue_num_multiple[[1]] <-
        traits_num_list[[1]]  %>%
        dplyr::select(id_sub_plots,
                      id_data_individuals,
                      trait,
                      issue,
                      id_trait_measures) %>%
        dplyr::mutate(issue = stringr::str_squish(issue)) %>%
        dplyr::mutate(rn = data.table::rowid(trait)) %>%
        tidyr::pivot_wider(
          names_from = trait,
          values_from = c(issue, id_trait_measures)
        ) %>%
        dplyr::select(-rn) %>%
        dplyr::select(-starts_with("id_trait"))


    } else {

      if (nrow(traits_num_list[[1]]) > 0)
        traits_num_multiple[[1]] <- traits_num_list[[1]]

    }

    if (multiple_census) {
      if (nrow(traits_num_list[[2]]) > 0 & pivot_table) {

        traits_num_multiple[[2]] <- .pivot_tab(dataset = traits_num_list[[2]], cat = "num", census = multiple_census)

        traits_num_multiple[[2]] <-
          traits_num_multiple[[2]] %>%
          group_by(id_data_individuals, id_sub_plots) %>%
          summarise(across(starts_with("id_trait"), ~ .x[!is.na(.x)][1]),
                    across(!starts_with("id_trait"), ~ .x[!is.na(.x)][1]))

        names(traits_num_multiple[[2]]) <-
          gsub("traitvalue_", "", names(traits_num_multiple[[2]]))


        issue_num_multiple[[2]] <-
          traits_num_list[[2]]  %>%
          dplyr::select(id_sub_plots,
                        id_data_individuals,
                        trait,
                        issue,
                        id_trait_measures,
                        census_name) %>%
          dplyr::mutate(issue = stringr::str_squish(issue)) %>%
          dplyr::mutate(rn = data.table::rowid(trait)) %>%
          tidyr::pivot_wider(
            names_from = c(trait, census_name),
            values_from = c(issue, id_trait_measures)
          ) %>%
          dplyr::select(-rn)

        issue_num_multiple[[2]] <-
          issue_num_multiple[[2]] %>%
          group_by(id_data_individuals, id_sub_plots) %>%
          summarise( # across(starts_with("id_trait"), ~ .x[!is.na(.x)][1]),
                    across(!starts_with("id_trait"), ~ .x[!is.na(.x)][1]))

      } else {

        if (nrow(traits_num_list[[2]]) > 0)
          traits_num_multiple[[2]] <- traits_num_list[[2]]

      }
    }




  } else {

    traits_num_multiple[[1]] <- NA
    traits_num_multiple[[2]] <- NA
    issue_num_multiple[[1]] <- NA
    issue_num_multiple[[2]] <- NA

  }

  if (is.null(traits_num_multiple[[2]]))
    traits_num_multiple[[2]] <- NA

  if (is.null(traits_char_multiple[[2]]))
    traits_char_multiple[[2]] <- NA

  return(list(traits_char = traits_char_multiple[unlist(lapply(traits_char_multiple, is.data.frame))],
              # issue_char = issue_char,
              traits_num = traits_num_multiple[unlist(lapply(traits_num_multiple, is.data.frame))],
              issue_num = issue_num_multiple[unlist(lapply(issue_num_multiple, is.data.frame))]))

}


.pivot_tab <- function(dataset, cat, census) {

  if (cat == "num" & census)
    res <- dataset %>%
      dplyr::select(
        id_sub_plots,
        id_data_individuals,
        trait,
        traitvalue,
        id_trait_measures,
        census_name
      ) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = c(trait, census_name),
        values_from = c(traitvalue, id_trait_measures)
      ) %>%
      dplyr::select(-rn)

  if (cat == "num" & !census)
    res <- dataset %>%
      dplyr::select(
        id_sub_plots,
        id_data_individuals,
        trait,
        traitvalue,
        id_trait_measures
      ) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = c(trait),
        values_from = c(traitvalue, id_trait_measures)
      ) %>%
      dplyr::select(-rn)


  if (cat == "char" & census)
    res <- dataset %>%
      dplyr::select(
        id_sub_plots,
        id_data_individuals,
        trait,
        traitvalue_char,
        id_trait_measures,
        census_name
      ) %>%
      dplyr::mutate(traitvalue_char = stringr::str_squish(traitvalue_char)) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = c(trait, census_name),
        values_from = c(traitvalue_char, id_trait_measures)
      ) %>%
      dplyr::select(-rn)

  if (cat == "char" & !census)
    res <- dataset %>%
      dplyr::select(
        id_sub_plots,
        id_data_individuals,
        trait,
        traitvalue_char,
        id_trait_measures
      ) %>%
      dplyr::mutate(traitvalue_char = stringr::str_squish(traitvalue_char)) %>%
      dplyr::mutate(rn = data.table::rowid(trait)) %>%
      tidyr::pivot_wider(
        names_from = c(trait),
        values_from = c(traitvalue_char, id_trait_measures)
      ) %>%
      dplyr::select(-rn)

  return(res)
}




#' Internal function
#'
#' Get for each trait, a tibble of individuals with measures or observations, deal with several observations
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param traits string vector with trait needed
#' @param id_individuals numeric vector with id_n individuals requested
#' @param ids_plot numeric vector with id_plots requested
#' @param skip_dates logical whether include day, month and year of observations
#' @param show_multiple_measures logical whether multiple measures (i.e. census or sometimes more than one value for given measure)
#' @param collapse_multiple_val logical whether multiple traits measures should be collapsed (resulting values as character, separated by dash)
#'
#'
#' @export
.get_trait_individuals_values <- function(traits,
                                          src_individuals = NULL,
                                          ids_plot = NULL,
                                          skip_dates = TRUE,
                                          show_multiple_census = FALSE) {

  cli::cli_alert_info("Extracting individual-level traits and features")

  traits_measures <-
    query_individual_features(id = src_individuals$id_n,
                              multiple_census = show_multiple_census,
                              id_traits = traits)

  if (length(traits_measures$traits_char) > 0) {

    traits_char <-
      purrr::reduce(traits_measures$traits_char,
                    dplyr::full_join,
                    by = c('id_data_individuals', 'id_sub_plots'))

    traits_char <-
      traits_char %>%
      group_by(id_data_individuals, id_sub_plots) %>%
      summarise(across(where(is.character), ~ stringr::str_c(.[!is.na(.)],
                                                             collapse = ", ")), # ~ paste(.x[!is.na(.x)], collapse = ",")
                across(where(is.numeric), ~ .x[!is.na(.x)][1])) %>%
      ungroup() %>%
      mutate(across(where(is.character), ~ na_if(., "")))

    traits_char <-
      traits_char %>%
      group_by(id_data_individuals) %>%
      select(-id_sub_plots) %>%
      dplyr::summarise(
        across(where(is.character), ~ stringr::str_c(.[!is.na(.)],
                                                     collapse = ", ")), # ~ paste(.x[!is.na(.x) & .x != ""], collapse = ",")
        across(where(is.numeric), ~ paste(.x[!is.na(.x)], collapse = ","))
      ) %>%
      mutate(across(where(is.character), ~ na_if(., "")))

  } else {
    traits_char <- NA
  }

  if (length(traits_measures$traits_num) > 0) {

    traits_num <-
      purrr::reduce(traits_measures$traits_num,
                    dplyr::full_join,
                    by = c('id_data_individuals', 'id_sub_plots'))

    traits_num <-
      traits_num %>%
      group_by(id_data_individuals, id_sub_plots) %>%
      summarise(across(starts_with("id_trait"), ~ stringr::str_c(.[!is.na(.)],
                                                                 collapse = ", ")), # ~ paste(.x[!is.na(.x)], collapse = ",")
                across(!starts_with("id_trait"), ~ mean(.x, na.rm = T)))

    issue_num <-
      purrr::reduce(traits_measures$issue_num,
                    dplyr::full_join,
                    by = c('id_data_individuals', 'id_sub_plots'))

    issue_num <-
      issue_num %>%
      group_by(id_data_individuals, id_sub_plots) %>%
      summarise(across(starts_with("id_trait"), ~ .x[!is.na(.x)][1]),
                across(!starts_with("id_trait"), ~ .x[!is.na(.x)][1]))

    traits_num <-
      traits_num %>%
      select(-id_sub_plots) %>%
      group_by(id_data_individuals) %>%
      dplyr::summarise(
        across(!starts_with("id_"), ~ mean(.x[!is.na(.x)])),
        across(starts_with("id_"), ~ stringr::str_c(.[!is.na(.)],
                                                    collapse = ", ")) # ~ paste(.x[!is.na(.x)], collapse = ",")
      )

    issue_num <-
      issue_num %>%
      select(-id_sub_plots) %>%
      group_by(id_data_individuals) %>%
      dplyr::summarise(
        across(!starts_with("id_"), ~ last(.x[!is.na(.x)])),
        across(starts_with("id_"), ~ last(.x[!is.na(.x)]))
      )

  } else {
    traits_num <- NA
    issue_num <- NA
  }



  # issue_char <- traits_measures$issue_char %>%
  #   group_by(id_data_individuals, id_sub_plots) %>%
  #   summarise(across(where(is.character), ~ paste(.x[!is.na(.x)], collapse = ",")))
  #
  # issue_num <- traits_measures$issue_num %>%
  #   group_by(id_data_individuals, id_sub_plots) %>%
  #   summarise(across(where(is.character), ~ paste(.x[!is.na(.x)], collapse = ",")))





  all_traits_list <- list(traits_char = traits_char,
                          traits_num = traits_num,
                          issue_num = issue_num)


  return(all_traits_list)


  # issue_char <- issue_char %>%
  #   group_by(id_data_individuals) %>%
  #   dplyr::summarise(
  #     across(where(is.character), ~ paste(.x[!is.na(.x) & .x != ""], collapse = ","))
  #   )
  #
  # issue_num <- issue_num %>%
  #   group_by(id_data_individuals) %>%
  #   dplyr::summarise(
  #     across(where(is.character), ~ paste(.x[!is.na(.x) & .x != ""], collapse = ","))
  #   )






  # if (is.null(src_individuals)) {
  #
  #   res_individuals_full <-
  #     merge_individuals_taxa(id_individual = id_individuals, id_plot = ids_plot)
  #
  # } else {
  #
  #   res_individuals_full <- src_individuals
  #
  # }










  ## merging traits measures and trait informations
  # traits_measures <-
  #   dplyr::tbl(mydb, "data_traits_measures") %>%
  #   dplyr::left_join(dplyr::tbl(mydb, "traitlist"), by = c("traitid" = "id_trait")) %>%
  #   dplyr::select(-id_specimen,-id_diconame) %>%
  #   dplyr::filter(id_data_individuals %in% !!res_individuals_full$id_n) %>%
  #   dplyr::collect()

  # traits_linked <-
  #   res_individuals_full %>%
  #   dplyr::left_join(traits_measures, by = c("id_n" = "id_data_individuals")) %>%
  #   dplyr::select(
  #     id_n,
  #     id_trait_measures,
  #     id_sub_plots,
  #     traitvalue,
  #     traitvalue_char,
  #     trait,
  #     issue,
  #     day,
  #     month,
  #     year
  #   ) %>%
  #   dplyr::filter(!is.na(trait)) %>%
  #   dplyr::filter(trait  %in% traits) %>%
  #   dplyr::mutate(traitvalue_char = stringr::str_squish(traitvalue_char))


  # if(nrow(traits_linked) > 0) {
  #
  #   all_trait <- dplyr::distinct(traits_linked, trait) %>% dplyr::pull() %>% sort()
  #
  #   all_traits_list <- list()
  #
  #   # print(all_trait)
  #
  #   for (i in 1:length(all_trait)) {
  #
  #     # cat(" ",i)
  #     traits_linked_subset <-
  #       traits_linked %>%
  #       dplyr::filter(trait == all_trait[i])
  #
  #     valuetype <-
  #       dplyr::tbl(mydb, "traitlist") %>%
  #       dplyr::filter(trait == !!all_trait[i]) %>%
  #       dplyr::distinct(valuetype) %>%
  #       dplyr::pull()
  #
  #     if (valuetype == "numeric") {
  #       traits_linked_subset <-
  #         traits_linked_subset %>%
  #         dplyr::select(trait,
  #                       traitvalue,
  #                       issue,
  #                       day,
  #                       month,
  #                       year,
  #                       id_n,
  #                       id_trait_measures,
  #                       id_sub_plots)
  #
  #       traits_linked_subset <-
  #         traits_linked_subset %>%
  #         group_by(id_n, id_sub_plots) %>%
  #         summarise(trait = first(trait),
  #                   traitvalue = mean(traitvalue),
  #                   issue = first(issue),
  #                   day = first(day),
  #                   month = first(month),
  #                   year = first(year),
  #                   id_trait_measures = first(id_trait_measures),
  #                   ) %>%
  #         ungroup()
  #
  #       issue_name <- paste0(all_trait[i], "_issue")
  #       issue_name_enquo <-
  #         rlang::parse_expr(rlang::quo_name(rlang::enquo(issue_name)))
  #
  #       traits_linked_subset <-
  #         traits_linked_subset %>%
  #         dplyr::rename(!!issue_name := issue)
  #
  #     }
  #
  #     if (valuetype == "character" | valuetype == "ordinal") {
  #
  #       traits_linked_subset <-
  #         traits_linked_subset %>%
  #         dplyr::select(
  #           trait,
  #           traitvalue_char,
  #           # issue,
  #           day,
  #           month,
  #           year,
  #           id_n,
  #           id_trait_measures,
  #           id_sub_plots
  #         ) %>%
  #         dplyr::mutate(traitvalue = traitvalue_char) %>%
  #         dplyr::select(-traitvalue_char)
  #     }
  #
  #     trait_name <- all_trait[i]
  #     trait_name_enquo <-
  #       rlang::parse_expr(rlang::quo_name(rlang::enquo(trait_name)))
  #
  #     # print(issue_name)
  #
  #     nbe_individuals_double <-
  #       traits_linked_subset %>%
  #       dplyr::group_by(id_n) %>%
  #       dplyr::count() %>%
  #       dplyr::filter(n > 1) %>%
  #       # dplyr::collect() %>%
  #       nrow()
  #
  #     multiple_census <- FALSE
  #     if (nbe_individuals_double > 0) {
  #
  #       # warning(paste("more than one trait value for at least one individual for", all_trait[i]))
  #       cli::cli_alert_info("more than one trait value for at least one individual for {all_trait[i]}")
  #
  #       if(!show_multiple_measures) {
  #
  #         if (valuetype == "numeric") {
  #
  #           traits_linked_subset <-
  #             traits_linked_subset %>%
  #             dplyr::collect() %>%
  #             dplyr::group_by(id_n) %>%
  #             dplyr::summarise(
  #               traitvalue = dplyr::last(traitvalue),
  #               trait = dplyr::last(trait),
  #               !!issue_name := dplyr::last(!!issue_name_enquo),
  #               day = dplyr::last(day),
  #               month = dplyr::last(month),
  #               year = dplyr::last(year),
  #               # id_old= dplyr::last(id_old),
  #               id_trait_measures = dplyr::last(id_trait_measures),
  #               id_sub_plots = dplyr::last(id_sub_plots)
  #             ) %>%
  #             dplyr::select(
  #               trait,
  #               traitvalue,
  #               !!issue_name_enquo,
  #               day,
  #               month,
  #               year,
  #               id_n,
  #               id_trait_measures,
  #               id_sub_plots
  #
  #             )
  #         } else {
  #
  #           traits_linked_subset <-
  #             traits_linked_subset %>%
  #             dplyr::collect() %>%
  #             dplyr::group_by(id_n) %>%
  #             dplyr::summarise(
  #               traitvalue = dplyr::last(traitvalue),
  #               trait = dplyr::last(trait),
  #               # !!issue_name := dplyr::last(!!issue_name_enquo),
  #               day = dplyr::last(day),
  #               month = dplyr::last(month),
  #               year = dplyr::last(year),
  #               # id_old= dplyr::last(id_old),
  #               id_trait_measures = dplyr::last(id_trait_measures),
  #               id_sub_plots = dplyr::last(id_sub_plots)
  #             ) %>%
  #             dplyr::select(
  #               trait,
  #               traitvalue,
  #               # !!issue_name_enquo,
  #               day,
  #               month,
  #               year,
  #               id_n,
  #               id_trait_measures,
  #               id_sub_plots
  #             )
  #
  #
  #           }
  #
  #       } else {
  #
  #         ids_subs <-
  #           traits_linked_subset %>%
  #           dplyr::distinct(id_sub_plots) %>%
  #           dplyr::pull()
  #
  #         subs_plots_concerned <-
  #           dplyr::tbl(mydb, "data_liste_sub_plots") %>%
  #           dplyr::filter(id_sub_plots %in% ids_subs) %>%
  #           dplyr::select(id_sub_plots,
  #                         id_table_liste_plots,
  #                         id_type_sub_plot,
  #                         typevalue,
  #                         month,
  #                         year) %>%
  #           dplyr::left_join(
  #             dplyr::tbl(mydb, "subplotype_list") %>%
  #               dplyr::select(type, id_subplotype),
  #             by = c("id_type_sub_plot" = "id_subplotype")
  #           ) %>%
  #           dplyr::select(-id_type_sub_plot) %>%
  #           dplyr::collect()
  #
  #
  #         if(nrow(subs_plots_concerned) > 0 & !collapse_multiple_val) {
  #
  #           ## if the multiple values for individuals are for different census, putting multiple_census as TRUE
  #           if(all(subs_plots_concerned$type == "census") &
  #              max(subs_plots_concerned$typevalue) > 1) {
  #
  #             for (j in 1:max(subs_plots_concerned$typevalue)) {
  #
  #               # cat(paste("\n", j, "census(es) for", trait_name), "for",
  #               #     nrow(subs_plots_concerned %>%
  #               #            dplyr::filter(type == 'census', typevalue == j)), "plots")
  #               cli::cli_alert_info("{j} census(es) for {trait_name} for census {nrow(subs_plots_concerned %>%
  #                          dplyr::filter(type == 'census', typevalue == j))}")
  #             }
  #
  #             multiple_census <- TRUE
  #
  #           }
  #         }
  #
  #         if (collapse_multiple_val | valuetype == "character" | valuetype == "ordinal") {
  #
  #           # traits_linked_subset <-
  #           #   traits_linked_subset %>%
  #           #   dplyr::group_by_at(dplyr::vars(-traitvalue, -id_trait_measures)) %>%
  #           #   dplyr::summarise(traitvalue = paste(traitvalue, collapse = "-"),
  #           #                    id_trait_measures = paste(id_trait_measures, collapse = "-")) %>%
  #           #   dplyr::ungroup()
  #
  #           grps_vals <- traits_linked_subset %>%
  #             select(-traitvalue, -id_trait_measures) %>% names()
  #
  #           traits_linked_subset <-
  #             traits_linked_subset %>%
  #             dplyr::group_by(across(all_of(grps_vals))) %>%
  #             dplyr::summarise(
  #               traitvalue = paste(traitvalue, collapse = "-"),
  #               id_trait_measures = paste(id_trait_measures, collapse = "-")
  #             ) %>%
  #             dplyr::ungroup()
  #
  #           if (collapse_multiple_val) multiple_census <- FALSE
  #
  #         }
  #
  #         # traits_linked_subset <-
  #         #   traits_linked_subset %>%
  #         #   dplyr::collect()
  #       }
  #
  #     }
  #
  #     traits_linked_subset_spread_list <- list()
  #
  #     ## spreading measures
  #     if (multiple_census) {
  #
  #       for (j in 1:max(subs_plots_concerned$typevalue)) {
  #
  #         ids_subs_select <- subs_plots_concerned %>%
  #           dplyr::filter(typevalue == j) %>%
  #           dplyr::pull(id_sub_plots)
  #
  #         if(length(ids_subs_select) > 0) {
  #
  #           traits_linked_subset_spread <-
  #             traits_linked_subset %>%
  #             dplyr::filter(id_sub_plots %in% ids_subs_select) %>%
  #             dplyr::select(-day, -month, -year) %>%
  #             # collect() %>%
  #             tidyr::spread(key = trait, value = traitvalue)
  #
  #           traits_linked_subset_spread_list[[length(traits_linked_subset_spread_list)+1]] <-
  #             traits_linked_subset_spread
  #
  #           names(traits_linked_subset_spread_list)[length(traits_linked_subset_spread_list)] <-
  #             paste("census", j, sep=("_"))
  #         }
  #       }
  #     } else{
  #
  #       traits_linked_subset_spread <-
  #         traits_linked_subset %>%
  #         dplyr::select(-day, -month, -year) %>%
  #         tidyr::spread(key = trait, value = traitvalue)
  #
  #       traits_linked_subset_spread_list[[1]] <- traits_linked_subset_spread
  #     }
  #
  #     ## getting dates of traits measures
  #     for (j in 1:length(traits_linked_subset_spread_list)) {
  #
  #       traits_linked_subset_spread_list[[j]] <-
  #         traits_linked_subset_spread_list[[j]] %>%
  #         dplyr::left_join(traits_linked_subset %>%
  #                            dplyr::select(id_trait_measures, day, month, year) %>%
  #                            dplyr::collect(),
  #                          by=c("id_trait_measures"="id_trait_measures"))
  #     }
  #
  #     # traits_linked_subset_spread <-
  #     #   traits_linked_subset_spread %>%
  #     #   dplyr::left_join(traits_linked_subset %>%
  #     #                      dplyr::select(id_trait_measures, day, month, year) %>%
  #     #                      dplyr::collect(),
  #     #             by=c("id_trait_measures"="id_trait_measures"))
  #
  #     if(skip_dates)
  #       for (j in 1:length(traits_linked_subset_spread_list))
  #         traits_linked_subset_spread_list[[j]] <-
  #       traits_linked_subset_spread_list[[j]] %>%
  #       dplyr::select(-day, -month, -year)
  #
  #     ## renaming measures table
  #     for (j in 1:length(traits_linked_subset_spread_list)) {
  #
  #       if(multiple_census) {
  #
  #         id_new_name <- paste("id_trait_measures", all_trait[i],
  #                              names(traits_linked_subset_spread_list)[j], sep="_")
  #         traits_linked_subset_spread_list[[j]] <-
  #           traits_linked_subset_spread_list[[j]] %>%
  #           dplyr::rename(!!id_new_name := id_trait_measures)
  #
  #         id_sub_plots_new_name <- paste("id_sub_plots", all_trait[i],
  #                                        names(traits_linked_subset_spread_list)[j], sep="_")
  #         traits_linked_subset_spread_list[[j]] <-
  #           traits_linked_subset_spread_list[[j]] %>%
  #           dplyr::rename(!!id_sub_plots_new_name := id_sub_plots)
  #
  #         trait_name_new <- paste(all_trait[i], names(traits_linked_subset_spread_list)[j], sep="_")
  #         traits_linked_subset_spread_list[[j]] <-
  #           traits_linked_subset_spread_list[[j]] %>%
  #           dplyr::rename(!!trait_name_new := !!trait_name_enquo)
  #
  #         if (valuetype == "numeric") {
  #           issue_trait_name_new <- paste(issue_name, names(traits_linked_subset_spread_list)[j], sep="_")
  #           traits_linked_subset_spread_list[[j]] <-
  #             traits_linked_subset_spread_list[[j]] %>%
  #             dplyr::rename(!!issue_trait_name_new := !!issue_name_enquo)
  #         }
  #
  #         if(!skip_dates) {
  #
  #           day_new_name <- paste("day", names(traits_linked_subset_spread_list)[j], sep="_")
  #           traits_linked_subset_spread_list[[j]] <-
  #             traits_linked_subset_spread_list[[j]] %>%
  #             dplyr::rename(!!day_new_name := day)
  #           month_new_name <- paste("month", names(traits_linked_subset_spread_list)[j], sep="_")
  #           traits_linked_subset_spread_list[[j]] %>%
  #             dplyr::rename(!!month_new_name := month)
  #           year_new_name <- paste("year", names(traits_linked_subset_spread_list)[j], sep="_")
  #           traits_linked_subset_spread_list[[j]] %>%
  #             dplyr::rename(!!year_new_name := year)
  #
  #         }
  #       }else{
  #
  #         id_new_name <- paste("id_trait_measures", all_trait[i], sep="_")
  #         traits_linked_subset_spread_list[[j]] <-
  #           traits_linked_subset_spread_list[[j]] %>%
  #           dplyr::rename(!!id_new_name := id_trait_measures)
  #         traits_linked_subset_spread_list[[j]] <-
  #           traits_linked_subset_spread_list[[j]] %>%
  #           dplyr::select(-id_sub_plots)
  #
  #       }
  #     }
  #
  #
  #     for (j in 1:length(traits_linked_subset_spread_list))
  #       all_traits_list[[length(all_traits_list)+1]] <- traits_linked_subset_spread_list[[j]]
  #
  #     # print(traits_linked_subset_spread)
  #   }
  #
  #
  # } else (
  #
  #   all_traits_list <- list()
  #
  # )


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

  if(!exists("mydb")) call.mydb()

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




#' Add a trait in trait list
#'
#' Add trait and associated descriptors in trait list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_trait string value with new trait descritors - try to avoid space
#' @param new_relatedterm string related trait to new trait
#' @param new_valuetype string one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character'
#' @param new_maxallowedvalue numeric if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue numeric if valuetype is numeric, indicate the minimum allowed value
#' @param new_traitdescription string full description of trait
#' @param new_factorlevels string a vector of all possible value if valuetype is categorical or ordinal
#' @param new_expectedunit string expected unit (unitless if none)
#' @param new_comments string any comments
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

  call.mydb()

  if(is.null(new_trait)) stop("define new trait")
  if(is.null(new_valuetype)) stop("define new_valuetype")

  if(!any(new_valuetype==c('numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character', 'table_data_liste_plots')))
    stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character' or 'table_data_liste_plots'")

  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_maxallowedvalue) & !is.integer(new_maxallowedvalue)) stop("valuetype numeric of integer and max value not of this type")
  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_minallowedvalue) & !is.integer(new_minallowedvalue)) stop("valuetype numeric of integer and min value not of this type")

  if(!exists("mydb")) call.mydb()

  new_data_renamed <- tibble(trait = new_trait,
                             relatedterm = ifelse(is.null(new_relatedterm), NA, new_relatedterm),
                             valuetype = new_valuetype,
                             maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
                             minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
                             traitdescription = ifelse(is.null(new_traitdescription), NA, new_traitdescription),
                             factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
                             expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
                             comments = ifelse(is.null(new_comments), NA, new_comments))

  print(new_data_renamed)

  Q <- utils::askYesNo("confirm adding this trait?")

  if(Q) DBI::dbWriteTable(mydb, "traitlist", new_data_renamed, append = TRUE, row.names = FALSE)

}






#' List, selected trait measures features
#'
#' Table of srait measures features
#'
#' @return A tibble of all subplots
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @param id_trait_measures integer
#'
#'
#' @export
query_traits_measures_features <- function(id_trait_measures  = NULL) {

  feat_data <-
    try_open_postgres_table(table = "data_ind_measures_feat", con = mydb) %>%
    dplyr::filter(id_trait_measures %in% !!id_trait_measures)

  nbe_feat_data <- nrow(feat_data %>%
                             dplyr::collect())

  if (nbe_feat_data  > 0) {

    all_sub_type <-
      feat_data %>%
      dplyr::distinct(id_trait) %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "traitlist") %>%
          dplyr::select(trait, valuetype, traitdescription, id_trait),
        by = c("id_trait" = "id_trait")
      )

    extracted_data <-
      feat_data %>%
      dplyr::left_join(all_sub_type,
                       by = c("id_trait" = "id_trait")) %>%
      dplyr::collect() %>%
      dplyr::select(id_trait_measures,
                    trait,
                    valuetype,
                    typevalue,
                    typevalue_char,
                    id_ind_meas_feat)

    # extracted_data <- extracted_data %>%
    #   group_by(id_trait_measures, trait) %>%
    #   summarise(valuetype = first(valuetype),
    #             typevalue = mean(typevalue, na.rm = T),
    #             n = n()) %>%
    #   filter(n > 1)

    if (any(extracted_data$valuetype == "numeric")) {
      numeric_subplots_pivot <-
        extracted_data %>%
        filter(valuetype == "numeric") %>%
        select(id_trait_measures, typevalue, trait, id_ind_meas_feat) %>%
        tidyr::pivot_wider(
          names_from = "trait",
          values_from = "typevalue",
          values_fn = ~ mean(.x, na.rm = TRUE)
        )
    } else {
      numeric_subplots_pivot <- NULL
    }

    if (any(extracted_data$valuetype == "character")) {
      character_feat_pivot <-
        extracted_data %>%
        filter(valuetype == "character") %>%
        select(id_trait_measures, typevalue, trait, id_ind_meas_feat)  %>%
        tidyr::pivot_wider(
          names_from = "trait",
          values_from = "typevalue_char",
          values_fn = ~ paste(.x, collapse = "|")
        )
    } else {
      character_subplots_pivot <- NULL
    }

    all_feat_pivot <-
      c(list(character_subplots_pivot),
        list(numeric_subplots_pivot))

    all_feat_pivot <-
      purrr::reduce(all_feat_pivot[!unlist(lapply(all_feat_pivot, is.null))],
                    dplyr::full_join,
                    by = 'id_trait_measures')


  } else {

    all_feat_pivot <- NA

  }

  return(list(all_feat_pivot = all_feat_pivot))

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

      if (any(data_feat$trait == 0)) {

        add_0 <- utils::askYesNo("Some value are equal to 0. Do you want to add these values anyway ??")

        if(!add_0)
          data_feat <-
            data_feat %>%
            dplyr::filter(trait != 0)

      }

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

      cli::cli_h3(".add_modif_field")
      data_feat <-
        .add_modif_field(dataset = data_feat)


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

        cf_merge <-
          askYesNo(msg = "confirm merging duplicates?")

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

      response <-
        utils::askYesNo("Confirm add these data to data_ind_measures_feat table?")

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

