

query_individual_features <- function(id = NULL, multiple_census = FALSE, id_traits = NULL, pivot_table = TRUE) {

  tbl <- "data_traits_measures"
  tbl2 <- "traitlist"

  if (!is.null(id) & is.null(id_traits))
    sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_data_individuals IN ({vals*})",
                        vals = id, .con = mydb)

  if (!is.null(id) & !is.null(id_traits))
    sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_data_individuals IN ({vals*}) AND id_trait IN ({vals2*})",
                          vals = id, vals2 = id_traits, .con = mydb)

  traits_measures <- func_try_fetch(con = mydb, sql = sql)
  traits_measures <- traits_measures %>% select(-starts_with("date_modif"), -id_specimen,-id_diconame)

  if (!is.null(id_traits))
    traits_measures <-
    traits_measures %>%
    filter(id_trait %in% id_traits)

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
  if (any(traits_measures$valuetype == "character") | any(traits_measures$valuetype == "ordinal")) {

    traits_char_list <- vector('list', 2)

    if (multiple_census) {

      traits_char_list[[1]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "character" | valuetype == "ordinal") %>%
        filter(is.na(id_sub_plots))

      traits_char_list[[2]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "character" | valuetype == "ordinal") %>%
        filter(!is.na(id_sub_plots))

    } else {

      traits_char_list[[1]] <-
        traits_measures %>%
        dplyr::filter(valuetype == "character" | valuetype == "ordinal")

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
                across(where(is.numeric), ~ .x[!is.na(.x)][1]))

    traits_char <- traits_char %>%
      group_by(id_data_individuals) %>%
      select(-id_sub_plots) %>%
      dplyr::summarise(
        across(where(is.character), ~ stringr::str_c(.[!is.na(.)],
                                                     collapse = ", ")), # ~ paste(.x[!is.na(.x) & .x != ""], collapse = ",")
        across(where(is.numeric), ~ paste(.x[!is.na(.x)], collapse = ","))
      )

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
