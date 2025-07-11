
#' Query for features linked to individual
#'
#' Query for features linked to a given individual in inventories
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param id numeric vector of id from individuals
#' @param multiple_census boolean whether multiple census should be shown
#' @param id_traits numeric vector of id from traits/features
#' @param pivot_table boolean whether results should be pivoted into multiple columns
#' @param extract_trait_measures_features boolean whether adding additional features linked to traits/features, only if pivot_table is FALSE
#' @param remove_obs_with_issue boolean whether features with issue should be excluded
#' @export
query_individual_features <- function(id = NULL,
                                      multiple_census = FALSE,
                                      id_traits = NULL,
                                      pivot_table = TRUE,
                                      extract_trait_measures_features = FALSE,
                                      extract_linked_individuals = FALSE,
                                      remove_obs_with_issue = TRUE) {

  if (length(id) > 1000) {
    chunk_size <- 1000
    
    chunks <- split(id, gl(length(id)/chunk_size, chunk_size, length(id)))
    
    cli::cli_alert_warning("Individual features queried by chunks because of large number of values")
    
  } else {
    
    chunks <- list(id)
    
  }
  
  pb <- txtProgressBar(min = 0, max = length(chunks), style = 3)
  traits_measures_list <- vector('list', length(chunks))
  for (i in 1:length(chunks)) {
    setTxtProgressBar(pb, i)
    
    # if (length(chunks) > 1) cat(i, " ")
    
    if (!is.null(id) & is.null(id_traits))
      sql <- .sql_query_trait_ind(id_in = chunks[[i]], mydb = mydb)
    
    if (!is.null(id) & !is.null(id_traits))
      sql <- .sql_query_trait_ind_2(id_ind = chunks[[i]], id_traits = id_traits, mydb = mydb)
    
    if (is.null(id) & !is.null(id_traits))
      sql <- .sql_query_trait(id_traits = id_traits, mydb = mydb)
    
    traits_measures_list[[i]] <-
      suppressMessages(func_try_fetch(con = mydb, sql = sql))
    
  }
  
  close(pb)
  
  traits_measures <- 
    bind_rows(traits_measures_list)
  
  if (remove_obs_with_issue)
    traits_measures <-
    traits_measures %>%
    filter(is.na(issue))
  

  ### Error : Failed to fetch row : SSL SYSCALL error: EOF detected
  traits_measures <-
    traits_measures %>% select(-starts_with("date_modif"))

  
  traits_measures <- traits_measures %>%
    dplyr::select(-any_of(c("id_diconame")))

  if (extract_trait_measures_features) {

    feats <- query_traits_measures_features(id_trait_measures = traits_measures$id_trait_measures)

    if (any(!is.na(feats$all_feat_pivot))) {

      feats_unique <-
        feats$all_feat_pivot %>%
        mutate(id_ind_meas_feat = as.character(id_ind_meas_feat)) %>%
        group_by(id_trait_measures) %>%
        summarise(across(where(is.numeric), ~mean(., na.rm = T)),
                  across(where(is.character), ~paste(.[!is.na(.)], collapse = "|"))) %>%
        mutate(across(where(is.character), ~na_if(.x, "")))

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
                                              cat = "char", census = FALSE)
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

        traits_char_multiple[[2]] <- .pivot_tab(dataset = traits_char_list[[2]], 
                                                cat = "char", 
                                                census = multiple_census)

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
  
  
  if (extract_linked_individuals & nrow(traits_measures) > 0) {
    
    ids_ind <- unique(traits_measures$id_data_individuals)
    
    if (length(ids_ind) > 30000) {
      chunk_size <- 30000
      
      chunks <- split(ids_ind, gl(length(ids_ind)/chunk_size, chunk_size, length(ids_ind)))
      
      # cli::cli_alert_warning("Individual linked queried by chunks because of large number of values")
      
    } else {
      
      chunks <- list(ids_ind)
      
    }
    pb <- txtProgressBar(min = 0, max = length(chunks), style = 3)
    traits_measures_list <- vector('list', length(chunks))
    
    ind_data_list <- vector('list', length(chunks))
    for (i in 1:length(chunks)) { # 
      setTxtProgressBar(pb, i)
      
      ind_data_list[[i]] <- 
        merge_individuals_taxa(id_individual = chunks[[i]])
      
    }
    close(pb)
    
    ind_data <- 
      bind_rows(ind_data_list)
    
    
    # ind_data <-
    #   query_plots(
    #     id_individual = unique(traits_measures$id_data_individuals),
    #     extract_subplot_features = FALSE,
    #     extract_traits = FALSE,
    #     extract_individual_features = FALSE
    #   )
    
    
  } else {
    ind_data <- NA
  }

  return(list(traits_char = traits_char_multiple[unlist(lapply(traits_char_multiple, is.data.frame))],
              # issue_char = issue_char,
              traits_num = traits_num_multiple[unlist(lapply(traits_num_multiple, is.data.frame))],
              issue_num = issue_num_multiple[unlist(lapply(issue_num_multiple, is.data.frame))],
              ind_data = ind_data))

}




.sql_query_trait_ind <- function(id_ind, mydb = mydb, tbl = "data_traits_measures", tbl2 = "traitlist") {
  
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_data_individuals IN ({vals*})",
                        vals = id_ind, .con = mydb)
  return(sql)
}

.sql_query_trait_ind_2 <- function(id_ind, id_traits, mydb = mydb, tbl = "data_traits_measures", tbl2 = "traitlist") {
  
  sql <-glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_data_individuals IN ({vals*}) AND id_trait IN ({vals2*})",
                       vals = id_ind, vals2 = id_traits, .con = mydb)
  return(sql)
}

.sql_query_trait <- function(id_traits, mydb = mydb, tbl = "data_traits_measures", tbl2 = "traitlist") {
  
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.traitid = {`tbl2`}.id_trait WHERE id_trait IN ({vals*})",
                        vals = id_traits, .con = mydb)
  return(sql)
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
    res <- 
      dataset %>%
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
#' @param remove_obs_with_issue logical
#' 
#' @importFrom data.table setDT is.data.table rowid
#' 
#' @export
.get_trait_individuals_values <- function(traits,
                                          src_individuals = NULL,
                                          ids_plot = NULL,
                                          skip_dates = TRUE,
                                          show_multiple_census = FALSE,
                                          remove_obs_with_issue = TRUE) {

  cli::cli_alert_info("Extracting individual-level traits and features")

  traits_measures <-
    query_individual_features(id = src_individuals$id_n,
                              multiple_census = show_multiple_census,
                              id_traits = traits,
                              remove_obs_with_issue = remove_obs_with_issue)
  
  cli::cli_alert_info("Extracting character values")

  # process_traits_char <- function(char_list, mode_cols = character()) {
  #   if (length(char_list) == 0) return(NA)
  #   
  #   mode_value <- function(x) {
  #     x <- x[!is.na(x)]
  #     if (length(x) == 0) return(NA_character_)
  #     ux <- unique(x)
  #     ux[which.max(tabulate(match(x, ux)))]
  #   }
  #   
  #   summarise_fun <- function(colname) {
  #     if (colname %in% mode_cols) {
  #       function(x) {
  #         x <- x[!is.na(x) & x != ""]
  #         if (length(x) == 0) return(NA_character_)
  #         ux <- unique(x)
  #         ux[which.max(tabulate(match(x, ux)))]
  #       }
  #     } else {
  #       function(x) {
  #         x <- x[!is.na(x) & x != ""]
  #         if (length(x) == 0) return(NA_character_)
  #         str_c(x, collapse = ", ")
  #       }
  #     }
  #   }
  #   
  #   summarise_num <- function(x) {
  #     x <- na.omit(x)
  #     if (length(x) == 0) return(NA_character_)
  #     str_c(x, collapse = ",")
  #   }
  #   
  #   df <- purrr::reduce(char_list, full_join, by = c("id_data_individuals", "id_sub_plots"))
  #   
  #   df1 <- df %>%
  #     group_by(id_data_individuals, id_sub_plots) %>%
  #     summarise(across(
  #       everything(),
  #       .fns = function(x, colname = cur_column()) {
  #         if (is.character(x)) {
  #           summarise_fun(colname)(x)
  #         } else if (is.numeric(x)) {
  #           x <- na.omit(x)
  #           if (length(x) == 0) NA_real_ else x[1]
  #         } else {
  #           x
  #         }
  #       }
  #     ), .groups = "drop")
  #   
  #   df2 <- df1 %>%
  #     select(-id_sub_plots) %>%
  #     group_by(id_data_individuals) %>%
  #     summarise(across(
  #       everything(),
  #       .fns = function(x, colname = cur_column()) {
  #         if (is.character(x)) {
  #           summarise_fun(colname)(x)
  #         } else if (is.numeric(x)) {
  #           summarise_num(x)
  #         } else {
  #           x
  #         }
  #       }
  #     ), .groups = "drop")
  #   
  #   return(df2)
  # }
  # 
  # traits_char <- process_traits_char(char_list = traits_measures$traits_char, 
  #                                    mode_cols = "light_observations")
  
  process_traits_char_dt <- function(char_list, mode_cols = character()) {
    if (length(char_list) == 0) return(NA)
    
    dt <- rbindlist(char_list, fill = TRUE, use.names = TRUE)
    data.table::setDT(dt)
    dt <- unique(dt)
    
    for (col in names(dt)) {
      if (is.character(dt[[col]])) {
        dt[[col]][dt[[col]] == ""] <- NA_character_
      }
    }
    
    trait_cols <- setdiff(names(dt), c("id_data_individuals", "id_sub_plots"))
    
    dt1 <- dt[, {
      out <- list()
      for (col in trait_cols) {
        x <- get(col)
        if (is.character(x)) {
          x <- x[!is.na(x)]
          if (col %in% mode_cols) {
            val <- if (length(x) == 0) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
          } else {
            val <- if (length(x) == 0) NA_character_ else str_c(x, collapse = ", ")
          }
        } else if (is.numeric(x)) {
          val <- if (all(is.na(x))) NA_real_ else as.numeric(x[!is.na(x)][1])
        } else {
          val <- x
        }
        out[[col]] <- val
      }
      out
    }, by = .(id_data_individuals, id_sub_plots)]
    
    # Agrégation finale par individu
    dt2 <- dt1[, {
      out <- list()
      for (col in trait_cols) {
        x <- get(col)
        if (is.character(x)) {
          x <- x[!is.na(x)]
          if (col %in% mode_cols) {
            val <- if (length(x) == 0) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
          } else {
            val <- if (length(x) == 0) NA_character_ else str_c(x, collapse = ", ")
          }
        } else if (is.numeric(x)) {
          x <- x[!is.na(x)]
          val <- if (length(x) == 0) NA_character_ else str_c(as.numeric(x), collapse = ",")
        } else {
          val <- x
        }
        out[[col]] <- val
      }
      out
    }, by = .(id_data_individuals)]
    
    return(as_tibble(dt2))
  }
  
  traits_char <- process_traits_char_dt(char_list = traits_measures$traits_char, 
                                     mode_cols = "light_observations")

  # if (length(traits_measures$traits_char) > 0) {
  # 
  #   traits_char <-
  #     purrr::reduce(traits_measures$traits_char,
  #                   dplyr::full_join,
  #                   by = c('id_data_individuals', 'id_sub_plots'))
  # 
  #   traits_char <-
  #     traits_char %>%
  #     group_by(id_data_individuals, id_sub_plots) %>%
  #     summarise(across(where(is.character), ~ stringr::str_c(.[!is.na(.)],
  #                                                            collapse = ", ")), # ~ paste(.x[!is.na(.x)], collapse = ",")
  #               across(where(is.numeric), ~ .x[!is.na(.x)][1])) %>%
  #     ungroup() %>%
  #     mutate(across(where(is.character), ~ na_if(., "")))
  # 
  #   traits_char <-
  #     traits_char %>%
  #     group_by(id_data_individuals) %>%
  #     select(-id_sub_plots) %>%
  #     dplyr::summarise(
  #       across(where(is.character), ~ stringr::str_c(.[!is.na(.)],
  #                                                    collapse = ", ")), # ~ paste(.x[!is.na(.x) & .x != ""], collapse = ",")
  #       across(where(is.numeric), ~ paste(.x[!is.na(.x)], collapse = ","))
  #     ) %>%
  #     mutate(across(where(is.character), ~ na_if(., "")))
  # 
  # } else {
  #   traits_char <- NA
  # }

  # process_traits_num <- function(num_list) {
  #   if (length(num_list) == 0) return(NA)
  #   
  #   purrr::reduce(num_list, full_join, by = c("id_data_individuals", "id_sub_plots")) %>%
  #     group_by(id_data_individuals, id_sub_plots) %>%
  #     summarise(
  #       across(starts_with("id_trait"), ~ str_c(na.omit(.x), collapse = ", ")),
  #       across(!starts_with("id_trait"), ~ mean(.x, na.rm = TRUE)),
  #       .groups = "drop"
  #     ) %>%
  #     select(-id_sub_plots) %>%
  #     group_by(id_data_individuals) %>%
  #     summarise(
  #       across(!starts_with("id_"), ~ mean(na.omit(.x))),
  #       across(starts_with("id_"), ~ str_c(na.omit(.x), collapse = ", ")),
  #       .groups = "drop"
  #     )
  # }
  
  process_traits_num_dt <- function(df, collapse_cols = character()) {
    if (!data.table::is.data.table(df)) data.table::setDT(df)
    
    # Séparation des colonnes à collapse vs moyenne
    collapse_cols <- intersect(collapse_cols, names(df))
    mean_cols <- setdiff(names(df), c("id_data_individuals", "id_sub_plots", collapse_cols))
    
    # Étape 1 : résumé par (id_data_individuals, id_sub_plots)
    df1 <- df[, lapply(.SD, function(x) mean(x, na.rm = TRUE)),
              by = .(id_data_individuals, id_sub_plots),
              .SDcols = mean_cols]
    
    df2 <- df[, lapply(.SD, function(x) paste(na.omit(x), collapse = ",")),
              by = .(id_data_individuals, id_sub_plots),
              .SDcols = collapse_cols]
    
    # Fusion des deux résumés intermédiaires
    df_combined <- merge(df1, df2, by = c("id_data_individuals", "id_sub_plots"), all = TRUE)
    
    # Étape 2 : résumé final par individu
    df_final <- df_combined[, lapply(.SD, function(x) {
      if (is.numeric(x)) mean(x, na.rm = TRUE)
      else {
        val <- paste(na.omit(x), collapse = ",")
        if (val == "") NA_character_ else val
      }
    }), by = id_data_individuals]
    
    return(as_tibble(df_final))
  }
  
  # traits_num <- process_traits_num(num_list = traits_measures$traits_num)
  
  cli::cli_alert_info("Extracting numeric values")
  
  traits_num <-
    process_traits_num_dt(df = purrr::reduce(
      traits_measures$traits_num,
      full_join,
      by = c("id_data_individuals", "id_sub_plots")
    ))
  
  # if (length(traits_measures$traits_num) > 0) {
  # 
  #   traits_num <-
  #     purrr::reduce(traits_measures$traits_num,
  #                   dplyr::full_join,
  #                   by = c('id_data_individuals', 'id_sub_plots'))
  # 
  #   traits_num <-
  #     traits_num %>%
  #     group_by(id_data_individuals, id_sub_plots) %>%
  #     summarise(across(starts_with("id_trait"), ~ stringr::str_c(.[!is.na(.)],
  #                                                                collapse = ", ")), # ~ paste(.x[!is.na(.x)], collapse = ",")
  #               across(!starts_with("id_trait"), ~ mean(.x, na.rm = T)))
  # 
  #   issue_num <-
  #     purrr::reduce(traits_measures$issue_num,
  #                   dplyr::full_join,
  #                   by = c('id_data_individuals', 'id_sub_plots'))
  # 
  #   issue_num <-
  #     issue_num %>%
  #     group_by(id_data_individuals, id_sub_plots) %>%
  #     summarise(across(starts_with("id_trait"), ~ .x[!is.na(.x)][1]),
  #               across(!starts_with("id_trait"), ~ .x[!is.na(.x)][1]))
  # 
  #   traits_num <-
  #     traits_num %>%
  #     select(-id_sub_plots) %>%
  #     group_by(id_data_individuals) %>%
  #     dplyr::summarise(
  #       across(!starts_with("id_"), ~ mean(.x[!is.na(.x)])),
  #       across(starts_with("id_"), ~ stringr::str_c(.[!is.na(.)],
  #                                                   collapse = ", ")) # ~ paste(.x[!is.na(.x)], collapse = ",")
  #     )
  # 
  #   issue_num <-
  #     issue_num %>%
  #     select(-id_sub_plots) %>%
  #     group_by(id_data_individuals) %>%
  #     dplyr::summarise(
  #       across(!starts_with("id_"), ~ last(.x[!is.na(.x)])),
  #       across(starts_with("id_"), ~ last(.x[!is.na(.x)]))
  #     )
  # 
  # } else {
  #   traits_num <- NA
  #   issue_num <- NA
  # }
  
  process_issues_num <- function(issue_list) {
    if (length(issue_list) == 0) return(NA)
    
    purrr::reduce(issue_list, full_join, by = c("id_data_individuals", "id_sub_plots")) %>%
      group_by(id_data_individuals, id_sub_plots) %>%
      summarise(
        across(everything(), ~ first(na.omit(.x))),
        .groups = "drop"
      ) %>%
      select(-id_sub_plots) %>%
      group_by(id_data_individuals) %>%
      summarise(
        across(everything(), ~ last(na.omit(.x))),
        .groups = "drop"
      )
  }
  
  issue_num <- process_issues_num(traits_measures$issue_num)

  all_traits_list <- list(traits_char = traits_char,
                          traits_num = traits_num,
                          issue_num = issue_num)


  return(all_traits_list)


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

  if(!any(new_valuetype==c('numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character', 'table_data_liste_plots', 'table_colnam')))
    stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character', 'table_data_liste_plots' or 'table_colnam'")

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

  # Q <- utils::askYesNo("confirm adding this trait?")
  Q <- choose_prompt(message = "confirm adding this trait ?")

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
        ) %>%
        mutate(id_ind_meas_feat = as.character(id_ind_meas_feat))
    } else {
      numeric_subplots_pivot <- NULL
    }

    if (any(extracted_data$valuetype == "character")) {
      character_feat_pivot <-
        extracted_data %>%
        filter(valuetype == "character") %>%
        select(id_trait_measures, typevalue_char, trait, id_ind_meas_feat)  %>%
        tidyr::pivot_wider(
          names_from = "trait",
          values_from = "typevalue_char",
          values_fn = ~ paste(.x, collapse = "|")
        ) %>%
        mutate(id_ind_meas_feat = as.character(id_ind_meas_feat))
    } else {
      character_feat_pivot <- NULL
    }

    if (any(extracted_data$valuetype == "ordinal")) {
      ordinal_subplots_pivot <-
        extracted_data %>%
        filter(valuetype == "ordinal") %>%
        select(id_trait_measures, typevalue_char, trait, id_ind_meas_feat)  %>%
        tidyr::pivot_wider(
          names_from = "trait",
          values_from = "typevalue_char",
          values_fn = ~ paste(.x, collapse = "|")
        ) %>%
        mutate(id_ind_meas_feat = as.character(id_ind_meas_feat))
    } else {
      ordinal_subplots_pivot <- NULL
    }


    if (any(grepl("table_colnam", extracted_data$valuetype))) {

      table_ids_subplots <- extracted_data %>%
        filter(grepl("table_", valuetype))

      allvalutype <- distinct(table_ids_subplots, valuetype)


      table_valutype_list <- vector('list', nrow(allvalutype))
      for (i in 1:nrow(allvalutype)) {

        ids_ <-
          case_when(
            table_ids_subplots$valuetype[i] == "table_colnam" ~ "id_table_colnam"
          )

        col_to_keep_ <-
          case_when(
            table_ids_subplots$valuetype[i] == "table_colnam" ~ "colnam"
          )

        table_collected <-
          tbl(mydb, table_ids_subplots$valuetype[i]) %>%
          collect()

        table_ids_subplots <-
          table_ids_subplots %>%
          left_join(table_collected %>%
                      dplyr::select(all_of(c(col_to_keep_, ids_))),
                    by = c("typevalue" = ids_)) %>%
          mutate(typevalue_char = !!rlang::parse_expr(col_to_keep_)) %>%
          dplyr::select(-all_of(col_to_keep_))

        table_valutype_list[[i]] <-
          table_ids_subplots %>%
          select(id_trait_measures, typevalue_char, trait, id_ind_meas_feat) %>%
          mutate(id_ind_meas_feat = as.character(id_ind_meas_feat)) %>%
          tidyr::pivot_wider(
            names_from = "trait",
            values_from = c("typevalue_char", "id_ind_meas_feat"),
            values_fn = ~ paste(., collapse = "|")
          ) %>%
          rename(id_ind_meas_feat =  id_ind_meas_feat_colnam,
                 colnam = typevalue_char_colnam)
      }
    } else {
      table_valutype_list <- NULL
    }

    # all_feat_pivot <-
    #   c(list(character_subplots_pivot),
    #     list(numeric_subplots_pivot),
    #     table_valutype_list)

    all_feat_pivot <-
      bind_rows(list(list(character_feat_pivot),
                     list(numeric_subplots_pivot),
                     list(ordinal_subplots_pivot),
                     table_valutype_list))

    # all_feat_pivot <-
    #   purrr::reduce(all_feat_pivot[!unlist(lapply(all_feat_pivot, is.null))],
    #                 dplyr::full_join,
    #                 by = c('id_trait_measures'))


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
          dplyr::select(idtax_n, id_n, sous_plot_name) %>%
          dplyr::filter(id_n %in% !!unique(new_data_renamed$id_n)) %>%
          dplyr::collect() %>%
          dplyr::mutate(rrr = 1),
        by = c("id_n" = "id_n")
      )
    
    if (dplyr::filter(link_individuals, is.na(rrr)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_individuals, is.na(sous_plot_name)))
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
          "ind_num_sous_plot"
        
        data_trait <-
          data_trait %>%
          dplyr::rename_at(dplyr::vars(all_of(individual_plot_field)), ~ individual_plot)
        
        
        ## not numeric or missing individuals tag
        nbe_not_numeric <-
          suppressWarnings(which(is.na(as.numeric(data_trait$ind_num_sous_plot))))
        
        data_trait <-
          data_trait %>%
          dplyr::mutate(ind_num_sous_plot = as.numeric(ind_num_sous_plot))
        
        if(length(nbe_not_numeric) > 0) {
          cli::cli_alert_warning(
            "Number of non numeric (or missing) value in column indicating invividual number in plot : {length(nbe_not_numeric)}"
          )
          print(nbe_not_numeric)
          
          data_trait <-
            data_trait %>%
            filter(!is.na(ind_num_sous_plot))
          
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
          dplyr::select(ind_num_sous_plot, id_table_liste_plots_n,
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
                             by=c("ind_num_sous_plot" = "ind_num_sous_plot"))
          
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
                            ind_num_sous_plot,
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
            dplyr::select(trait, plot_name, ind_num_sous_plot, id_data_individuals, id_new_data)
          
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
        
        
        
      }
      
    } else{
      
      cli::cli_alert_info("no added data for {trait} - no values different of 0")
      
    }
  }
  
  # linked_problems_individuals_list <-
  #   linked_problems_individuals_list %>%
  #   dplyr::select(plot_name,
  #                 ind_num_sous_plot,
  #                 country,
  #                 leaf_area,
  #                 specific_leaf_area,
  #                 dbh.x,
  #                 dbh.y,
  #                 original_tax_name,
  #                 corrected.name,
  #                 full_name_no_auth,
  #                 id_table_liste_plots_n,
  #                 ddlon,
  #                 ddlat) %>%
  #   left_join(tbl(mydb, "data_liste_plots") %>%
  #               dplyr::select(plot_name, id_liste_plots) %>%
  #               collect(), by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
  #   rename(dbh_provided = dbh.x,
  #          dbh_database = dbh.y,
  #          name_provided = original_tax_name,
  #          name_provided_corrected = corrected.name,
  #          name_database = full_name_no_auth,
  #          plot_name_provided = plot_name.x,
  #          plot_name_corrected = plot_name.y)
  
  
  if(exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))
  
  if(!exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data))
  
}


