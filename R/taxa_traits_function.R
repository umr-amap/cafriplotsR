
# query_traits_measures <- function(idtax = NULL,
#                                   idtax_good = NULL,
#                                   add_taxa_info = FALSE,
#                                   id_trait = NULL,
#                                   trait_cat_mode = "most_frequent",
#                                   verbose = TRUE,
#                                   pivot_table = TRUE,
#                                   include_remarks = FALSE,
#                                   extract_trait_measures_features = FALSE) {
#   
#   mydb <- call.mydb()
#   mydb_taxa <- call.mydb.taxa()
#   
#   tbl <- "table_traits_measures"
#   tbl2 <- "table_traits"
#   
#   if (!is.null(idtax)) {
#     
#     if (!is.null(idtax_good)) {
#       
#       idtax_tb <- dplyr::tibble(idtax = idtax, idtax_good = idtax_good)
#       
#     } else {
#       
#       table_taxa <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa)
#       
#       # table_taxa <- try_open_postgres_table(table = "table_idtax", con = mydb)
#       
#       ids_syn <- table_taxa %>%
#         dplyr::select(idtax_n, idtax_good_n) %>%
#         dplyr::filter(idtax_good_n %in% !!idtax) %>%
#         dplyr::collect()
#       
#       idtax <- unique(c(idtax, ids_syn$idtax_n))
#       
#       idtax_tb <- table_taxa %>%
#         # dplyr::select(idtax_n, idtax_good_n) %>%
#         dplyr::filter(idtax_n %in% !!idtax) %>%
#         dplyr::collect() %>%
#         dplyr::rename(idtax = idtax_n,
#                       idtax_good = idtax_good_n)
#       
#     }
#     
#     
#     if (!is.null(id_trait)) {
#       sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.fk_id_trait = {`tbl2`}.id_trait  WHERE id_trait IN ({vals2*}) AND idtax IN ({vals*})",
#                             vals = idtax, .con = mydb_taxa, vals2 = id_trait)
#     } else {
#       sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.fk_id_trait = {`tbl2`}.id_trait  WHERE idtax IN ({vals*})",
#                             vals = idtax, .con = mydb_taxa)
#     }
#     
#   } else {
#     
#     table_taxa <- try_open_postgres_table(table = "table_taxa", con = mydb_taxa)
#     
#     sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.fk_id_trait = {`tbl2`}.id_trait  WHERE id_trait IN ({vals2*})",
#                           .con = mydb_taxa, vals2 = id_trait)
#     
#     idtax_tb <- 
#       table_taxa %>%
#       dplyr::rename(idtax = idtax_n,
#                     idtax_good = idtax_good_n)
#     
#   }
#   
#   traits_found <-
#     func_try_fetch(con = mydb_taxa, sql = sql)
#   
#   # traits_found <- 
#   #   traits_found %>% 
#   #   rename(id_trait = fk_id_trait)
#   
#   if (is.null(idtax)) {
#     
#     idtax <- unique(traits_found$idtax)
#     
#     idtax_tb <- 
#       idtax_tb %>%
#       dplyr::filter(idtax %in% !!idtax) %>%
#       dplyr::collect()
#   }
#   
#   
#   # sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax IN ({vals*}) LEFT OUTER JOIN table_traits ON table_traits_measures.id_trait = table_traits.id_trait",
#   #                      vals = idtax, .con = mydb_taxa)
#   
#   
#   traits_found <-
#     traits_found %>%
#     dplyr::left_join(idtax_tb %>%
#                        dplyr::select(idtax, idtax_good),
#                      by = c("idtax" = "idtax")) %>%
#     dplyr::mutate(idtax = ifelse(is.na(idtax_good), idtax, idtax_good)) %>%
#     dplyr::select(-idtax_good)
#   
#   if (extract_trait_measures_features) {
#     
#     feats <- query_traits_measures_features(id_trait_measures = traits_found$id_trait_measures, src = "taxa")
#     
#     if (any(!is.na(feats$all_feat_pivot))) {
#       
#       feats_unique <-
#         feats$all_feat_pivot %>%
#         mutate(id_taxa_trait_feat = as.character(id_taxa_trait_feat)) %>%
#         group_by(id_trait_measures) %>%
#         summarise(across(where(is.numeric), ~mean(., na.rm = T)),
#                   across(where(is.character), ~paste(.[!is.na(.)], collapse = "|"))) %>%
#         mutate(across(where(is.character), ~na_if(.x, "")))
#       
#       traits_found <- 
#         traits_found %>% 
#         left_join(feats_unique,
#                   by = c("id_trait_measures" = "id_trait_measures"))
#       
# 
#     }
#     
#   }
#   
#   if (add_taxa_info) {
#     
#     taxa_infos <-
#       add_taxa_table_taxa(ids = traits_found$idtax) %>%
#       collect() %>%
#       dplyr::select(idtax_n,
#                     idtax_good_n,
#                     tax_fam,
#                     tax_gen,
#                     tax_esp,
#                     tax_infra_level)
#     
#     traits_found <-
#       traits_found %>%
#       left_join(taxa_infos,
#                 by = c("idtax" = "idtax_n"))
#     
#   }
#   
#   if(nrow(traits_found) > 0) {
#     
#     if (any(traits_found$valuetype == "categorical")) {
#       
#       traits_idtax_char <-
#         traits_found %>%
#         dplyr::filter(valuetype == "categorical") %>%
#         dplyr::select(idtax,
#                       trait,
#                       traitvalue_char,
#                       basisofrecord,
#                       measurementremarks,
#                       id_trait_measures)
#       
#       if (!include_remarks)
#         traits_idtax_char <-
#           traits_idtax_char %>%
#           dplyr::select(-measurementremarks)
#       
#       if (pivot_table) {
#         traits_idtax_char <-
#           traits_idtax_char %>%
#           mutate(rn = 1:nrow(.)) %>%
#           # dplyr::mutate(rn = data.table::rowid(trait)) %>%
#           tidyr::pivot_wider(
#             names_from = trait,
#             values_from = c(traitvalue_char, basisofrecord, id_trait_measures),
#             names_prefix = "taxa_level_"
#           ) %>%
#           dplyr::select(-rn)
#         
#         names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))
#         
#         traits_idtax_concat <-
#           traits_idtax_char %>%
#           dplyr::select(idtax, starts_with("id_trait_")) %>%
#           dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
#           dplyr::group_by(idtax) %>%
#           dplyr::mutate(dplyr::across(where(is.character),
#                                       ~ stringr::str_c(.[!is.na(.)],
#                                                        collapse = ", "))) %>%
#           dplyr::ungroup() %>%
#           dplyr::distinct()
#         
#         
#         if (trait_cat_mode == "all_unique") {
#           
#           if (verbose) cli::cli_alert_info("Extracting all unique values for categorical traits")
#           
#           ### concatenate all distinct values
#           traits_idtax_char <-
#             traits_idtax_char %>%
#             dplyr::select(-starts_with("id_trait_")) %>%
#             dplyr::group_by(idtax) %>%
#             dplyr::mutate(dplyr::across(where(is.character),
#                                         ~ stringr::str_c(.[!is.na(.)],
#                                                          collapse = ", "))) %>%
#             dplyr::distinct() %>%
#             dplyr::ungroup()
#           
#         }
#         
#         if (trait_cat_mode == "most_frequent") {
#           
#           if (verbose) cli::cli_alert_info("Extracting most frequent value for categorical traits")
#           
#           traits_idtax_char <-
#             traits_idtax_char %>%
#             dplyr::select(-starts_with("id_trait_")) %>%
#             group_by(idtax, across(where(is.character))) %>%
#             count() %>%
#             arrange(idtax, desc(n)) %>%
#             ungroup() %>%
#             group_by(idtax) %>%
#             dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))
#           
#           
#           # trait_multiple_val <- max_unique_val %>% filter(max_n > 1) %>% pull(trait)
#           #
#           # traits_idtax_char_multiple <-
#           #   traits_idtax_char %>%
#           #   dplyr::select(-starts_with("id_trait_")) %>%
#           #   group_by(idtax, across({{trait_multiple_val}})) %>%
#           #   count() %>%
#           #   arrange(idtax, desc(n)) %>%
#           #   ungroup() %>%
#           #   group_by(idtax) %>%
#           #   dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))
#           #
#           # trait_unique_val <- max_unique_val %>% filter(max_n == 1) %>% pull(trait)
#           #
#           # traits_idtax_char_multiple <-
#           #   traits_idtax_char %>%
#           #   # dplyr::select(-starts_with("id_trait_"),
#           #   #               idtax,
#           #   #               all_of(trait_unique_val),
#           #   #               -all_of(trait_multiple_val)) %>%
#           #   group_by(idtax, across({{trait_unique_val}})) %>%
#           #   count() %>%
#           #   ungroup() %>%
#           #   group_by(idtax) %>%
#           #   dplyr::summarise_if(is.character, ~ .x[!is.na(.x)])
#           
#           
#         }
#         
#         traits_idtax_char <-
#           left_join(traits_idtax_char,
#                     traits_idtax_concat, by = c("idtax" = "idtax"))
#         
#       } else {
#         
#         if (extract_trait_measures_features) {
#           if (feats_unique %>% 
#               filter(id_trait_measures %in% traits_idtax_char$id_trait_measures) %>% 
#               nrow() > 0)
#             traits_idtax_char <-
#               traits_idtax_char %>%
#               dplyr::left_join(feats_unique,
#                                by = c("id_trait_measures" = "id_trait_measures"))
#         } 
#           
#           
#         
#       }
#       
#       if (add_taxa_info) {
#         
#         traits_idtax_char <-
#           traits_idtax_char %>%
#           left_join(taxa_infos,
#                     by = c("idtax" = "idtax_n"))
#       }
#       
#     } else {
#       
#       traits_idtax_char <- NA
#       
#     }
#     
#     if (any(traits_found$valuetype == "numeric")) {
#       
#       traits_idtax_num <-
#         traits_found %>%
#         filter(valuetype == "numeric") %>%
#         dplyr::select(idtax,
#                       trait,
#                       traitvalue,
#                       basisofrecord,
#                       id_trait_measures)
#       
#       if (pivot_table) {
#         
#         traits_idtax_num <- 
#           traits_idtax_num  %>%
#           dplyr::mutate(rn = data.table::rowid(trait)) %>%
#           tidyr::pivot_wider(
#             names_from = trait,
#             values_from = c(traitvalue, basisofrecord, id_trait_measures),
#             names_prefix = "taxa_level_"
#           ) %>%
#           dplyr::select(-rn) %>%
#           dplyr::mutate(across(starts_with("id_trait_"), as.character))
#         
#         names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))
#         
#         traits_idtax_concat <-
#           traits_idtax_num %>%
#           dplyr::select(idtax, starts_with("id_trait_")) %>%
#           dplyr::mutate(dplyr::across(starts_with("id_trait_"), as.character)) %>%
#           dplyr::group_by(idtax) %>%
#           dplyr::mutate(dplyr::across(where(is.character),
#                                       ~ stringr::str_c(.[!is.na(.)],
#                                                        collapse = ", "))) %>%
#           dplyr::ungroup() %>%
#           dplyr::distinct()
#         
#         ### concatenate all distinct values
#         traits_idtax_num_summarized <-
#           traits_idtax_num %>%
#           dplyr::select(-starts_with("id_trait_")) %>%
#           dplyr::group_by(idtax) %>%
#           dplyr::summarise(dplyr::across(where(is.numeric),
#                                          .fns= list(mean = ~mean(., na.rm = T),
#                                                     sd = ~sd(., na.rm = T),
#                                                     n = ~length(.[!is.na(.)])),
#                                          .names = "{.col}_{.fn}"))
#         
#         traits_idtax_num <-
#           traits_idtax_num_summarized %>%
#           left_join(traits_idtax_concat, by = c("idtax" = "idtax"))
#       } else {
#         
#         if (extract_trait_measures_features) {
#           if (feats_unique %>% 
#               filter(id_trait_measures %in% traits_idtax_num$id_trait_measures) %>% 
#               nrow() > 0)
#             traits_idtax_num <-
#               traits_idtax_num %>%
#               dplyr::left_join(feats_unique,
#                                by = c("id_trait_measures" = "id_trait_measures"))
#         } 
#         
#         
#         
#       }
#       
#       
#       # if (!is.null(idtax_good))
#       traits_idtax_num <-
#         traits_idtax_num %>%
#         left_join(idtax_tb %>%
#                     dplyr::select(idtax, idtax_good),
#                   by = c("idtax" = "idtax")) %>%
#         # dplyr::select(idtax, idtax_good) %>%
#         dplyr::mutate(idtax = ifelse(is.na(idtax_good), idtax, idtax_good)) %>%
#         dplyr::select(-idtax_good)
#       
#       if (add_taxa_info) {
#         
#         traits_idtax_num <-
#           traits_idtax_num %>%
#           left_join(taxa_infos,
#                     by = c("idtax" = "idtax_n"))
#       }
#       
#       
#     } else {
#       
#       traits_idtax_num <- NA
#       
#     }
#     
#   } else {
#     
#     traits_found <- NA
#     traits_idtax_num <- NA
#     traits_idtax_char <- NA
#     
#   }
#   
# 
#   
#   return(list(traits_found = traits_found,
#               traits_idtax_num = traits_idtax_num,
#               traits_idtax_char = traits_idtax_char))
# }

#' Add an observation in trait measurement table at species level
#'
#' Add a trait measure in trait measurement table
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param collector string column name which contain the collector name
#' @param plot_name_field string column name which contain the plot_name for linking
#' @param idtax string column name which contain the individual tag for linking
#' @param id_plot_name string column name which contain the ID of plot_name
#' @param id_tag_plot string column name which contain the ID of individuals table
#' @param add_data logical whether or not data should be added - by default FALSE
#'
#' @export
add_sp_traits_measures <- function(new_data,
                                   col_names_select = NULL,
                                   col_names_corresp = NULL,
                                   traits_field,
                                   collector = NULL,
                                   idtax = NULL,
                                   features_field = NULL,
                                   add_data = FALSE,
                                   ask_before_update = TRUE) {

  
  
  for (i in 1:length(traits_field))
    if (!any(colnames(new_data) == traits_field[i]))
      stop(paste("traits_field provide not found in new_data", traits_field[i]))
  
  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))
  
  mydb_taxa <- call.mydb.taxa()

  if(is.null(idtax))
    stop("provide a column containing link to taxa")

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = idtax,
                 col_new = "idtax")

  if (!is.null(col_names_select) & !is.null(col_names_corresp)) {
    new_data_renamed <-
      .rename_data(dataset = new_data_renamed,
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

  ### Linking collectors names
  if(!is.null(collector)) {

    new_data_renamed <-
      .rename_data(dataset = new_data,
                   col_old = collector,
                   col_new = "colnam")

    # new_data_renamed <-
    #   .link_colnam(
    #     data_stand = new_data_renamed,
    #     collector_field = "colnam"
    #   )

    new_data_renamed <-
      .link_table(
        data_stand = new_data_renamed,
        column_searched = "colnam",
        column_name = "colnam",
        id_field = "id_colnam",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )

  } else {

    new_data_renamed <-
      new_data_renamed %>%
      mutate(idcolnam = NA_real_)

  }

  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(traits_field))
  for (i in 1:length(traits_field)) {

    trait <- traits_field[i]
    if (!any(colnames(new_data_renamed) == trait))
      stop(paste("trait field not found", trait))

    data_trait <-
      new_data_renamed

    trait_name <-
      "trait"
    data_trait <-
      data_trait %>%
      dplyr::rename_at(dplyr::vars(all_of(trait)), ~ trait_name)

    data_trait <-
      data_trait %>%
      dplyr::filter(!is.na(trait))

    if (any(data_trait$trait == 0)) {


      add_0 <- 
        choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
      
      
      if(!add_0)
        data_trait <-
          data_trait %>%
          dplyr::filter(trait != 0)

    }

    if(nrow(data_trait) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_trait <-
        .link_sp_trait(data_stand = data_trait, trait = trait)

      queried_trait <-
        query_trait(id_trait = data_trait %>%
                      dplyr::distinct(id_trait) %>%
                      pull())

      ## see what type of value numeric of character
      valuetype <-
        queried_trait %>%
        dplyr::select(valuetype, id_trait, factorlevels, relatedterm, list_factors)

      if (!any(is.na(unlist(queried_trait$list_factors)))) {

        TypeValue <- "character"

        cli::cli_alert_info("categorical trait: check if values are in factorlevels")

        all_factor_levels <-
          queried_trait$list_factors[[1]] %>%
          mutate(true_value = NA) %>%
          mutate(true_value = as.character(true_value))

        for (j in 1:nrow(all_factor_levels)) {

          selected_id <- .find_cat(value_to_search = all_factor_levels$value[j],
                                   compared_table = all_factor_levels,
                                   column_name = "value")

          level_selected <-
            selected_id$sorted_matches %>%
            slice(as.numeric(selected_id$selected_name))

          all_factor_levels <-
            all_factor_levels %>%
            mutate(true_value = replace(true_value,
                                        value == all_factor_levels$value[j],
                                        level_selected$comp_value))

        }

        data_trait <-
          data_trait %>%
          left_join(all_factor_levels, by = c("trait" = "value")) %>%
          dplyr::select(-trait) %>%
          dplyr::rename(trait = true_value)

        if(data_trait %>% dplyr::pull(trait) %>% is.na() %>% any()) {

          cli::cli_alert_danger("Some value are not found in accepted factor for this trait : {unlist(queried_trait$list_factors[[1]])}")

          data_trait %>%
            filter(is.na(trait))

        }

      }

      if (valuetype$valuetype == "numeric")
        TypeValue <- "numeric"

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
          mutate(basisofrecord = rep(choices$basis[as.numeric(selected_basisofrecord)], nrow(.)))
      }

      ### choosing measurementremarks if none
      cli::cli_h3("basis")
      if (!any(colnames(data_trait) == "measurementremarks")) {

        selected_measurementremarks <-
          readline(prompt = "Add measurementremarks ? 'enter if none : ")

        if (selected_measurementremarks != "") {

          data_trait <-
            data_trait %>%
            mutate(measurementremarks = rep(selected_measurementremarks, nrow(.)))

        }
      }

      ### checking if any duplicates in data to add
      if (data_trait %>% dplyr::distinct() %>% nrow() != nrow(data_trait)) {

        duplicates_lg <- duplicated(data_trait)

        cli::cli_alert_warning("Duplicates in new data for {trait} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        cf_merge <-
          choose_prompt(message = "confirm merging duplicates?")
        

        if (cf_merge) {

          data_trait <- data_trait %>% dplyr::distinct()
        } else{
          stop()
        }

      }

      cli::cli_h3(".add_modif_field")
      data_trait <-
        .add_modif_field(dataset = data_trait)

      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          idtax = data_trait$idtax,
          decimallatitude =
            ifelse(rep(
              any(colnames(data_trait) == "decimallatitude"), nrow(data_trait)
            ), data_trait$decimallatitude, NA),
          decimallongitude =
            ifelse(rep(
              any(colnames(data_trait) == "decimallongitude"), nrow(data_trait)
            ), data_trait$decimallongitude, NA),
          elevation = ifelse(rep(
            any(colnames(data_trait) == "elevation"), nrow(data_trait)
          ), data_trait$elevation, NA),
          verbatimlocality = ifelse(rep(
            any(colnames(data_trait) == "verbatimlocality"), nrow(data_trait)
          ), data_trait$verbatimlocality, NA),
          basisofrecord = data_trait$basisofrecord,
          reference = ifelse(rep(
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
          fk_id_trait = data_trait$id_trait,
          traitvalue =
            ifelse(
              rep(any(TypeValue == "numeric"), nrow(data_trait))
              ,
              data_trait$trait,
              NA
            ),
          traitvalue_char = ifelse(
            rep(any(TypeValue == "character"), nrow(data_trait))
            ,
            data_trait$trait,
            NA
          ),
          original_tax_name = ifelse(rep(
            any(colnames(data_trait) == "original_tax_name"), nrow(data_trait)
          ), data_trait$original_tax_name, NA),
          issue = data_trait$issue,
          date_modif_d = data_trait$date_modif_d,
          date_modif_m = data_trait$date_modif_m,
          date_modif_y = data_trait$date_modif_y
        )

      list_add_data[[i]] <-
        data_to_add

      print(data_to_add)

      ### identify if measures are already within DB
      cli::cli_alert_info("Identifying if imported values are already in DB")

      trait_id <- unique(data_to_add$fk_id_trait)
      selected_data_traits <-
        data_to_add %>%
        dplyr::select(idtax,
                      traitvalue_char,
                      traitvalue,
                      issue,
                      basisofrecord,
                      fk_id_trait,
                      measurementremarks)

      all_vals <-
        dplyr::tbl(mydb_taxa, "table_traits_measures") %>%
        dplyr::select(idtax,
                      traitvalue_char,
                      traitvalue,
                      issue,
                      basisofrecord,
                      fk_id_trait,
                      measurementremarks) %>%
        dplyr::filter(fk_id_trait == !!trait_id) %>% #, !is.na(id_sub_plots)
        dplyr::collect()

      if (TypeValue == "numeric") {
        all_vals <-
          all_vals %>%
          dplyr::select(-traitvalue_char) %>%
          rename(trait = traitvalue)

        selected_data_traits <-
          selected_data_traits %>%
          dplyr::select(-traitvalue_char) %>%
          rename(trait = traitvalue)

      }


      if (TypeValue == "character") {
        all_vals <-
          all_vals %>%
          dplyr::select(-traitvalue) %>%
          rename(trait = traitvalue_char)

        selected_data_traits <-
          selected_data_traits %>%
          dplyr::select(-traitvalue) %>%
          rename(trait = traitvalue_char)
      }


      duplicated_rows <-
        dplyr::bind_rows(selected_data_traits,
                         all_vals) %>%
        dplyr::filter(is.na(issue)) %>%
        dplyr::group_by(idtax,
                        fk_id_trait,
                        trait,
                        basisofrecord,
                        measurementremarks) %>%
        dplyr::count() %>%
        dplyr::filter(n > 1)


      if (nrow(duplicated_rows) > 1) {
        
        cli::cli_alert_danger("Some values are already in DB")
        print(duplicated_rows %>%
                dplyr::ungroup() %>%
                dplyr::select(idtax, fk_id_trait, basisofrecord))

        exclud_yes <- 
          choose_prompt(message = "Exclude duplicated rows ?")
        
        
        if (exclud_yes) {
          cli::cli_alert_danger("Excluding {nrow(duplicated_rows)} values because already in DB")
          data_to_add <-
            data_to_add %>%
            dplyr::filter(!idtax %in% duplicated_rows$idtax)
          
          
        }
        
        if(nrow(data_trait) < 1) stop("no new values anymore to import after excluding duplicates")
      }

      # print(data_to_add %>%
      #         dplyr::left_join(tbl(mydb, "data_liste_sub_plots") %>%
      #                            select(typevalue, id_type_sub_plot, id_sub_plots) %>%
      #                            collect(), by=c("id_sub_plots"="id_sub_plots"))) %>%
      #   dplyr::left_join(tbl(mydb, "subplotype_list") %>%
      #                      select(id_subplotype, type ) %>%
      #                      collect(), by=c("id_type_sub_plot"="id_subplotype")) %>%
      #   View()

      if (ask_before_update) {
        response <-
          choose_prompt(message = "Confirm add these data to data_traits_measures table ?")
        
      } else {
        response <- TRUE
      }

      if(add_data & response) {

        DBI::dbWriteTable(mydb_taxa, "table_traits_measures",
                          data_to_add, append = TRUE, row.names = FALSE)
        
        cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")
        
        if (!is.null(features_field)) {
          
          imported_data <- tbl(mydb_taxa, "table_traits_measures") %>%
            filter(date_modif_d == !!data_to_add$date_modif_d[1],
                   date_modif_m == !!data_to_add$date_modif_m[1],
                   date_modif_y == !!data_to_add$date_modif_y[1]) %>%
            select(id_trait_measures, idtax) %>%
            collect() %>%
            arrange(id_trait_measures)
          
          ids <- imported_data %>% slice((nrow(imported_data)-nrow(data_to_add)+1):nrow(imported_data))
          
          data_feats <-
            data_trait %>% 
            select(all_of(features_field), idtax) %>%
            mutate(id_trait_measures = ids$id_trait_measures,
                   idtax = ids$idtax)
          
          add_sp_traits_measures_features(
            new_data = data_feats,
            id_trait_measures = "id_trait_measures",
            features = features_field , #
            add_data = T
          )
          
        }
        
      }

    } else {

      cli::cli_alert_info("no added data for {trait} - no values different of 0")

    }
  }

  if(exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))

  if(!exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data))

}



add_sp_traits_measures_features <- function(new_data,
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
      try_open_postgres_table(table = "table_traits_measures", con = mydb_taxa) %>%
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
    
    feat_name <-
      "trait"
    data_feat <-
      data_feat %>%
      dplyr::rename_at(dplyr::vars(all_of(feat)), ~ feat_name)
    
    data_feat <-
      data_feat %>%
      dplyr::filter(!is.na(!!sym(feat_name)))
    
    if(nrow(data_feat) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_feat <-
        .link_sp_trait(data_stand = data_feat, trait = feat)
      
      ## see what type of value numeric of character
      valuetype <-
        data_feat %>%
        dplyr::distinct(id_trait) %>%
        dplyr::left_join(
          dplyr::tbl(mydb_taxa, "table_traits") %>%
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
        
        add_0 <- 
          choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
        
        
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
        
        cf_merge <-
          choose_prompt(message = "confirm merging duplicates?")
        
        
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
        choose_prompt(message = "Confirm add these data to data_ind_measures_feat table?")
      
      
      if(add_data & response) {
        
        DBI::dbWriteTable(mydb_taxa, "table_traits_measures_feat",
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



#' Add a trait in species trait list
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
#' @description
#' See https://terminologies.gfbio.org/terms/ets/pages/index.html for description of each field
#'
#' @export
add_trait_taxa <- function(new_trait = NULL,
                           new_relatedterm = NULL,
                           new_valuetype = NULL,
                           new_maxallowedvalue = NULL,
                           new_minallowedvalue = NULL,
                           new_traitdescription = NULL,
                           new_factorlevels = NULL,
                           new_expectedunit = NULL,
                           new_comments = NULL) {

  if(is.null(new_trait)) stop("define new trait")
  if(is.null(new_valuetype)) stop("define new_valuetype")

  if (!any(new_valuetype == c('numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character')))
    stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', or 'character'")

  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_maxallowedvalue) & !is.integer(new_maxallowedvalue)) stop("valuetype numeric of integer and max value not of this type")
  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_minallowedvalue) & !is.integer(new_minallowedvalue)) stop("valuetype numeric of integer and min value not of this type")

  mydb_taxa <- 
    call.mydb.taxa(pass = NULL, user = NULL, reset = TRUE)
  
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

  Q <- choose_prompt(message = "confirm adding this trait ?")

  if(Q) DBI::dbWriteTable(mydb_taxa, "table_traits", new_data_renamed, append = TRUE, row.names = FALSE)

}



#' Add growth forms to a single taxa
#'
#' Add growth form information to a single taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A tibble
#' @export
add_growth_form_taxa <- function(idtax) {

  mydb_taxa <- 
    call.mydb.taxa(pass = NULL, user = NULL, reset = TRUE)

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
                         add_data = T)

}


#' List of trait
#'
#' Provide list of traits available
#'
#' @return A tibble of all traits
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
traits_taxa_list <- function(id_trait = NULL) {

  all_colnames_ind <-
    try_open_postgres_table(table = "table_traits", con = mydb_taxa) %>%
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






add_sp_trait_measures_features <- function(new_data,
                                         id_trait_measures = "id_trait_measures",
                                         features,
                                         allow_multiple_value = FALSE,
                                         add_data = FALSE) {
  
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
      try_open_postgres_table(table = "table_traits_measures", con = mydb_taxa) %>%
        dplyr::select(id_trait_measures) %>%
        dplyr::filter(id_trait_measures %in% !!unique(new_data_renamed$id_trait_measures)) %>%
        dplyr::collect() %>%
        dplyr::mutate(rrr = 1),
      by = c("id_trait_measures" = "id_trait_measures")
    )
  
  if (dplyr::filter(link_trait_measures, is.na(rrr)) %>%
      nrow() > 0) {
    print(dplyr::filter(link_trait_measures, is.na(rrr)))
    stop("provided trait_measures not found in table_traits_measures")
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
        .link_sp_trait(data_stand = data_feat, trait = feat)
      
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
      
      if (valuetype$valuetype == "table_colnam") {
        
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
        
        cf_merge <- 
          choose_prompt(message = "confirm merging duplicates?")
        
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
        choose_prompt(message = "Confirm add these data to table_traits_measures_feat table?")
      
      if(add_data & response) {
        
        DBI::dbWriteTable(mydb_taxa, "table_traits_measures_feat",
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
#' @param con_taxa Connection to taxa database (optional)
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
    con_taxa = NULL
) {
  
  categorical_mode <- match.arg(categorical_mode)
  format <- match.arg(format)
  
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
    con = con_taxa
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
fetch_taxa_trait_measurements <- function(idtax, trait_ids = NULL, con) {
  
  # Build query with explicit column selection to avoid duplicates
  query <- "
    SELECT 
      tm.id_trait_measures,
      tm.idtax,
      tm.traitvalue,
      tm.traitvalue_char,
      tm.basisofrecord,
      tm.measurementremarks,
      tm.fk_id_trait,
      tl.id_trait,
      tl.trait,
      tl.valuetype,
      tl.traitdescription,
      tl.expectedunit,
      tl.minallowedvalue,
      tl.maxallowedvalue
    FROM table_traits_measures tm
    LEFT JOIN table_traits tl ON tm.fk_id_trait = tl.id_trait
    WHERE 1=1
  "
  
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
  
  DBI::dbGetQuery(con, query)
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
    extract_trait_measures_features = FALSE
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
    include_measurement_features = extract_trait_measures_features
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
  condition_hierarchical <- sapply(strsplit(unlist(condition_hierarchical), '[.]'), `[`, 1)
  
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
query_trait <- function(id_trait = NULL, pattern = NULL) {
  
  mydb_taxa <- call.mydb.taxa()
  
  if (!is.null(id_trait)) {
    cli::cli_alert_info("query trait by id")
    
    table_traits <- try_open_postgres_table(table = "table_traits", con = mydb_taxa)
    
    valuetype <-
      table_traits %>%
      dplyr::filter(id_trait == !!id_trait) %>%
      dplyr::collect()
  }
  
  if (is.null(id_trait) & !is.null(pattern)) {
    
    cli::cli_alert_info("query trait by string pattern")
    
    sql <- glue::glue_sql(paste0("SELECT * FROM table_traits WHERE trait ILIKE '%", pattern, "%'"))
    
    valuetype <- func_try_fetch(con = mydb_taxa, sql = sql)
    
    
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













