


#' List of subplot features
#'
#' Provide list of subplot features of plot
#'
#' @return A tibble of all subplot features
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
subplot_list <- function() {
  if(!exists("mydb")) call.mydb()
  
  nn <-
    try_open_postgres_table(table = "subplotype_list", con = mydb) %>%
    dplyr::collect()
  
  # dbDisconnect(mydb)
  return(nn)
}



.sql_query_subplot <- function(id_subplots, 
                               mydb_ = mydb, 
                               tbl = "data_liste_sub_plots", 
                               tbl2 = "subplotype_list") {
  
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.id_type_sub_plot = {`tbl2`}.id_subplotype WHERE id_subplotype IN ({vals*})",
                        vals = id_subplots, .con = mydb_)
  return(sql)
}


.sql_query_subplot_plot <- function(id_plots, 
                                    mydb_ = mydb, 
                                    tbl = "data_liste_sub_plots", 
                                    tbl2 = "subplotype_list") {
  
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.id_type_sub_plot = {`tbl2`}.id_subplotype WHERE id_table_liste_plots IN ({vals*})",
                        vals = id_plots, .con = mydb_)
  return(sql)
}

.sql_query_subplot_plot_2 <- function(id_plots,
                                      id_subplots,
                                      mydb_ = mydb, 
                                      tbl = "data_liste_sub_plots", 
                                      tbl2 = "subplotype_list") {
  
  sql <-glue::glue_sql("SELECT * FROM {`tbl`} LEFT JOIN {`tbl2`} ON {`tbl`}.id_type_sub_plot = {`tbl2`}.id_subplotype WHERE id_table_liste_plots IN ({vals*}) AND id_subplotype IN ({vals2*})",
                       vals = id_plots, vals2 = id_subplots, .con = mydb)
  return(sql)
}



#' List, selected subplots
#'
#' Table of subplot for selected plots
#'
#' @return A tibble of all subplots
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @param id_plots integer
#' @param ids_subplots integer
#' @param team_lead string fuzzy person name to look for
#' @param plot_name string fuzzy plot name to look for
#' @param tag numeric exact tag number of the plot
#' @param country string fuzzy country name to look for
#' @param province string fuzzy province name to look for
#' @param locality_name string fuzzy locality_name name to look for
#' @param method stringfuzzy method name to look for
#' @param subtype string subtype to select
#' @param verbose logical
#' @param extract_subplots_obs_features logical
#'
#'
#' @export
query_subplots <- function(ids_plots = NULL,
                           ids_subplots = NULL,
                           team_lead = NULL,
                           plot_name = NULL,
                           tag = NULL,
                           country = NULL,
                           province = NULL,
                           locality_name = NULL,
                           method = NULL,
                           subtype = NULL,
                           verbose = TRUE,
                           extract_subplots_obs_features = FALSE
) {
  
  if (is.null(ids_plots) & is.null(ids_subplots)) {
    
    queried_plots <-
      query_plots(
        team_lead = team_lead,
        plot_name = plot_name,
        tag = tag,
        country = country,
        province = province,
        locality_name = locality_name,
        method = method,
        remove_ids = FALSE,
        extract_subplot_features = FALSE,
      )
    
    ids_plots <- queried_plots$id_liste_plots
    
    sub_plot_data <-
      dplyr::tbl(mydb, "data_liste_sub_plots") %>%
      dplyr::filter(id_table_liste_plots %in% ids_plots) %>% 
      collect()
    
  } 
    
  if (!is.null(ids_plots) &
      is.null(ids_subplots))
    sql <- .sql_query_subplot_plot(id_plots = ids_plots, mydb_ = mydb)
  
  if (!is.null(ids_plots) & !is.null(ids_subplots))
    sql <- .sql_query_subplot_plot_2(id_plots = ids_plots,
                                     id_subplots = ids_subplots,
                                     mydb_ = mydb)
  
  if (is.null(ids_plots) & !is.null(ids_subplots))
    sql <- .sql_query_subplot(id_subplots = ids_subplots, mydb_ = mydb)
  
  sub_plot_data <-
    suppressMessages(func_try_fetch(con = mydb, sql = sql)
    )
    
  

  
  nbe_subplot_data <- nrow(dplyr::distinct(sub_plot_data,
                                           id_table_liste_plots))
  
  if (verbose) {
    cli::cli_alert_info("{length(ids_plots)} plots selected")
    cli::cli_alert_info("subplot_features found for {nbe_subplot_data} plots")}
  
  if (nbe_subplot_data > 0) {
    
    # all_sub_type <-
    #   sub_plot_data %>%
    #   dplyr::distinct(id_type_sub_plot) %>%
    #   dplyr::left_join(
    #     dplyr::tbl(mydb, "subplotype_list") %>%
    #       dplyr::select(type, valuetype, typedescription, id_subplotype),
    #     by = c("id_type_sub_plot" = "id_subplotype")
    #   )
    
    if (!is.null(subtype)) {
      sub_plot_data <-
        sub_plot_data %>%
        dplyr::filter(grepl(subtype, type))
      
      cli::cli_alert_info("Selected subplot features: {all_sub_type$type}")
      
      # sub_plot_data <-
      #   sub_plot_data %>%
      #   dplyr::filter(id_type_sub_plot %in% !!(all_sub_type %>%
      #                                            dplyr::pull(id_type_sub_plot)))
      
    }
    
    extracted_data <-
      sub_plot_data %>%
      # dplyr::left_join(all_sub_type,
      #                  by = c("id_type_sub_plot" = "id_type_sub_plot")) %>%
      # dplyr::collect() %>%
      dplyr::select(id_table_liste_plots,
                    year,
                    month,
                    day,
                    type,
                    valuetype,
                    typevalue,
                    typevalue_char,
                    original_subplot_name,
                    id_sub_plots,
                    comment)
    
    
    if (extract_subplots_obs_features) {
      
      feats <- query_subplot_observations_feat(id_sub_plots = extracted_data$id_sub_plots)
      
      if (any(!is.na(feats$all_feat_pivot))) {
        
        feats_unique <-
          feats$all_feat_pivot %>%
          mutate(id_subplot_feat = as.character(id_subplot_feat)) %>%
          group_by(id_sub_plots) %>%
          summarise(across(where(is.numeric), ~mean(., na.rm = T)),
                    across(where(is.character), ~paste(.[!is.na(.)], collapse = "|"))) %>%
          mutate(across(where(is.character), ~na_if(.x, "")))
        
        extracted_data <-
          extracted_data %>%
          dplyr::left_join(feats_unique,
                           by = c("id_sub_plots" = "id_sub_plots"))
      }
      
    }
    
    
    if (any(extracted_data$valuetype == "numeric")) {
      numeric_subplots_pivot <-
        extracted_data %>%
        filter(valuetype == "numeric",
               type != "census") %>%
        select(id_table_liste_plots, typevalue, type) %>%
        tidyr::pivot_wider(
          names_from = "type",
          values_from = "typevalue",
          values_fn = ~ mean(.x, na.rm = TRUE)
        )
    } else {
      numeric_subplots_pivot <- NULL
    }
    
    if (any(extracted_data$valuetype == "character")) {
      character_subplots_pivot <-
        extracted_data %>%
        filter(valuetype == "character",
               type != "census") %>%
        select(id_table_liste_plots, typevalue_char, type) %>%
        tidyr::pivot_wider(
          names_from = "type",
          values_from = "typevalue_char",
          values_fn = ~ paste(.x, collapse = "|")
        )
    } else {
      character_subplots_pivot <- NULL
    }
    
    if (any(extracted_data$type == "census")) {
      
      census_plots <-
        extracted_data %>%
        dplyr::filter(type == "census")
      
      ## max census for each plot
      census_plots_nbr <-
        census_plots %>%
        dplyr::group_by(id_table_liste_plots) %>%
        dplyr::summarise(number_of_census = max(typevalue, na.rm = T))
      
      census_features <-
        census_plots 
      # %>%
      #   # dplyr::left_join(dplyr::tbl(mydb, "table_colnam") %>%
      #   #                    filter(id_table_colnam %in% !!census_plots$id_colnam) %>%
      #   #                    collect(),
      #   #                  by = c("id_colnam" = "id_table_colnam")) %>%
      #   dplyr::select(
      #     year,
      #     month,
      #     day,
      #     typevalue,
      #     id_sub_plots,
      #     id_table_liste_plots
      #   )
      
      nbr_census <- dplyr::distinct(census_features, typevalue)
      census_dates_lists <- vector('list', nrow(nbr_census))
      for (i in 1:nrow(nbr_census)) {
        
        census_features_selected <-
          census_features %>%
          dplyr::filter(typevalue == i)
        
        census_features_selected <-
          census_features_selected %>%
          dplyr::mutate(date =
                          paste(ifelse(!is.na(month),
                                       month, 1), # if day is missing, by default 1
                                ifelse(!is.na(day),
                                       day, 1), # if month is missing, by default 1
                                ifelse(!is.na(year),
                                       year, ""),
                                sep = "/")) %>%
          dplyr::mutate(date_julian = date::as.date(date))
        # %>%
        #   dplyr::select(date, date_julian, id_table_liste_plots)
        
        date_name <- paste0("date_census_", i)
        date_name_enquo1 <-
          rlang::parse_expr(rlang::quo_name(rlang::enquo(date_name)))
        date_name <- paste0("date_census_julian_", i)
        date_name_enquo2 <-
          rlang::parse_expr(rlang::quo_name(rlang::enquo(date_name)))
        
        census_features_selected <-
          census_features_selected %>%
          dplyr::rename(!!date_name_enquo1 := date) %>%
          dplyr::rename(!!date_name_enquo2 := date_julian)
        
        census_dates_lists[[i]] <- 
          census_features_selected %>% 
          dplyr::select(id_table_liste_plots, 
                        starts_with("date_census_"))
        
      }
      
    } else {
      census_dates_lists <- NULL
      census_plots_nbr <- NULL
      census_features <- NA
    }
    
    if (any(grepl("table_", extracted_data$valuetype))) {
      
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
          select(id_table_liste_plots, typevalue_char, type) %>%
          tidyr::pivot_wider(
            names_from = "type",
            values_from = "typevalue_char",
            values_fn = ~ paste(., collapse = ", ")
          )
      }
    } else {
      table_valutype_list <- NULL
    }
    
    all_subplot_pivot <-
      c(census_dates_lists,
        table_valutype_list,
        list(census_plots_nbr),
        list(character_subplots_pivot),
        list(numeric_subplots_pivot))
    
    all_subplot_pivot <-
      purrr::reduce(all_subplot_pivot[!unlist(lapply(all_subplot_pivot, is.null))],
                    dplyr::full_join,
                    by = 'id_table_liste_plots')
    
    return(list(all_subplots = extracted_data,
                all_subplot_pivot = all_subplot_pivot,
                census_features = census_features))
    
  } else {
    
    return(list(all_subplots = NA,
                all_subplot_pivot = NA,
                census_features = NA))
    
  }
  
  
  
}




#' Add an observation in subplot_features table
#'
#' Add a trait measure in subplot_features table
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param plot_name_field string column name which contain the plot_name for linking
#' @param id_plot_name string id of plot_name
#' @param subplottype_field string vector listing trait columns names in new_data
#' @param add_data logical whether or not data should be added - by default FALSE
#' @param ask_before_update logical ask before adding
#' @param verbose logical
#' @param check_existing_data logical if it should be checked if imported data already exist in the database
#'
#' @export
add_subplot_features <- function(new_data,
                                 col_names_select = NULL,
                                 col_names_corresp= NULL,
                                 plot_name_field = NULL,
                                 id_plot_name = NULL,
                                 id_plot_name_corresp = "id_table_liste_plots_n",
                                 subplottype_field,
                                 features_field = NULL,
                                 add_data = FALSE,
                                 ask_before_update = TRUE,
                                 verbose = TRUE,
                                 check_existing_data = TRUE) {
  
  if(!exists("mydb")) call.mydb()
  
  for (i in 1:length(subplottype_field)) if(!any(colnames(new_data)==subplottype_field[i]))
    stop(paste("subplottype_field provide not found in new_data", subplottype_field[i]))
  
  
  if (!is.null(col_names_select) &
      !is.null(col_names_corresp)) {
    new_data_renamed <-
      .rename_data(dataset = new_data,
                   col_old = col_names_select,
                   col_new = col_names_corresp)
  } else {
    new_data_renamed <-
      new_data
  }
  
  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))
  
  if(is.null(plot_name_field) & is.null(id_plot_name)) stop("no plot links provided, provide either plot_name_field or id_plot_name")
  
  if (!any(col_names_corresp == "day")) {
    if (verbose) cli::cli_alert_warning("no information collection day provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(day = NA)
  }
  
  if (!any(col_names_corresp == "year")) {
    if (verbose)  cli::cli_alert_warning("no information collection year provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(year = NA)
  }
  
  if (!any(col_names_corresp == "month")) {
    if (verbose)  cli::cli_alert_warning("no information collection month provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(month = NA)
  }
  
  new_data_renamed <-
    new_data_renamed %>%
    mutate(id_new_data = 1:nrow(.))
  
  ### Linking collectors names
  # if(!is.null(collector_field)) {
  #   if(!any(colnames(new_data_renamed) == collector_field))
  #     stop("no collector_field found in new dataset")
  #   # new_data_renamed <-
  #   #   .link_colnam(data_stand = new_data_renamed, collector_field = collector_field)
  #   
  #   new_data_renamed <- .link_colnam(
  #     data_stand = new_data_renamed,
  #     column_searched = collector_field,
  #     column_name = "colnam",
  #     id_field = "id_colnam",
  #     id_table_name = "id_table_colnam",
  #     db_connection = mydb,
  #     table_name = "table_colnam"
  #   )
  #   
  # }else{
  #   if (verbose)  cli::cli_alert_warning("no information on collector provided")
  #   new_data_renamed <-
  #     new_data_renamed %>%
  #     tibble::add_column(id_colnam = NA)
  # }
  
  ### Linking plot names
  if (!is.null(plot_name_field)) {
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
  
  if(!is.null(id_plot_name)) {
    
    # if(id_plot_name == "id_table_liste_plots_n") id_plot_name <- "id_table_liste_plots_n"
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::all_of(dplyr::vars(id_plot_name)), ~ dplyr::all_of(id_plot_name_corresp))
    
    if(any(colnames(new_data_renamed) == "plot_name"))
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::select(-plot_name)
    
    if (id_plot_name_corresp == "id_table_liste_plots_n")
      link_plot <-
        new_data_renamed %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "data_liste_plots") %>%
            dplyr::select(plot_name, id_liste_plots) %>% dplyr::collect(),
          by = c("id_table_liste_plots_n" = "id_liste_plots")
        )
    
    
    if(id_plot_name_corresp == "id_old")
      link_plot <-
        new_data_renamed %>%
        dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                           dplyr::select(plot_name, id_old) %>% dplyr::collect(),
                         by=c("id_old" = "id_old"))
    
    if(dplyr::filter(link_plot, is.na(plot_name)) %>%
       nrow() > 0) {
      print(dplyr::filter(link_plot, is.na(plot_name)))
      if (verbose)  cli::cli_alert_warning("provided id plot not found in plot metadata")
    }
    
    if(id_plot_name_corresp == "id_table_liste_plots_n")
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename(id_liste_plots = id_table_liste_plots_n)
    
    if(id_plot_name_corresp == "id_old")
      new_data_renamed <-
        new_data_renamed %>%
        left_join(tbl(mydb, "data_liste_plots") %>%
                    dplyr::select(id_old, id_liste_plots) %>%
                    collect(),
                  c("id_old"="id_old"))
    
  }
  
  ### preparing dataset to add for each subplottype
  list_add_data <- vector('list', length(subplottype_field))
  for (i in 1:length(subplottype_field)) {
    
    subplottype <- subplottype_field[i]
    
    if (!any(colnames(new_data_renamed) == subplottype))
      stop(paste("subplottype field not found", subplottype))
    
    data_subplottype <-
      new_data_renamed
    
    ### adding subplot id and adding potential issues based on subplot
    data_subplottype <-
      .link_subplotype(data_stand = data_subplottype,
                       subplotype = subplottype)
    
    # subplottype_name <-
    #   "subplottype"
    # 
    # data_subplottype <-
    #   data_subplottype %>%
    #   dplyr::rename_with(.cols = dplyr::all_of(subplottype),
    #                      .fn = ~ subplottype_name)
    
    data_subplottype <-
      data_subplottype %>%
      dplyr::filter(!is.na(subplotype))
    

    
    print(".add_modif_field")
    data_subplottype <-
      .add_modif_field(dataset = data_subplottype)
    
    
    ## see what type of value numeric of character
    valuetype <-
      data_subplottype %>%
      dplyr::distinct(id_subplottype) %>%
      dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                         dplyr::select(valuetype, id_subplotype) %>%
                         dplyr::collect(),
                       by=c("id_subplottype"="id_subplotype"))
    
    print("data_to_add")
    data_to_add <-
      dplyr::tibble(id_table_liste_plots = data_subplottype$id_liste_plots,
                    # id_colnam = data_subplottype$id_colnam,
                    year = data_subplottype$year,
                    month = data_subplottype$month,
                    day = data_subplottype$day,
                    id_type_sub_plot = data_subplottype$id_subplottype,
                    # typevalue = data_subplottype$subplottype,
                    typevalue = ifelse(rep(any(valuetype$valuetype %in% c("numeric", "table_colnam")),
                                           nrow(data_subplottype)), data_subplottype$subplotype, NA),
                    typevalue_char = ifelse(rep(valuetype$valuetype == "character",
                                                nrow(data_subplottype)), data_subplottype$subplotype, NA),
                    original_subplot_name = ifelse(rep(any(colnames(data_subplottype)=="original_subplot_name"),
                                                       nrow(data_subplottype)), data_subplottype$original_subplot_name, NA),
                    issue = data_subplottype$issue,
                    comment = ifelse(rep(any(colnames(data_subplottype)=="comment"),
                                         nrow(data_subplottype)), data_subplottype$comment, NA),
                    date_modif_d = data_subplottype$date_modif_d,
                    date_modif_m = data_subplottype$date_modif_m,
                    date_modif_y = data_subplottype$date_modif_y)
    
    if(any(is.na(data_to_add$id_table_liste_plots))) {
      rm_na <- utils::askYesNo(msg = "Remove features not linked to plot?")
      
      if(rm_na) data_to_add <-
          data_to_add %>%
          filter(!is.na(id_table_liste_plots))
      
    }
    
    list_add_data[[i]] <-
      data_to_add
    
    if (check_existing_data) {
      ## check if new data already exist in database
      selected_new_data <-
        data_to_add %>%
        dplyr::select(id_table_liste_plots, id_type_sub_plot, typevalue) %>%
        dplyr::rename(typevalue_new = typevalue)
      
      all_existing_data <-
        dplyr::tbl(mydb, "data_liste_sub_plots") %>%
        dplyr::select(id_table_liste_plots, id_type_sub_plot, typevalue) %>%
        dplyr::collect() %>%
        dplyr::rename(typevalue_old = typevalue)
      
      crossing_data <-
        selected_new_data %>%
        dplyr::left_join(
          all_existing_data,
          by = c(
            "id_table_liste_plots" = "id_table_liste_plots",
            "id_type_sub_plot" = "id_type_sub_plot"
          )
        ) %>%
        filter(!is.na(typevalue_old))
      
      continue <- TRUE
      if (nrow(crossing_data) > 0) {
        cli::cli_alert_info("Data to be imported already exist in the database")
        print(crossing_data)
        continue <- utils::askYesNo(msg = "Continue importing?")
      }
      
    } else {
      continue <- TRUE
    }
    
    print(data_to_add)
    
    if(continue) {
      
      if (ask_before_update) {
        response <-
          utils::askYesNo("Confirm add these data to data_liste_sub_plots table?")
      } else {
        response <- TRUE
      }
    } else {
      response <- FALSE
    }
    
    if(add_data & response) {
      
      message(paste("adding data:", nrow(data_subplottype), "rows"))
      DBI::dbWriteTable(mydb, "data_liste_sub_plots",
                        data_to_add, append = TRUE, row.names = FALSE)
      
      cli::cli_alert_success("{nrow(data_to_add)} line imported in data_liste_sub_plots")
      
      
      
      
      if (!is.null(features_field)) {
        
        imported_data <- tbl(mydb, "data_liste_sub_plots") %>%
          filter(date_modif_d == !!data_to_add$date_modif_d[1],
                 date_modif_m == !!data_to_add$date_modif_m[1],
                 date_modif_y == !!data_to_add$date_modif_y[1]) %>%
          select(id_sub_plots, id_table_liste_plots) %>%
          collect() %>%
          arrange(id_sub_plots)
        
        ids <- imported_data %>% slice((nrow(imported_data)-nrow(data_to_add)+1):nrow(imported_data))
        
        data_feats <-
          data_subplottype %>% 
          select(all_of(features_field)) %>%
          mutate(id_sub_plots = ids$id_sub_plots,
                 id_table_liste_plots = ids$id_table_liste_plots)
        
        add_subplot_observations_feat(
          new_data = data_feats,
          id_sub_plots = "id_sub_plots",
          features = features_field , #
          add_data = T
        )
        
      }
      
    } else {
      
      cli::cli_alert_danger("Data not imported because add_data if FALSE")
      
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
  
  
  return(list_add_data)
  
}




#' Add a type in subplot table
#'
#' Add feature and associated descriptors in subplot list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_type string value with new type descritors - try to avoid space
#' @param new_valuetype string one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character'
#' @param new_maxallowedvalue numeric if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue numeric if valuetype is numeric, indicate the minimum allowed value
#' @param new_typedescription string full description of trait
#' @param new_factorlevels string a vector of all possible value if valuetype is categorical or ordinal
#' @param new_expectedunit string expected unit (unitless if none)
#' @param new_comments string any comments
#'
#' @export
add_subplottype <- function(new_type = NULL,
                            new_valuetype = NULL,
                            new_maxallowedvalue = NULL,
                            new_minallowedvalue = NULL,
                            new_typedescription = NULL,
                            new_factorlevels = NULL,
                            new_expectedunit = NULL,
                            new_comments = NULL) {
  
  if(is.null(new_type)) stop("define new type")
  
  if(dplyr::tbl(mydb, "subplotype_list") %>%
     dplyr::distinct(type) %>%
     dplyr::filter(type == !!new_type) %>%
     dplyr::collect() %>%
     nrow()>0)  stop("new type already in table")
  
  
  if (is.null(new_valuetype)) stop("define new_valuetype")
  
  if (!any(new_valuetype==c('numeric',
                            'integer',
                            'categorical',
                            'ordinal',
                            'logical',
                            'character',
                            'table_colnam'))) stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character' or 'table_colnam'")
  
  if (new_valuetype=="numeric" | new_valuetype=="integer")
    if (!is.numeric(new_maxallowedvalue) & !is.integer(new_maxallowedvalue)) stop("valuetype numeric of integer and max value not of this type")
  if (new_valuetype=="numeric" | new_valuetype=="integer")
    if (!is.numeric(new_minallowedvalue) & !is.integer(new_minallowedvalue)) stop("valuetype numeric of integer and min value not of this type")
  
  if(!exists("mydb")) call.mydb()
  
  new_data_renamed <- tibble(type = new_type,
                             valuetype = new_valuetype,
                             maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
                             minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
                             typedescription = ifelse(is.null(new_typedescription), NA, new_typedescription),
                             factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
                             expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
                             comments = ifelse(is.null(new_comments), NA, new_comments))
  
  print(new_data_renamed)
  
  Q <- utils::askYesNo("confirm adding this type?")
  
  if(Q)
    DBI::dbWriteTable(mydb, "subplotype_list", new_data_renamed, append = TRUE, row.names = FALSE)
  
  
}







add_subplot_observations_feat <- function(new_data,
                                          id_sub_plots = "id_sub_plots",
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
    rename(id_sub_plots := all_of(id_sub_plots))
  
  link_subplots_measures <-
    new_data_renamed %>%
    dplyr::left_join(
      try_open_postgres_table(table = "data_liste_sub_plots", con = mydb) %>%
        dplyr::select(id_sub_plots) %>%
        dplyr::filter(id_sub_plots %in% !!unique(new_data_renamed$id_sub_plots)) %>%
        dplyr::collect() %>%
        dplyr::mutate(rrr = 1),
      by = c("id_sub_plots" = "id_sub_plots")
    )
  
  if (dplyr::filter(link_subplots_measures, is.na(rrr)) %>%
      nrow() > 0) {
    print(dplyr::filter(link_subplots_measures, is.na(rrr)))
    stop("provided subplots not found in data_liste_sub_plots")
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
        .link_subplotype(data_stand = data_feat, subplotype = feat)
      
      ## see what type of value numeric of character
      valuetype <-
        data_feat %>%
        dplyr::distinct(id_subplottype) %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "subplotype_list") %>%
            dplyr::select(valuetype, id_subplotype) %>%
            dplyr::collect(),
          by = c("id_subplottype" = "id_subplotype")
        )
      
      if(valuetype$valuetype == "table_colnam") {
        
        add_col_sep <-
          data_feat %>%
          tidyr::separate_rows(subplotype, sep = ",") %>%
          mutate(subplotype = stringr::str_squish(subplotype))
        
        add_col_sep <- .link_colnam(
          data_stand = add_col_sep,
          column_searched = "subplotype",
          column_name = "colnam",
          id_field = "subplotype",
          id_table_name = "id_table_colnam",
          db_connection = mydb,
          table_name = "table_colnam"
        )
        
        data_feat <- add_col_sep
        
      }
      
      if (any(data_feat$subplotype == 0)) {

        add_0 <-
          utils::askYesNo("Some value are equal to 0. Do you want to add these values anyway ??")

        if(!add_0)
          data_feat <-
            data_feat %>%
            dplyr::filter(subplotype != 0)

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
          id_sub_plots = data_feat$id_sub_plots,
          id_type_sub_plot = data_feat$id_subplottype,
          typevalue = ifelse(
            rep(val_type == "numeric", nrow(data_feat)),
            data_feat$subplotype,
            NA
          ),
          typevalue_char = ifelse(
            rep(val_type == "character", nrow(data_feat)),
            as.character(data_feat$subplotype),
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
        utils::askYesNo("Confirm add these data to data_subplot_feat table?")
      
      if(add_data & response) {
        
        DBI::dbWriteTable(mydb, "data_subplot_feat",
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
query_subplot_observations_feat <- function(id_sub_plots  = NULL,
                                            pivot_table = TRUE) {
  
  feat_data <-
    try_open_postgres_table(table = "data_subplot_feat", con = mydb) %>%
    dplyr::filter(id_sub_plots %in% !!id_sub_plots)
  
  nbe_feat_data <- nrow(feat_data %>%
                          dplyr::collect())
  
  if (nbe_feat_data  > 0) {
    
    all_sub_type <-
      feat_data %>%
      dplyr::distinct(id_type_sub_plot) %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "subplotype_list") %>%
          dplyr::select(type, valuetype, typedescription, id_subplotype),
        by = c("id_type_sub_plot" = "id_subplotype")
      )
    
    extracted_data <-
      feat_data %>%
      dplyr::left_join(all_sub_type,
                       by = c("id_type_sub_plot" = "id_type_sub_plot")) %>%
      dplyr::collect() %>%
      dplyr::select(id_sub_plots,
                    type,
                    valuetype,
                    typevalue,
                    typevalue_char,
                    id_subplot_feat)
    
    # extracted_data <- extracted_data %>%
    #   group_by(id_trait_measures, trait) %>%
    #   summarise(valuetype = first(valuetype),
    #             typevalue = mean(typevalue, na.rm = T),
    #             n = n()) %>%
    #   filter(n > 1)
    
    if (pivot_table) {
      
      if (any(extracted_data$valuetype == "numeric")) {
        
        numeric_subplots_pivot <-
          extracted_data %>%
          filter(valuetype == "numeric") %>%
          select(id_sub_plots, typevalue, type, id_subplot_feat) %>%
          tidyr::pivot_wider(
            names_from = "type",
            values_from = "typevalue",
            values_fn = ~ mean(.x, na.rm = TRUE)
          ) %>%
          mutate(id_subplot_feat = as.character(id_subplot_feat))
        
      } else {
        
        numeric_subplots_pivot <- NULL
        
      }
      
      if (any(extracted_data$valuetype == "character")) {
        
        character_feat_pivot <-
          extracted_data %>%
          filter(valuetype == "character") %>%
          select(id_sub_plots, typevalue_char, type, id_subplot_feat)  %>%
          tidyr::pivot_wider(
            names_from = "type",
            values_from = "typevalue_char",
            values_fn = ~ paste(.x, collapse = "|")
          ) %>%
          mutate(id_subplot_feat = as.character(id_subplot_feat))
        
      } else {
        character_feat_pivot <- NULL
      }
      
      if (any(extracted_data$valuetype == "ordinal")) {
        ordinal_subplots_pivot <-
          extracted_data %>%
          filter(valuetype == "ordinal") %>%
          select(id_sub_plots, typevalue_char, type, id_subplot_feat)  %>%
          tidyr::pivot_wider(
            names_from = "type",
            values_from = "typevalue_char",
            values_fn = ~ paste(.x, collapse = "|")
          ) %>%
          mutate(id_subplot_feat = as.character(id_subplot_feat))
      } else {
        ordinal_subplots_pivot <- NULL
      }
      
      
      if (any(grepl("table_colnam", extracted_data$valuetype))) {
        
        table_ids_subplots <- extracted_data %>%
          filter(grepl("table_", valuetype))
        
        allvalutype <- distinct(table_ids_subplots, type)
        
        table_valutype_list <- vector('list', nrow(allvalutype))
        for (i in 1:nrow(allvalutype)) {
          
          table_ids_subplots_filt <- 
            table_ids_subplots %>% 
            filter(type == allvalutype$type[i])
          
          ids_ <-
            case_when(
              table_ids_subplots_filt$valuetype[i] == "table_colnam" ~ "id_table_colnam"
            )
          
          col_to_keep_ <-
            case_when(
              table_ids_subplots_filt$valuetype[i] == "table_colnam" ~ "colnam"
            )
          
          table_collected <-
            tbl(mydb, table_ids_subplots_filt$valuetype[i]) %>%
            collect()
          
          table_ids_subplots_filt <-
            table_ids_subplots_filt %>%
            left_join(table_collected %>%
                        dplyr::select(all_of(c(col_to_keep_, ids_))),
                      by = c("typevalue" = ids_)) %>%
            mutate(typevalue_char = !!rlang::parse_expr(col_to_keep_)) %>%
            dplyr::select(-all_of(col_to_keep_))
          
          table_ids_subplots_piv <- 
            table_ids_subplots_filt %>%
            select(id_sub_plots, typevalue_char, type, id_subplot_feat) %>%
            mutate(id_subplot_feat = as.character(id_subplot_feat)) %>%
            tidyr::pivot_wider(
              names_from = "type",
              values_from = c("typevalue_char", "id_subplot_feat"),
              values_fn = ~ paste(., collapse = "|")
            )
          
          names(table_ids_subplots_piv) <- 
            gsub("typevalue_char_", "", names(table_ids_subplots_piv))
          
          names(table_ids_subplots_piv)[which(grepl("id_subplot_feat", names(table_ids_subplots_piv)))] <- 
            "id_subplot_feat"
          
          table_valutype_list[[i]] <-
            table_ids_subplots_piv
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
      
    }else {
      
      all_feat_pivot <- extracted_data
    }
    
    
    
    # all_feat_pivot <-
    #   purrr::reduce(all_feat_pivot[!unlist(lapply(all_feat_pivot, is.null))],
    #                 dplyr::full_join,
    #                 by = c('id_trait_measures'))
    
    
  } else {
    
    all_feat_pivot <- NA
    
  }
  
  return(list(all_feat_pivot = all_feat_pivot))
  
}
