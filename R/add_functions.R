



# =============================================================================
# Add meta data as well as subplots features linked to metadata
# =============================================================================


#' Add new plot metadata
#'
#' Add metadata for new plots
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string a vector of string indicating columns names of new_data
#' @param col_names_corresp string a vector of string indicating to which columns selected columns of new_data corresponds
#'
#' @importFrom methods new
#' @importFrom stats dist sd
#' @importFrom kableExtra cell_spec kable_styling
#'
#' @return No return value, new plots are added
#' @export
add_plots <- function(new_data,
                      col_names_select,
                      col_names_corresp) {
  
  mydb <- call.mydb()
  
  new_data_renamed <-
    new_data
  
  for (i in 1:length(col_names_select)) {
    if(any(colnames(new_data_renamed)==col_names_select[i])){
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    }else{
      stop(paste("Column name provided not found in provided new dataset", col_names_corresp[i]))
      
    }
  }
  
  ## Checking dates
  if (any(colnames(new_data_renamed) == "date_y"))
    if (any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] > lubridate::year(Sys.Date())) |
        any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] < 1900))
      stop("ERREUR dans date_y, year provided impossible")
  if (any(colnames(new_data_renamed) == "date_m"))
    if (any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] > 12) |
        any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] < 1))
      stop("ERREUR dans date_m, month provided impossible")
  if (any(colnames(new_data_renamed) == "data_d"))
    if (any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] > 31) |
        any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] < 1))
      stop("ERREUR dans data_d, day provided impossible")
  if (any(colnames(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) |
        any(new_data_renamed$ddlon < -180))
      stop("ERREUR dans ddlon, longitude provided impossible")
  if (any(colnames(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) |
        any(new_data_renamed$ddlon < -90))
      stop("ERREUR dans ddlat, latitude provided impossible")
  
  ## Checking if names plot are already in the database
  if(any(colnames(new_data_renamed) == "plot_name")) {
    
    found_plot <-
      try_open_postgres_table(table = "data_liste_plots", con = mydb) %>%
      dplyr::filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      dplyr::collect()
    
    if (nrow(found_plot) > 0) {
      print(found_plot)
      stop("Some plot_name in new data already in the plot list table. No duplicate allowed.")
    }
  }
  
  ## Checking method
  if(!any(names(new_data_renamed) == "method")) {
    
    stop("missing method information")
    
  } else {
    
    new_data_renamed <-
      .link_table(
        data_stand = new_data_renamed,
        column_searched = "method",
        column_name = "method",
        id_field = "id_method",
        id_table_name = "id_method",
        db_connection = mydb,
        table_name = "methodslist"
      )
    
    # new_data_renamed <-
    #   new_data_renamed %>%
    #  dplyr::select(-method)
    
    col_names_corresp[which(col_names_corresp == "method")] <-
      "id_method"
    
  }
  
  ## Checking country
  if(!any(names(new_data_renamed) == "country")) {
    
    stop("missing country information")
    
  } else {
    
    new_data_renamed <-
      .link_table(
        data_stand = new_data_renamed,
        column_searched = "country",
        column_name = "country",
        id_field = "id_country",
        id_table_name = "id_country",
        db_connection = mydb,
        table_name = "table_countries"
      )
    
    
    col_names_corresp[which(col_names_corresp == "country")] <-
      "id_country"
    
  }
  
  ## Checking team_leader
  if(!any(names(new_data_renamed) == "team_leader")) {
    
    cli::cli_alert_danger("missing team_leader column")
    
    chose_pi <- choose_prompt(message = "Choose one team_leader for all plot ?")
    
    if (chose_pi) {
      
      id_team_leader <- .link_colnam(
        data_stand = tibble(team_leader = " "),
        column_searched = "team_leader",
        column_name = "colnam",
        id_field = "id_team_leader",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
      
      id_team_leader <-
        tibble(plot_name = new_data_renamed$plot_name,
               team_leader = id_team_leader$id_team_leader)
      
    }
    
  } else {
    
    cli::cli_alert_info("Identifying team_leader")
    
    team_leader_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, team_leader) %>%
      tidyr::separate_rows(team_leader, sep = ",") %>%
      mutate(team_leader = stringr::str_squish(team_leader))
    
    id_team_leader <-
      .link_colnam(
        data_stand = team_leader_sep,
        column_searched = "team_leader",
        column_name = "colnam",
        id_field = "team_leader",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
    
    # col_names_corresp[which(col_names_corresp == "team_leader")] <-
    #   "id_colnam"
    
  }
  
  ## Checking team_leader
  if(!any(names(new_data_renamed) == "PI")) {
    
    cli::cli_alert_danger("missing PI column")
    
    chose_pi <- choose_prompt(message = "Choose one PI for all plot ?")
    
    if (chose_pi) {
      # id_pi <- .link_colnam(data_stand = tibble(PI = " "),
      #                       collector_field = "PI", id_colnam = "id_pi")
      
      
      id_pi <-
        .link_colnam(
          data_stand = tibble(PI = " "),
          column_searched = "PI",
          column_name = "colnam",
          id_field = "id_pi",
          id_table_name = "id_table_colnam",
          db_connection = mydb,
          table_name = "table_colnam"
        )
      
      id_pi <-
        tibble(plot_name = new_data_renamed$plot_name,
               PI = id_pi$id_pi)
      
    }
    
  } else {
    
    cli::cli_alert_info("Identifying PI")
    
    pi_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, PI) %>%
      tidyr::separate_rows(PI, sep = ",") %>%
      mutate(PI = stringr::str_squish(PI))
    
    # id_pi <-
    #   .link_colnam(data_stand = pi_sep,
    #                collector_field = "PI", id_colnam = "PI")
    
    id_pi <-
      .link_colnam(
        data_stand = pi_sep,
        column_searched = "PI",
        column_name = "colnam",
        id_field = "PI",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
    
  }
  
  
  ## Checking data manager
  if(!any(names(new_data_renamed) == "data_manager")) {
    
    cli::cli_alert_danger("missing data_manager column")
    
    chose_data_manager <- choose_prompt(message = "Choose one data_manager for all plot ?")
    
    if (chose_data_manager) {
      # data_manager <- .link_colnam(data_stand = tibble(data_manager = " "),
      #                       collector_field = "data_manager",
      #                       id_colnam = "id_data_manager")
      
      data_manager <- .link_colnam(
        data_stand = tibble(data_manager = " "),
        column_searched = "data_manager",
        column_name = "colnam",
        id_field = "id_data_manager",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
      
      id_data_manager <-
        tibble(plot_name = new_data_renamed$plot_name,
               data_manager = data_manager$id_data_manager)
      
    }
    
  } else {
    
    cli::cli_alert_info("Identifying data manager")
    
    data_manager_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, data_manager) %>%
      tidyr::separate_rows(data_manager, sep = ",") %>%
      mutate(data_manager = stringr::str_squish(data_manager))
    
    # data_manager_sep <-
    #   .link_colnam(data_stand = data_manager_sep,
    #                collector_field = "data_manager", id_colnam = "data_manager")
    
    
    data_manager_sep <-
      .link_colnam(
        data_stand = data_manager_sep,
        column_searched = "data_manager",
        column_name = "colnam",
        id_field = "data_manager",
        id_table_name = "id_table_colnam",
        db_connection = mydb,
        table_name = "table_colnam"
      )
    
  }
  
  if(!any(names(new_data_renamed) == "additional_people")) {
    
    cli::cli_alert_danger("missing additional_people information")
    
  } else {
    
    cli::cli_alert_info("Identifying additional people list for the plot/transect")
    
    add_col_sep <-
      new_data_renamed %>%
      dplyr::select(plot_name, additional_people) %>%
      tidyr::separate_rows(additional_people, sep = ",") %>%
      mutate(additional_people = stringr::str_squish(additional_people))
    
    # add_col_sep <-
    #   .link_colnam(data_stand = add_col_sep,
    #                collector_field = "additional_people", id_colnam = "additional_people")
    
    
    add_col_sep <- .link_colnam(
      data_stand = add_col_sep,
      column_searched = "additional_people",
      column_name = "colnam",
      id_field = "additional_people",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )
    
    
  }
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-any_of(c("team_leader", "PI", "additional_people", "data_manager")))
  
  col_names_corresp <-
    col_names_corresp[which(!col_names_corresp %in% c("team_leader", "PI", "additional_people", "data_manager"))]
  
  ## Checking coordinates
  if (any(names(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) | any(new_data_renamed$ddlat < -90)) stop("ddlat impossible")
  
  if (any(names(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) | any(new_data_renamed$ddlon < -180)) stop("ddlon impossible")
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))
  
  new_data_renamed <-
    new_data_renamed %>%
    mutate(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )
  
  add <- choose_prompt(message = "Add these data to the table of plot data?")
  
  
  
  if(add) {
    DBI::dbWriteTable(mydb, "data_liste_plots", new_data_renamed, append = TRUE, row.names = FALSE)
    cli::cli_alert_success("{nrow(new_data_renamed)} plot imported in data_liste_plots")
    
    ids_list_plot <-
      try_open_postgres_table(table = "data_liste_plots", con = mydb) %>%
      filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      collect() %>%
      dplyr::select(id_liste_plots, plot_name)
    
    if (exists("id_team_leader")) {
      
      id_team_leader <-
        id_team_leader %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))
      
      add_subplot_features(new_data = id_team_leader,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("team_leader"),
                           add_data = T,
                           ask_before_update = F)
      
    }
    
    if (exists("id_pi")) {
      
      id_pi <-
        id_pi %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name")) %>%
        rename(principal_investigator = PI)
      
      add_subplot_features(new_data = id_pi,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("principal_investigator"),
                           add_data = T,
                           ask_before_update = F)
      
    }
    
    if (exists("add_col_sep")) {
      
      add_col_sep <-
        add_col_sep %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))
      
      add_subplot_features(new_data = add_col_sep,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("additional_people"),
                           add_data = T,
                           ask_before_update = F)
      
    }
    
    if (exists("data_manager_sep")) {
      
      data_manager_sep <-
        data_manager_sep %>%
        left_join(ids_list_plot, by = c("plot_name" = "plot_name"))
      
      add_subplot_features(new_data = data_manager_sep,
                           id_plot_name = "id_liste_plots",
                           subplottype_field = c("data_manager"),
                           add_data = T,
                           ask_before_update = F)
      
    }
    
  }
  
  if(!add)
    message("no data added")
  
  return(new_data_renamed)
  
}



#' Add an observation in subplot_features table
#'
#' Add a observation in subplot_features table
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
#' @param interactive logical whether to use interactive prompts (default TRUE)
#' @param verbose logical
#' @param check_existing_data logical if it should be checked if imported data already exist in the database
#' @param con database connection (optional, will create if NULL)
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
                                 interactive = TRUE,
                                 verbose = TRUE,
                                 check_existing_data = TRUE,
                                 con = NULL) {

  # Use provided connection or create new one
  if (is.null(con)) {
    mydb <- call.mydb()
  } else {
    mydb <- con
  }
  
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
  
  
  ## Linking plot names
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

    # Check if plot_name already exists in the data
    has_plot_name <- any(colnames(new_data_renamed) == "plot_name")

    # Only look up plot_name from database if it's not already present
    if (!has_plot_name) {
      if (id_plot_name_corresp == "id_table_liste_plots_n") {
        link_plot <-
          new_data_renamed %>%
          dplyr::left_join(
            dplyr::tbl(mydb, "data_liste_plots") %>%
              dplyr::select(plot_name, id_liste_plots) %>% dplyr::collect(),
            by = c("id_table_liste_plots_n" = "id_liste_plots")
          )

        # Update new_data_renamed with the joined data (including plot_name)
        new_data_renamed <- link_plot
      }

      if (id_plot_name_corresp == "id_old") {
        link_plot <-
          new_data_renamed %>%
          dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                             dplyr::select(plot_name, id_old) %>% dplyr::collect(),
                           by=c("id_old" = "id_old"))

        # Update new_data_renamed with the joined data (including plot_name)
        new_data_renamed <- link_plot
      }

      # Check if lookup failed
      if(dplyr::filter(new_data_renamed, is.na(plot_name)) %>%
         nrow() > 0) {
        print(dplyr::filter(new_data_renamed, is.na(plot_name)))
        if (verbose)  cli::cli_alert_warning("provided id plot not found in plot metadata")
      }
    } else {
      if (verbose) cli::cli_alert_info("plot_name already present in data, skipping database lookup")
    }

    if(id_plot_name_corresp == "id_table_liste_plots_n")
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename(id_liste_plots = id_table_liste_plots_n)
    
    if(id_plot_name_corresp == "id_old")
      new_data_renamed <-
        new_data_renamed %>%
        left_join(tbl(mydb, "data_liste_plots") %>%
                    dplyr::select(all_of(c(id_old, id_liste_plots))) %>%
                    collect(),
                  c("id_old"="id_old"))
    
  }
  
  ## preparing dataset to add for each subplottype
  list_add_data <- vector('list', length(subplottype_field))
  for (i in 1:length(subplottype_field)) {
    
    subplottype <- subplottype_field[i]
    
    if (!any(colnames(new_data_renamed) == subplottype))
      stop(paste("subplottype field not found", subplottype))
    
    data_subplottype <-
      new_data_renamed
    
    ## adding subplot id and adding potential issues based on subplot
    data_subplottype <-
      .link_subplotype(data_stand = data_subplottype,
                       subplotype = subplottype,
                       con = mydb)
    
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
    
    
    # see what type of value numeric of character
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
      # In interactive mode, ask user what to do
      # In non-interactive mode, automatically remove NA records
      if (interactive) {
        rm_na <- choose_prompt(message = "Remove features not linked to plot ?")
      } else {
        rm_na <- TRUE  # Auto-remove in non-interactive mode
        if (verbose) cli::cli_alert_warning("Removing {sum(is.na(data_to_add$id_table_liste_plots))} features not linked to plots (non-interactive mode)")
      }

      if(rm_na) data_to_add <-
          data_to_add %>%
          filter(!is.na(id_table_liste_plots))

    }
    
    list_add_data[[i]] <-
      data_to_add
    
    if (check_existing_data) {
      # check if new data already exist in database
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
        if (interactive) {
          continue <- choose_prompt(message = "Continue importing ?")
        } else {
          continue <- FALSE  # Don't import duplicates in non-interactive mode
          if (verbose) cli::cli_alert_warning("Skipping {nrow(crossing_data)} duplicate records (non-interactive mode)")
        }

      }
      
    } else {
      continue <- TRUE
    }
    
    print(data_to_add)
    
    if(continue) {

      if (ask_before_update && interactive) {
        response <-
          choose_prompt(message = "Confirm add these data to data_liste_sub_plots table?")
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
          features = features_field,
          add_data = TRUE,
          interactive = interactive  # Pass through interactive parameter
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



#' Add subplot observations features
#'
#' @description
#' A short description...
#' 
#' @param new_data A data frame containing the new observations to add.
#' @param id_sub_plots A single string specifying the column name for subplot IDs. Optional.
#' @param features A character vector of feature names to process.
#' @param allow_multiple_value A single logical value indicating whether multiple values are allowed. Optional.
#' @param add_data A single logical value indicating whether to actually add data to the database. Optional.
#'
#' @returns 
#' A list containing `list_features_add`, which is a list of data frames
#' for each processed feature. The function may error if features are not
#' found in the data, if no valid values exist, or if subplot IDs are not
#' found in the database.
#'
#' @export
add_subplot_observations_feat <- function(new_data,
                                          id_sub_plots = "id_sub_plots",
                                          features,
                                          allow_multiple_value = FALSE,
                                          add_data = FALSE,
                                          interactive = TRUE) {
  
  for (i in 1:length(features))
    if (!any(colnames(new_data) == features[i]))
      stop(paste("features field provide not found in new_data", features[i]))
  
  new_data_renamed <- new_data
  
  # removing entries with NA values for traits
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
  
  
  ## preparing dataset to add for each trait
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
      ## adding trait id and adding potential issues based on trait
      data_feat <-
        .link_subplotype(data_stand = data_feat, subplotype = feat, con = mydb)
      
      # see what type of value numeric of character
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

        if (interactive) {
          add_0 <- choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
        } else {
          add_0 <- FALSE  # Auto-remove zeros in non-interactive mode
          cli::cli_alert_warning("Removing {sum(data_feat$subplotype == 0)} zero values (non-interactive mode)")
        }

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
        
        if (interactive) {
          cf_merge <- choose_prompt(message = "confirm merging duplicates ?")
        } else {
          cf_merge <- TRUE  # Auto-merge duplicates in non-interactive mode
          cli::cli_alert_info("Auto-merging duplicates (non-interactive mode)")
        }

        if (cf_merge) {
          
          # issues_dup <- data_to_add %>%
          #   filter(id_trait_measures %in% data_to_add[duplicates_lg, "id_trait_measures"]) %>%
          #   dplyr::select(issue, id_trait_measures)
          
          # resetting issue
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

      if (interactive) {
        response <- choose_prompt(message = "Confirm add these data to data_subplot_feat table?")
      } else {
        response <- TRUE  # Auto-confirm in non-interactive mode
      }

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



# =============================================================================
# Add individuals or stems linked to plot data as well as individuals/stems features linked to individuals/stems
# =============================================================================



#' Add new individuals data
#'
#' Add new individuals data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be import
#' @param col_names_select string
#' @param col_names_corresp string
#' @param id_col integer indicate which name of col_names_select is the id for matching plot in metadata
#' @param launch_adding_data logical FALSE whether adding should be done or not
#' @param features_field vector string of field names in new_data containing the features associated with individual or stem data
#'
#'
#' @return No return value individuals updated
#' @export
add_individuals <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col,
                            features_field = NULL,
                            launch_adding_data = FALSE) {
  
  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )
  
  mydb <- call.mydb()
  mydb_taxa <- call.mydb.taxa()
  
  if(length(col_names_select) != length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")
  
  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))
  
  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename_at(dplyr::vars(col_names_select[id_col]), ~ col_names_corresp[id_col])
  
  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))
  
  new_data_renamed <-
    .link_table(data_stand = new_data_renamed,
                column_searched = "plot_name",
                column_name = "plot_name",
                id_field = "id_liste_plots",
                id_table_name = "id_liste_plots",
                db_connection = mydb,
                table_name = "data_liste_plots",
                keep_columns = "plot_name")
  
  
  ids_plot <-
    new_data_renamed %>%
    dplyr::select(plot_name, id_liste_plots) %>%
    dplyr::distinct(plot_name, id_liste_plots)
  
  if(any(is.na(ids_plot$id_liste_plots))) {
    warning("some plot are not found in metadata")
    print(ids_plot %>%
            dplyr::filter(is.na(id_liste_plots)))
    ids_plot <-
      ids_plot %>%
      dplyr::filter(is.na(id_liste_plots))
    
    logs <-
      dplyr::bind_rows(logs,
                       dplyr::tibble(
                         column = "id_liste_plots",
                         note = paste(nrow(ids_plot %>%
                                             dplyr::filter(is.na(id_liste_plots))),
                                      "some plot are not found in metadata")
                       ))
  }
  
  plots_already_in_db <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(id_table_liste_plots_n %in% !!ids_plot$id_liste_plots) %>%
    dplyr::distinct(id_table_liste_plots_n) %>%
    dplyr::collect()
  
  if (nrow(plots_already_in_db) > 0) {
    print(
      plots_already_in_db %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "data_liste_plots") %>%
            dplyr::select(plot_name, id_liste_plots) %>%
            dplyr::collect(),
          by = c("id_table_liste_plots_n" = "id_liste_plots")
        ) %>%
        dplyr::pull(plot_name)
    )
    warning("data for some plots already in database")
    
  }
  
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::left_join(ids_plot) %>%
    dplyr::rename(id_table_liste_plots_n = id_liste_plots)
  
  
  col_names_select <-
    col_names_select[-id_col]
  col_names_corresp <-
    col_names_corresp[-id_col]
  
  
  
  col_names_corresp <-
    c(col_names_corresp, "id_table_liste_plots_n")
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))
  
  ### CHECKS
  method <-
    ids_plot %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "data_liste_plots") %>%
        dplyr::select(plot_name, id_liste_plots, id_method) %>%
        dplyr::left_join(dplyr::tbl(mydb, "methodslist")) %>%
        dplyr::collect(),
      by = c("id_liste_plots" = "id_liste_plots")
    ) %>%
    dplyr::distinct(method)
  
  if (nrow(method) > 1) {
    print(method)
    stop("More than one method selected, import plot of one method at a time")
  }
  
  if (!any(colnames(new_data_renamed) == "idtax_n"))
    stop("idtax_n column missing")
  
  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))
  
  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))
  
  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(
      try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
        # dplyr::tbl(mydb_taxa, "table_taxa") %>%
        dplyr::select(idtax_n, id_tax_famclass) %>%
        filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
        dplyr::collect(),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    dplyr::filter(is.na(id_tax_famclass)) %>%
    dplyr::pull(idtax_n)
  
  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in diconame", unmatch_id_diconame))
  
  if(any(is.na(names(new_data_renamed) == "dbh"))) {
    
    message("\n dbh and others traits measure should be added independantly using add_traits_measures function")
    
  }
  
  ## checking column given method
  if(dplyr::pull(method) == "Large") {
    
    # if (!any(colnames(new_data_renamed) == "tra"))
    #   stop("sous_plot_name column missing")
    if (!any(colnames(new_data_renamed) == "tag"))
      stop("tag column missing")
    
    
  }
  
  if (dplyr::pull(method) == "1ha-IRD" | dplyr::pull(method) == " ") {
    if (!any(colnames(new_data_renamed) == "tag"))
      stop("tag column missing - Tag individual")
    
    
    ### checking duplicated tags within plots
    duplicated_tags <-
      new_data_renamed %>%
      group_by(id_table_liste_plots_n, tag) %>%
      count() %>%
      filter(n > 1)
    
    duplicated_tags <-
      new_data_renamed %>%
      dplyr::left_join(
        duplicated_tags ,
        by = c(
          "id_table_liste_plots_n" = "id_table_liste_plots_n",
          "tag" = "tag"
        )
      ) %>%
      dplyr::filter(!is.na(n)) %>%
      dplyr::left_join(
        tbl(mydb, "data_liste_plots") %>%
          dplyr::select(id_liste_plots, plot_name) %>%
          dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )
    
    if(nrow(duplicated_tags) > 0) {
      warning("\n Duplicated tags in some plots")
      print(duplicated_tags)
      
      readr::write_excel_csv(duplicated_tags, "duplicated_tags.csv")
    }
    
    if(any(names(new_data_renamed) == "multi_tiges_id")) {
      cli::cli_alert_info("Checking multi tiges")
      
      
      
    }
    
  }
  
  ## checking tag
  
  if(!is.numeric(new_data_renamed$tag)) {
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(tag = as.numeric(tag))
    
    if(any(is.na(new_data_renamed$tag)))
      new_data_renamed %>%
      filter(is.na(tag)) %>%
      print()
    stop("tag missing after converting to numeric")
  }
  
  # check herbarium specimen coherence
  
  if (!any(colnames(new_data_renamed) == "herbarium_nbe_type"))
    cli::cli_alert_danger("herbarium_nbe_type column missing")
  if (!any(colnames(new_data_renamed) == "herbarium_nbe_char"))
    cli::cli_alert_danger("herbarium_nbe_char column missing")
  
  if (any(colnames(new_data_renamed) == "herbarium_nbe_char")) {
    all_herb_ref <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char) %>%
      dplyr::filter(!is.na(herbarium_nbe_char))
  }
  
  
  if (any(colnames(new_data_renamed) == "herbarium_nbe_type")) {
    
    all_herb_type <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_type) %>%
      dplyr::filter(!is.na(herbarium_nbe_type))
    
    if (nrow(all_herb_type) != nrow(all_herb_ref)) {
      
      print(all_herb_type)
      print(all_herb_ref)
      cli::cli_alert_warning("Number of herbarium specimen type and reference are not identical")
      
      missing_herb_ref <-
        all_herb_type %>%
        filter(!herbarium_nbe_type %in% all_herb_ref$herbarium_nbe_char)
      
      if(nrow(missing_herb_ref) > 0) {
        print(missing_herb_ref)
        stop("Specimen in type not found in reference specimens")
      }
      
      missing_herb_type <- all_herb_ref %>%
        filter(!herbarium_nbe_char %in% all_herb_type$herbarium_nbe_type)
      
      
      if(nrow(missing_herb_type) > 0) {
        
        cli::cli_alert_danger("Some specimens type not represented in specimens links")
        
        print(missing_herb_type)
        
        complete_type_specimen <-
          choose_prompt(message = "Complete automatically type specimen by taking the first individual?")
        
        
        
        if(complete_type_specimen) {
          
          new_data_renamed <-
            new_data_renamed %>%
            mutate(id_temp = 1:nrow(.))
          
          for (i in 1:nrow(missing_herb_type)) {
            
            id_selected <-
              new_data_renamed %>%
              filter(herbarium_nbe_char == missing_herb_type$herbarium_nbe_char[i]) %>%
              arrange(tag, id_table_liste_plots_n) %>%
              dplyr::slice(1) %>%
              dplyr::select(id_temp)
            
            new_data_renamed <-
              new_data_renamed %>%
              mutate(herbarium_nbe_type = replace(herbarium_nbe_type,
                                                  id_temp == id_selected$id_temp,
                                                  missing_herb_type$herbarium_nbe_char[i]))
          }
          
          new_data_renamed <-
            new_data_renamed %>%
            dplyr::select(-id_temp)
          
        }
      }
    }
    
    herb_type_dups <-
      new_data_renamed %>%
      dplyr::group_by(herbarium_nbe_type) %>%
      dplyr::count() %>%
      dplyr::filter(n > 1,!is.na(herbarium_nbe_type))
    
    
    if (nrow(herb_type_dups) > 0) {
      
      warning(paste(
        "herbarium_nbe_type is duplicated for",
        nrow(herb_type_dups),
        "specimen"
      ))
      
      new_data_renamed %>%
        dplyr::filter(herbarium_nbe_type %in% dplyr::pull(herb_type_dups, herbarium_nbe_type))
      
      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_type",
                           note = paste(
                             "herbarium_nbe_type is duplicated for",
                             paste(
                               dplyr::pull(herb_type_dups, herbarium_nbe_type),
                               collapse = ";"
                             ),
                             "specimen"
                           )
                         ))
    }
  }
  
  ## check herbarium specimen reference coherence
  if(any(colnames(new_data_renamed)=="herbarium_nbe_char")) {
    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char, idtax_n) %>%
      dplyr::filter(!is.na(herbarium_nbe_char)) %>%
      dplyr::group_by(herbarium_nbe_char) %>%
      dplyr::count() %>%
      dplyr::filter(n>1)
    
    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::filter(herbarium_nbe_char %in% dplyr::pull(herb_ref_multiple_taxa, herbarium_nbe_char)) %>%
      dplyr::select(herbarium_nbe_char, original_tax_name, idtax_n) %>%
      dplyr::distinct()
    
    if(nrow(herb_ref_multiple_taxa) > 0) {
      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_char",
                           note = paste("herbarium_nbe_char carry different identification for",
                                        paste(herb_ref_multiple_taxa %>%
                                                dplyr::distinct(herbarium_nbe_char) %>%
                                                dplyr::pull(),
                                              collapse = "; "),
                                        paste(herb_ref_multiple_taxa %>%
                                                dplyr::distinct(original_tax_name) %>%
                                                dplyr::pull(),
                                              collapse = ", "))
                         ))
    }
    
  }
  
  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )
  
  if(launch_adding_data) {
    
    print(list(new_data_renamed, logs))
    
    confirmed <- choose_prompt(message = "Confirm adding?")
    
    
    if(confirmed) {
      
      DBI::dbWriteTable(mydb, "data_individuals", new_data_renamed, append = TRUE, row.names = FALSE)
      cli::cli_alert_success("Added individuals : {nrow(new_data_renamed)} rows to individuals table")
    }
  }
  
  return(list(new_data_renamed, logs))
  
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
  
  mydb <- call.mydb()
  
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
          dplyr::select(idtax_n, 
                        id_n, 
                        # sous_plot_name
          ) %>%
          dplyr::filter(id_n %in% !!unique(new_data_renamed$id_n)) %>%
          dplyr::collect() %>%
          dplyr::mutate(rrr = 1),
        by = c("id_n" = "id_n")
      )
    
    if (dplyr::filter(link_individuals, is.na(rrr)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_individuals
                          , 
                          is.na(rrr)
      ))
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
          "tag"
        
        data_trait <-
          data_trait %>%
          dplyr::rename_at(dplyr::vars(all_of(individual_plot_field)), ~ individual_plot)
        
        
        ## not numeric or missing individuals tag
        nbe_not_numeric <-
          suppressWarnings(which(is.na(as.numeric(data_trait$tag))))
        
        data_trait <-
          data_trait %>%
          dplyr::mutate(tag = as.numeric(tag))
        
        if(length(nbe_not_numeric) > 0) {
          cli::cli_alert_warning(
            "Number of non numeric (or missing) value in column indicating invividual number in plot : {length(nbe_not_numeric)}"
          )
          print(nbe_not_numeric)
          
          data_trait <-
            data_trait %>%
            filter(!is.na(tag))
          
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
          dplyr::select(tag, id_table_liste_plots_n,
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
                             by=c("tag" = "tag"))
          
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
                            tag,
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
            dplyr::select(trait, plot_name, tag, id_data_individuals, id_new_data)
          
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

      
      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {
        
        duplicates_lg <- duplicated(data_to_add)
        
        cli::cli_alert_warning("Duplicates in new data for {trait} concerning {length(duplicates_lg[duplicates_lg])} id(s)")
        

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
        
      } else{
        
        cli::cli_alert_danger("No added data for {trait} - add_data is FALSE")
        
      }
      
    } else{
      
      cli::cli_alert_danger("No added data for {trait} - no values different of 0")
      
    }
  }
  
  if(exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))
  
  if(!exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data))
  
  
  
}






#' Add new specimens data
#'
#' Add new specimens data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be imported
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param plot_name_field integer indicate which name of col_names_select is the id for matching liste plots table
#' @param collector_field integer indicate which name of col_names_select is the id for matching collector
#'
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_specimens <- function(new_data ,
                          col_names_select,
                          col_names_corresp,
                          # id_col,
                          plot_name_field = NULL,
                          collector_field = NULL,
                          launch_adding_data = FALSE) {
  
  # logs <-
  #   dplyr::tibble(
  #     column = as.character(),
  #     note = as.character()
  #   )
  
  mydb <- call.mydb()
  mydb_taxa <- call.mydb.taxa()
  
  if(length(col_names_select)!=length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")
  
  new_data_renamed <-
    new_data %>%
    mutate(id_new_data=1:nrow(.))
  
  for (i in 1:length(col_names_select)) {
    if (any(colnames(new_data_renamed) == col_names_select[i])) {
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename(!!col_names_corresp[i] := !!col_names_select[i])
      # dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_names_select[i]
      ))
    }
  }
  
  col_names_corresp <-
    c(col_names_corresp, "id_new_data")
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))
  
  ### check diconame id
  if(!any(colnames(new_data_renamed)=="idtax_n")) stop("idtax_n column missing")
  
  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))
  
  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))
  
  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(try_open_postgres_table(table = "table_taxa", con = mydb_taxa) %>%
                       dplyr::select(idtax_n, idtax_good_n) %>%
                       dplyr::filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
                       dplyr::collect() %>%
                       dplyr::mutate(tag = 1), by=c("idtax_n" = "idtax_n")) %>%
    dplyr::filter(is.na(tag)) %>%
    dplyr::pull(idtax_n)
  
  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in table_taxa", unmatch_id_diconame))
  
  
  ### check locality and adding it if link to plots
  if(!any(colnames(new_data_renamed) == "locality"))
    warning("locality column missing"
    )
  
  ### Linking collectors names
  if (!is.null(collector_field)) {
    
    
    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed,
                   column_searched = collector_field)
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-original_colnam)
    
  } else{
    if (!any(colnames(new_data_renamed) == "id_colnam"))
      stop("indicate the field if of collector name for standardizing")
  }
  
  ### check determination data
  if (any(colnames(new_data_renamed) == "detd")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detd = as.numeric(detd))
  }
  
  if(any(colnames(new_data_renamed) == "detm")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detm = as.numeric(detm))
  }
  
  if (any(colnames(new_data_renamed) == "dety")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(dety = as.numeric(dety))
  }
  
  if (!any(names(new_data_renamed) == "suffix")) {
    
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(suffix = NA) %>%
      dplyr::mutate(suffix = as.character(suffix))
    
  }
  
  ## check if not duplicates in new specimens
  dup_imported_datasets <-
    new_data_renamed %>%
    dplyr::select(colnbr, id_colnam, suffix, id_new_data) %>%
    group_by(colnbr, id_colnam, suffix) %>%
    count() %>%
    filter(n > 1)
  
  if (nrow(dup_imported_datasets) > 0) {
    print(dup_imported_datasets)
    stop("Duplicates in imported dataset")
  }
  
  
  ## check if specimens are not already in database
  matched_specimens <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::select(colnbr, id_colnam, id_specimen) %>%
    dplyr::filter(!is.na(id_colnam)) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      new_data_renamed %>%
        dplyr::select(colnbr, id_colnam, id_new_data),
      by = c("colnbr" = "colnbr", "id_colnam" = "id_colnam")
    ) %>%
    dplyr::filter(!is.na(id_new_data))
  
  
  if (nrow(matched_specimens) > 0) {
    warning(paste("New specimens already in database", nrow(matched_specimens)))
    print(matched_specimens)
  }
  
  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
                       data_modif_m=lubridate::month(Sys.Date()),
                       data_modif_y=lubridate::year(Sys.Date()))
  
  if (any(colnames(new_data_renamed) == "col_name"))
    new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-col_name)
  
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-id_new_data)
  
  if(launch_adding_data) {
    
    print(list(new_data_renamed))
    
    confirmed <- choose_prompt(message = "Confirm adding?")
    
    if(confirmed) {
      
      DBI::dbWriteTable(mydb, "specimens", new_data_renamed, append = TRUE, row.names = FALSE)
      
      message(paste0(nrow(new_data_renamed), " records added to specimens table"))
    }
    
  }
  
  return(list(new_data_renamed))
  
}





#' Add 1ha IRd plot coordinates
#'
#' print table as html in viewer reordered
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble
#' @param ddlat column name of dataset containing latitude in decimal degrees
#' @param ddlon column name of dataset containing longitude in decimal degrees
#' @param launch_add_data whether addd data or not
#' @param X_theo column that contain the X quadrat name
#' @param Y_theo column that contain the Y quadrat name
#' @param check_existing_data check if data already exists
#' @param add_cols string character vectors with columns names of dataset of additonal information
#' @param cor_cols string character vectors with colums names corresponding to add_cols
#' @param collector_field string vector of size one with column name containing the name of the person collecting data
#'
#' @return print html in viewer
#' @export
add_plot_coordinates <-
  function(dataset,
           ddlat = "Latitude",
           ddlon = "Longitude",
           launch_add_data = FALSE,
           X_theo = "X_theo",
           Y_theo = "Y_theo",
           check_existing_data = TRUE,
           add_cols = NULL,
           cor_cols = NULL,
           collector_field = NULL) {
    
    X_theo_p <- dplyr::sym(X_theo)
    Y_theo_p <- dplyr::sym(Y_theo)
    
    dataset <- 
      dataset %>% 
      mutate(quadrat = paste(!!X_theo_p, !!Y_theo_p, sep = "_"))
    
    all_q <- dataset %>%
      distinct(quadrat) %>% pull()
    
    all_cols <- c(ddlat, ddlon)
    
    res_l <- vector('list', length(all_cols))
    for (i in 1:length(all_cols)) {
      col_s <- dplyr::sym(all_cols[i])
      
      if (!any(names(dataset) == col_s))
        stop(glue::glue("{col_s} column not found"))
      
      if (i == 1)
        names_pref <- "ddlat_plot_X_Y_"
      if (i == 2)
        names_pref <- "ddlon_plot_X_Y_"
      
      dataset <-
        dataset %>%
        mutate(!!col_s := as.numeric(!!col_s))
      
      res_l[[i]] <-
        tidyr::pivot_wider(
          data = dataset,
          names_from = quadrat,
          values_from = !!col_s,
          names_prefix = names_pref
        ) %>%
        group_by(plot_name) %>%
        summarise(across(starts_with(names_pref), ~ mean(.x, na.rm = TRUE)),
                  across(all_of(add_cols), ~ first(.x)),
                  across(all_of(collector_field), ~ first(.x)))
      
      print(res_l[[i]])
      
      if (launch_add_data) {
        
        add_subplot_features(new_data = res_l[[i]], 
                             col_names_select = add_cols, 
                             col_names_corresp = cor_cols, 
                             plot_name_field = "plot_name", 
                             subplottype_field = res_l[[i]] %>% 
                               dplyr::select(starts_with("ddl")) %>% names(), 
                             add_data = TRUE,
                             ask_before_update = FALSE,
                             check_existing_data = check_existing_data)
        
      } else {
        cli::cli_alert_danger("No data added because launch_add_data is FALSE")
      }
    }
    
    return(res_l)
    
  }





#' Add a method in method list
#'
#' Add method and associated descriptors in method list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_method string value with new method descriptors, avoid space
#' @param new_description_method
#'
#'
#' @export
add_method <- function(new_method = NULL,
                       new_description_method = NULL) {
  
  if(is.null(new_method)) stop("define new method")
  
  mydb <- call.mydb()
  
  new_data_renamed <- tibble(
    method = new_method,
    description_method = ifelse(is.null(new_description_method), NA, new_description_method)
  )
  
  print(new_data_renamed)
  
  # Q <- utils::askYesNo("confirm adding this method ?")
  Q <- choose_prompt(message = "confirm adding this method ?")
  
  if(Q) DBI::dbWriteTable(mydb, "methodslist", new_data_renamed, append = TRUE, row.names = FALSE)
  
}





#' Add trait
#'
#' @description
#' Add trait and associated descriptors in trait list table
#' 
#' @param new_trait A single string.
#' @param new_relatedterm Optional. A single string.
#' @param new_valuetype A single string, one of `"numeric"`, `"integer"`, `"categorical"`, `"ordinal"`, `"logical"`, `"character"`, `"table_data_liste_plots"`, or `"table_colnam"`.
#' @param new_maxallowedvalue Optional. if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue Optional. if valuetype is numeric, indicate the minimum allowed value
#' @param new_traitdescription Optional. A single string.
#' @param new_factorlevels Optional. Factor levels.
#' @param new_expectedunit Optional. A single string.
#' @param new_comments Optional. A single string.
#'
#' @returns 
#' The function writes to a database table if confirmed by the user. The function
#' will error if `new_trait` or `new_valuetype` are not provided, if `new_valuetype`
#' is not one of the allowed values, or if numeric/integer value types don't match
#' their corresponding min/max values.
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
  
  mydb <- call.mydb()
  
  if(is.null(new_trait)) stop("define new trait")
  if(is.null(new_valuetype)) stop("define new_valuetype")
  
  if (!any(
    new_valuetype == c(
      'numeric',
      'integer',
      'categorical',
      'ordinal',
      'logical',
      'character',
      'table_data_liste_plots',
      'table_colnam'
    )
  ))
  stop(
    "valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character', 'table_data_liste_plots' or 'table_colnam'"
  )
  
  if (new_valuetype == "numeric" | new_valuetype == "integer")
    if (!is.numeric(new_maxallowedvalue) &
        !is.integer(new_maxallowedvalue))
      stop("valuetype numeric of integer and max value not of this type")
  if (new_valuetype == "numeric" | new_valuetype == "integer")
    if (!is.numeric(new_minallowedvalue) &
        !is.integer(new_minallowedvalue))
      stop("valuetype numeric of integer and min value not of this type")
  
  new_data_renamed <- tibble(
    trait = new_trait,
    relatedterm = ifelse(is.null(new_relatedterm), NA, new_relatedterm),
    valuetype = new_valuetype,
    maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
    minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
    traitdescription = ifelse(is.null(new_traitdescription), NA, new_traitdescription),
    factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
    expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
    comments = ifelse(is.null(new_comments), NA, new_comments)
  )
  
  print(new_data_renamed)
  
  # Q <- utils::askYesNo("confirm adding this trait?")
  Q <- choose_prompt(message = "confirm adding this trait ?")
  
  if(Q) DBI::dbWriteTable(mydb, "traitlist", new_data_renamed, append = TRUE, row.names = FALSE)
  
}







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
                                   ask_before_update = TRUE,
                                   basisofrecord = NULL,
                                   measurementremarks = NULL,
                                   interactive = TRUE) {
  
  
  
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
      if (!any(colnames(data_trait) == "basisofrecord")) {

        # Use parameter if provided, otherwise prompt if interactive
        if (!is.null(basisofrecord)) {
          # Use provided basisofrecord
          data_trait <-
            data_trait %>%
            mutate(basisofrecord = rep(basisofrecord, nrow(.)))

        } else if (interactive) {
          # Interactive prompt
          cli::cli_h3("basis")
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

        } else {
          stop("basisofrecord must be provided in non-interactive mode")
        }
      }
      
      ### choosing measurementremarks if none
      if (!any(colnames(data_trait) == "measurementremarks")) {

        # Use parameter if provided, otherwise prompt if interactive
        if (!is.null(measurementremarks) && nchar(trimws(measurementremarks)) > 0) {
          # Use provided measurementremarks
          data_trait <-
            data_trait %>%
            mutate(measurementremarks = rep(measurementremarks, nrow(.)))

        } else if (interactive) {
          # Interactive prompt
          cli::cli_h3("remarks")
          selected_measurementremarks <-
            readline(prompt = "Add measurementremarks ? 'enter if none : ")

          if (selected_measurementremarks != "") {
            data_trait <-
              data_trait %>%
              mutate(measurementremarks = rep(selected_measurementremarks, nrow(.)))
          }
        }
        # If not provided and not interactive, leave as NA (optional field)
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




