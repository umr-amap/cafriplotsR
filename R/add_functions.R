



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
      !is.null(col_names_corresp) &
      length(col_names_select) > 0) {
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

    # For table_colnam type features (e.g. team_leader, additional_people),
    # resolve person names to id_table_colnam IDs and separate comma-delimited values
    if (isTRUE(any(valuetype$valuetype == "table_colnam", na.rm = TRUE))) {

      data_subplottype <-
        data_subplottype %>%
        tidyr::separate_rows(subplotype, sep = ",") %>%
        dplyr::mutate(subplotype = stringr::str_squish(as.character(subplotype))) %>%
        dplyr::filter(!is.na(subplotype) & subplotype != "")

      # Detect whether subplotype values are already numeric IDs (e.g. from
      # the Shiny import wizard Step 4 lookup matching, which pre-resolves
      # person names to id_table_colnam IDs). If so, bypass .link_colnam()
      # — calling it with numeric IDs would make .link_table() try to match
      # "123" as a person name in table_colnam.colnam and fall into the
      # interactive .find_cat() readline() loop, which hangs/errors silently
      # in non-interactive Shiny context.
      values_are_ids <-
        nrow(data_subplottype) > 0 &&
        !any(is.na(suppressWarnings(as.numeric(data_subplottype$subplotype))))

      if (values_are_ids) {

        if (verbose) cli::cli_alert_info(
          "{subplottype} values are already numeric IDs (pre-matched); skipping .link_colnam()"
        )

        # Validate IDs against table_colnam
        valid_ids <- dplyr::tbl(mydb, "table_colnam") %>%
          dplyr::select(id_table_colnam) %>%
          dplyr::collect() %>%
          dplyr::pull(id_table_colnam)

        provided_ids <- as.integer(data_subplottype$subplotype)
        invalid_ids <- provided_ids[!(provided_ids %in% valid_ids)]

        if (length(invalid_ids) > 0) {
          stop(sprintf(
            "Invalid %s IDs not found in table_colnam: %s",
            subplottype,
            paste(unique(invalid_ids), collapse = ", ")
          ))
        }

        data_subplottype <- data_subplottype %>%
          dplyr::mutate(id_colnam = provided_ids)

      } else {

        data_subplottype <- .link_colnam(
          data_stand = data_subplottype,
          column_searched = "subplotype",
          column_name = "colnam",
          id_field = "id_colnam",
          id_table_name = "id_table_colnam",
          db_connection = mydb,
          table_name = "table_colnam"
        )

        # CRITICAL: .link_colnam() calls .link_table() with keep_original_value = TRUE,
        # which renames the searched column ("subplotype") to "original_colnam".
        # The downstream data_to_add tibble construction references
        # data_subplottype$subplotype, which would otherwise be NULL and silently
        # produce invalid data/errors. Restore the subplotype column here.
        if ("original_colnam" %in% colnames(data_subplottype) &&
            !"subplotype" %in% colnames(data_subplottype)) {
          data_subplottype <- data_subplottype %>%
            dplyr::rename(subplotype = original_colnam)
        }
      }
    } else {
      # For non-table_colnam features, add empty id_colnam column
      data_subplottype <- data_subplottype %>%
        dplyr::mutate(id_colnam = NA_integer_)
    }

    # Ensure issue column exists (created by .link_subplotype, may be NA)
    if (!("issue" %in% colnames(data_subplottype))) {
      data_subplottype <- data_subplottype %>%
        dplyr::mutate(issue = NA_character_)
    } else {
      # Ensure issue is character type
      data_subplottype <- data_subplottype %>%
        dplyr::mutate(issue = as.character(issue))
    }

    data_to_add <-
      dplyr::tibble(id_table_liste_plots = data_subplottype$id_liste_plots,
                    year = data_subplottype$year,
                    month = data_subplottype$month,
                    day = data_subplottype$day,
                    id_type_sub_plot = data_subplottype$id_subplottype,
                    id_colnam = data_subplottype$id_colnam,
                    typevalue = ifelse(rep(isTRUE(any(valuetype$valuetype %in% c("numeric", "table_colnam"), na.rm = TRUE)),
                                           nrow(data_subplottype)), suppressWarnings(as.numeric(data_subplottype$subplotype)), NA_real_),
                    typevalue_char = ifelse(rep(isTRUE(any(valuetype$valuetype == "character", na.rm = TRUE)),
                                                nrow(data_subplottype)), as.character(data_subplottype$subplotype), NA_character_),
                    original_subplot_name = ifelse(rep(any(colnames(data_subplottype)=="original_subplot_name"),
                                                       nrow(data_subplottype)), as.character(data_subplottype$original_subplot_name), NA_character_),
                    issue = data_subplottype$issue,
                    comment = ifelse(rep(any(colnames(data_subplottype)=="comment"),
                                         nrow(data_subplottype)), as.character(data_subplottype$comment), NA_character_),
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

      # Always log data structure to console for debugging
      cat("\n[DEBUG] data_to_add structure:\n")
      cat("Columns: ", paste(colnames(data_to_add), collapse = ", "), "\n")
      cat("Types: ", paste(sapply(data_to_add, function(x) paste(class(x), collapse="/")), collapse = ", "), "\n")
      cat("Rows: ", nrow(data_to_add), "\n")
      if (nrow(data_to_add) > 0) {
        cat("First row values: ", paste(sapply(data_to_add[1,], function(x) {
          if (is.na(x)) "NA" else as.character(x)
        }), collapse = " | "), "\n")
      }
      cat("\n")

      tryCatch({
        DBI::dbWriteTable(mydb, "data_liste_sub_plots",
                          data_to_add, append = TRUE, row.names = FALSE)

        cli::cli_alert_success("{nrow(data_to_add)} line imported in data_liste_sub_plots")
      }, error = function(e) {
        error_msg <- e$message
        if (is.null(error_msg) || identical(error_msg, "")) {
          error_msg <- "Unknown error - check database schema constraints"
        }
        cat("\n[ERROR] Insert failed: ", error_msg, "\n\n")
        cli::cli_alert_danger("Error inserting data into data_liste_sub_plots: {error_msg}")
        stop("Subplot feature insertion failed: ", error_msg, call. = FALSE)
      })
      
      
      
      
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
            suppressWarnings(as.numeric(data_feat$subplotype)),
            NA_real_
          ),
          typevalue_char = ifelse(
            rep(val_type == "character", nrow(data_feat)),
            as.character(data_feat$subplotype),
            NA_character_
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
    dplyr::mutate(
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
                                         add_data = FALSE) {

  # Get database connection
  mydb <- call.mydb()

  # Handle connection pool checkout
  is_pool <- inherits(mydb, "Pool")
  if (is_pool) {
    actual_con <- pool::poolCheckout(mydb)
    on.exit({
      pool::poolReturn(actual_con)
    }, add = TRUE)
  } else {
    actual_con <- mydb
  }

  # If add_data is TRUE, use transaction
  if (add_data) {

    DBI::dbBegin(actual_con)

    tryCatch({

      # Call internal helper with interactive mode
      result <- .add_trait_features_internal(
        new_data = new_data,
        id_trait_measures = id_trait_measures,
        features = features,
        con = actual_con,
        allow_multiple_value = allow_multiple_value,
        interactive = TRUE  # Show prompts for backward compatibility
      )

      # Commit transaction
      DBI::dbCommit(actual_con)
      cli::cli_alert_success("Transaction committed - features added successfully")

      return(result)

    }, error = function(e) {
      # Rollback on error
      tryCatch({
        DBI::dbRollback(actual_con)
        cli::cli_alert_danger("Transaction rolled back due to error: {e$message}")
      }, error = function(rollback_error) {
        cli::cli_alert_danger("Error during rollback: {rollback_error$message}")
      })
      stop(e)
    })

  } else {

    # Preview mode (add_data = FALSE) - no transaction needed
    result <- .add_trait_features_internal(
      new_data = new_data,
      id_trait_measures = id_trait_measures,
      features = features,
      con = actual_con,
      allow_multiple_value = allow_multiple_value,
      interactive = TRUE
    )

    return(result)

  }

}




#' Internal: Execute trait measurement insert with RETURNING clause
#'
#' @description
#' Performs INSERT with RETURNING to get auto-generated IDs immediately.
#' Uses safe value escaping for SQL injection prevention.
#'
#' @param data_to_add Data frame with trait measurement data to insert
#' @param con Database connection (must be actual connection, not pool)
#'
#' @return Data frame with id_trait_measures and id_data_individuals columns
#'
#' @keywords internal
#' @noRd
.execute_trait_insert_with_returning <- function(data_to_add, con) {

  # Use a session-scoped temp table so data is sent via COPY (efficient for
  # large payloads) rather than building a giant VALUES string in R.
  tmp <- paste0("tmp_trait_insert_", format(Sys.time(), "%H%M%S%OS3"))
  tmp_id <- DBI::dbQuoteIdentifier(con, tmp)

  on.exit(
    tryCatch(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", tmp_id)),
             error = function(e) NULL),
    add = TRUE
  )

  # Write to temp table — RPostgres uses COPY for dbAppendTable / dbWriteTable
  DBI::dbWriteTable(con, tmp, data_to_add, temporary = TRUE, overwrite = TRUE,
                    row.names = FALSE)

  col_names <- paste(DBI::dbQuoteIdentifier(con, names(data_to_add)), collapse = ", ")

  insert_sql <- sprintf(
    "INSERT INTO data_traits_measures (%s) SELECT %s FROM %s RETURNING %s, %s",
    col_names,
    col_names,
    tmp_id,
    DBI::dbQuoteIdentifier(con, "id_trait_measures"),
    DBI::dbQuoteIdentifier(con, "id_data_individuals")
  )

  DBI::dbGetQuery(con, insert_sql)
}



#' Internal: Add trait features without prompts (for transactional use)
#'
#' @description
#' Core logic for adding trait measurement features. Designed to be called
#' within transactions without user prompts when interactive = FALSE.
#'
#' @param new_data Data frame with feature data
#' @param id_trait_measures Column name containing trait measurement IDs
#' @param features Character vector of feature column names
#' @param con Database connection (must be actual connection, not pool)
#' @param allow_multiple_value Logical, allow multiple values per measurement
#' @param interactive Logical, whether to show prompts (default TRUE)
#'
#' @return List of data frames prepared for each feature
#'
#' @keywords internal
#' @noRd
.add_trait_features_internal <- function(new_data,
                                         id_trait_measures = "id_trait_measures",
                                         features,
                                         con,
                                         allow_multiple_value = FALSE,
                                         interactive = TRUE) {

  # Validate features exist in data
  for (i in 1:length(features))
    if (!any(colnames(new_data) == features[i]))
      stop(paste("features field not found in new_data:", features[i]))

  new_data_renamed <- new_data

  # Remove entries with NA values for all features
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(features), ~ !is.na(.x)))

  if (nrow(new_data_renamed) == 0)
    stop("no values for selected features(s)")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::mutate(id_new_data = dplyr::row_number())

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::rename_with(~ "id_trait_measures", dplyr::all_of(id_trait_measures))

  # Validate trait_measures exist in database
  provided_tm_ids <- unique(new_data_renamed$id_trait_measures)
  provided_tm_ids <- provided_tm_ids[!is.na(provided_tm_ids)]

  found_tm_ids <-
    try_open_postgres_table(table = "data_traits_measures", con = con) %>%
    dplyr::filter(.data$id_trait_measures %in% !!provided_tm_ids) %>%
    dplyr::pull("id_trait_measures")

  missing_tm_ids <- setdiff(provided_tm_ids, found_tm_ids)
  if (length(missing_tm_ids) > 0) {
    print(missing_tm_ids)
    stop("provided trait_measures not found in data_traits_measures")
  }

  # Prepare dataset to add for each feature
  list_add_data <- vector('list', length(features))

  for (i in 1:length(features)) {

    feat <- features[i]
    if (!any(colnames(new_data_renamed) == feat))
      stop(paste("feat field not found", feat))

    data_feat <- new_data_renamed

    data_feat <-
      data_feat %>%
      dplyr::filter(!is.na(!!rlang::sym(feat)))

    if (nrow(data_feat) > 0) {

      # Link to trait definition
      data_feat <-
        .link_trait(data_stand = data_feat, trait = feat)

      # Determine value type (numeric vs character)
      valuetype <-
        data_feat %>%
        dplyr::distinct(.data$id_trait) %>%
        dplyr::left_join(
          get_traitlist(con)[, c("valuetype", "id_trait"), drop = FALSE],
          by = c("id_trait" = "id_trait")
        )

      # Handle table_colnam type (people references)
      if (valuetype$valuetype == "table_colnam") {

        add_col_sep <-
          data_feat %>%
          tidyr::separate_rows(trait, sep = ",") %>%
          dplyr::mutate(trait = stringr::str_squish(trait))

        add_col_sep <- .link_colnam(
          data_stand = add_col_sep,
          column_searched = "trait",
          column_name = "colnam",
          id_field = "trait",
          id_table_name = "id_table_colnam",
          db_connection = con,
          table_name = "table_colnam"
        )

        data_feat <- add_col_sep

      }

      # Handle zero values
      if (any(data_feat$trait == 0, na.rm = TRUE)) {

        if (interactive) {
          add_0 <- choose_prompt(message = "Some values are equal to 0. Do you want to add these values anyway?")
        } else {
          add_0 <- FALSE  # Skip zeros in non-interactive mode
        }

        if (!add_0)
          data_feat <-
            data_feat %>%
            dplyr::filter(trait != 0)

      }

      # Add modification date fields
      if (interactive) cli::cli_h3(".add_modif_field")
      data_feat <-
        .add_modif_field(dataset = data_feat)

      # Determine storage column based on valuetype
      val_type <- if (valuetype$valuetype %in% c("numeric", "integer", "table_colnam")) {
        "numeric"
      } else if (valuetype$valuetype %in% c("character", "ordinal", "categorical")) {
        "character"
      } else {
        stop(paste("unhandled valuetype:", valuetype$valuetype))
      }

      # Prepare data for insert
      if (interactive) cli::cli_h3("data_to_add")

      data_to_add <-
        dplyr::tibble(
          id_trait_measures = data_feat$id_trait_measures,
          id_trait = data_feat$id_trait,
          typevalue = ifelse(
            rep(val_type == "numeric", nrow(data_feat)),
            suppressWarnings(as.numeric(data_feat$trait)),
            NA_real_
          ),
          typevalue_char = ifelse(
            rep(val_type == "character", nrow(data_feat)),
            as.character(data_feat$trait),
            NA_character_
          ),
          date_modif_d = data_feat$date_modif_d,
          date_modif_m = data_feat$date_modif_m,
          date_modif_y = data_feat$date_modif_y
        )

      list_add_data[[i]] <- data_to_add

      if (interactive) print(data_to_add)

      # Handle duplicates
      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {

        duplicates_lg <- duplicated(data_to_add)

        cli::cli_alert_warning("Duplicates in new data for {feat} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        if (interactive) {
          cf_merge <- choose_prompt(message = "confirm merging duplicates?")
        } else {
          cf_merge <- TRUE  # Auto-merge in non-interactive mode
        }

        if (cf_merge) {
          data_to_add <- data_to_add %>% dplyr::distinct()
        } else {
          if (!allow_multiple_value) stop("Duplicates found and not allowed")
        }

      }

      # Insert features (no prompt if non-interactive)
      if (interactive) {
        response <- choose_prompt(message = "Confirm add these data to data_ind_measures_feat table?")
      } else {
        response <- TRUE
      }

      if (response) {

        DBI::dbWriteTable(con, "data_ind_measures_feat",
                          data_to_add,
                          append = TRUE,
                          row.names = FALSE)

        if (interactive) {
          cli::cli_alert_success("Adding data: {nrow(data_to_add)} values added for feature {feat}")
        }
      }

    } else {

      if (interactive) cli::cli_alert_info("no added data for {feat} - no values different of 0")

    }
  }

  return(list(list_features_add = list_add_data))

}



#' Add an observation in trait measurement table
#'
#' Add a trait measure in trait measurement table
#'
#' @details
#' This function now uses database transactions to ensure atomic operations.
#' When \code{features_field} is provided, both measurements and their features
#' are added together in a single transaction. If any error occurs, all changes
#' are rolled back automatically.
#'
#' The function uses PostgreSQL's RETURNING clause for efficient ID retrieval,
#' eliminating the need for separate queries to fetch generated IDs. This improves
#' reliability and prevents race conditions from concurrent operations.
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param collector_field string column name which contain the collector name
#' @param plot_name_col string column name containing plot names for linking to
#'   \code{id_liste_plots}. Use together with \code{tag_col} to resolve
#'   individuals by plot name + tag combination.
#' @param tag_col string column name containing individual tag numbers. Used
#'   together with \code{plot_name_col} to resolve individuals by plot + tag.
#' @param id_individual_col string column name containing individual IDs
#'   (\code{id_n} from \code{data_individuals}). Self-sufficient — no plot ID
#'   argument is required when this is provided; the plot ID is resolved
#'   internally only when needed for census lookup.
#' @param traits_field string vector listing trait columns names in new_data
#' @param features_field string vector listing features (column names) to link to measurements in new_data
#' @param add_data logical whether or not data should be added - by default FALSE
#' @param allow_multiple_value if multiple values linked to one individual can be uploaded at once
#' @param census_col string. Optional column name in \code{new_data} containing the
#'   census typevalue (integer, e.g. 1, 2, 3). The corresponding \code{id_sub_plots}
#'   is resolved automatically from the database per plot × census. Rows with
#'   different census values are handled correctly within a single call.
#'   Takes precedence over the interactive census prompt but is overridden by
#'   \code{id_sub_plots_col}.
#' @param id_sub_plots_col string. Optional column name in \code{new_data} containing
#'   the \code{id_sub_plots} value directly. When provided, no database lookup or
#'   interactive prompt is performed. Takes precedence over \code{census_col}.
#'
#' @export
add_traits_measures <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = NULL,
                                collector_field = NULL,
                                plot_name_col = NULL,
                                tag_col = NULL,
                                id_individual_col = NULL,
                                traits_field,
                                features_field = NULL,
                                allow_multiple_value = FALSE,
                                add_data = FALSE,
                                census_col = NULL,
                                id_sub_plots_col = NULL) {
  
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
    dplyr::filter(dplyr::if_any(dplyr::all_of(traits_field), ~ !is.na(.)))
  
  if (nrow(new_data_renamed) == 0)
    stop("no values for selected trait(s)")
  
  if (!any(col_names_corresp == "day")) {
    cli::cli_alert_info("no information collection day provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(day = NA) %>%
      mutate(day = as.numeric(day))
    
    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus date is mandatory")
  }

  if (!any(col_names_corresp == "year")) {
    cli::cli_alert_info("no information collection year provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(year = NA) %>%
      mutate(year = as.numeric(year))

    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus date is mandatory")
  }

  if (!any(col_names_corresp == "month")) {
    cli::cli_alert_info("no information collection month provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(month = NA) %>%
      mutate(month = as.numeric(month))

    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus date is mandatory")
  }

  if(!any(col_names_corresp == "country")) {
    cli::cli_alert_info("no country provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(country = NA) %>%
      mutate(country = as.character(country))

    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus country is mandatory")

  }

  if (!any(col_names_corresp == "decimallatitude")) {
    cli::cli_alert_info("no decimallatitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(decimallatitude = NA) %>%
      dplyr::mutate(decimallatitude = as.double(decimallatitude))

    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus decimallatitude is mandatory")
  }
  
  if (!any(col_names_corresp == "decimallongitude")) {
    cli::cli_alert_info("no decimallongitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(decimallongitude = NA) %>%
      dplyr::mutate(decimallongitude = as.double(decimallongitude))
    
    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus decimallongitude is mandatory")
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
      mutate(id_colnam = NA_integer_)
    
    if (is.null(plot_name_col) & is.null(tag_col) & is.null(id_individual_col))
      stop("no links provided (either plot_name_col+tag_col or id_individual_col), thus collector_field is mandatory")
  }
  
  ### Linking plot names
  if(!is.null(plot_name_col)) {
    if (!any(colnames(new_data_renamed) == plot_name_col))
      stop("plot_name_col not found in colnames")

    new_data_renamed <-
      .link_table(data_stand = new_data_renamed,
                  column_searched = plot_name_col,
                  column_name = "plot_name",
                  id_field = "id_liste_plots",
                  id_table_name = "id_liste_plots",
                  db_connection = mydb,
                  table_name = "data_liste_plots")
  }
  
  ### linking individuals by id
  if (!is.null(id_individual_col)) {

    new_data_renamed <- new_data_renamed %>%
      dplyr::rename(id_n = !!id_individual_col)

    provided_ids <- unique(new_data_renamed$id_n)
    provided_ids <- provided_ids[!is.na(provided_ids)]

    found_ids <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_n %in% !!provided_ids) %>%
      dplyr::pull(id_n)

    missing_ids <- setdiff(provided_ids, found_ids)
    if (length(missing_ids) > 0) {
      print(missing_ids)
      stop("provided id_individual_col values not found in data_individuals")
    }

    new_data_renamed <- new_data_renamed %>%
      dplyr::rename(id_data_individuals = id_n)

  } else {

    new_data_renamed <- new_data_renamed %>%
      dplyr::mutate(id_data_individuals = NA_integer_)

  }
  
  
  # If no plot ID resolved yet, derive it from individual IDs (needed for census lookup)
  if (is.null(plot_name_col) && !is.null(id_individual_col)) {

    ind_ids <- unique(new_data_renamed$id_data_individuals)
    ind_ids <- ind_ids[!is.na(ind_ids)]

    ind_to_plot <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_n %in% !!ind_ids) %>%
      dplyr::select(id_n, id_table_liste_plots_n) %>%
      dplyr::collect()

    new_data_renamed <- new_data_renamed %>%
      dplyr::select(-dplyr::any_of("id_table_liste_plots_n")) %>%
      dplyr::left_join(ind_to_plot, by = c("id_data_individuals" = "id_n")) %>%
      dplyr::rename(id_liste_plots = id_table_liste_plots_n)

  } else if (is.null(plot_name_col) && is.null(id_individual_col)) {

    new_data_renamed <- new_data_renamed %>%
      dplyr::mutate(id_liste_plots = NA_integer_)

  }
  
  ### Resolve census → id_sub_plots
  # Three modes, in order of precedence:
  #   1. id_sub_plots_col: column in new_data already contains id_sub_plots → use directly
  #   2. census_col: column contains census typevalue → resolve id_sub_plots from DB per plot × census
  #   3. Interactive fallback (original behaviour)

  multiple_census <- FALSE

  if (!is.null(id_sub_plots_col)) {

    # ── Mode 1: id_sub_plots provided directly ──────────────────────────────
    if (!id_sub_plots_col %in% names(new_data_renamed))
      stop(paste("id_sub_plots_col column not found in new_data:", id_sub_plots_col))

    new_data_renamed <- new_data_renamed %>%
      dplyr::rename(id_sub_plots = !!id_sub_plots_col) %>%
      dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))

    # Determine multiple_census: fetch typevalue for the used id_sub_plots
    used_subplot_ids <- unique(new_data_renamed$id_sub_plots)
    used_subplot_ids <- used_subplot_ids[!is.na(used_subplot_ids)]
    if (length(used_subplot_ids) > 0) {
      subplot_typevalues <-
        try_open_postgres_table(table = "data_liste_sub_plots", con = mydb) %>%
        dplyr::filter(id_sub_plots %in% used_subplot_ids) %>%
        dplyr::select(id_sub_plots, typevalue) %>%
        dplyr::collect()
      multiple_census <- any(subplot_typevalues$typevalue > 1, na.rm = TRUE)
    }
    cli::cli_alert_success(
      "Census linked via id_sub_plots column '{id_sub_plots_col}' ({length(used_subplot_ids)} unique census(es))"
    )

  } else if (!is.null(census_col)) {

    # ── Mode 2: census typevalue column → resolve id_sub_plots per plot × census
    if (!census_col %in% names(new_data_renamed))
      stop(paste("census_col column not found in new_data:", census_col))

    new_data_renamed <- new_data_renamed %>%
      dplyr::rename(.census_typevalue = !!census_col) %>%
      dplyr::mutate(.census_typevalue = as.integer(.census_typevalue))

    unique_ids_plots <- unique(new_data_renamed$id_liste_plots)
    all_censuses <-
      try_open_postgres_table(table = "data_liste_sub_plots", con = mydb) %>%
      dplyr::filter(id_table_liste_plots %in% unique_ids_plots, id_type_sub_plot == 27) %>%
      dplyr::select(id_table_liste_plots, id_sub_plots, typevalue) %>%
      dplyr::collect() %>%
      dplyr::mutate(typevalue = as.integer(typevalue))

    new_data_renamed <- new_data_renamed %>%
      dplyr::left_join(
        all_censuses %>% dplyr::select(id_table_liste_plots, typevalue, id_sub_plots),
        by = c("id_liste_plots" = "id_table_liste_plots",
               ".census_typevalue" = "typevalue")
      )

    unmatched <- sum(is.na(new_data_renamed$id_sub_plots))
    if (unmatched > 0)
      warning(sprintf("%d row(s) could not be matched to a census and will have id_sub_plots = NA", unmatched))

    # Determine multiple_census before dropping the typevalue column
    multiple_census <- any(new_data_renamed$.census_typevalue > 1, na.rm = TRUE)

    new_data_renamed <- new_data_renamed %>% dplyr::select(-.census_typevalue)

    cli::cli_alert_success("Census linked via typevalue column '{census_col}'")

  } else {

    # ── Mode 3: Interactive fallback (original behaviour) ───────────────────
    census_check <-
      choose_prompt(message = "Link trait measures to census (only for permanent plots) ?")

    if (census_check) {
      unique_ids_plots <- unique(new_data_renamed$id_liste_plots)
      censuses <-
        try_open_postgres_table(table = "data_liste_sub_plots", con = mydb) %>%
        dplyr::filter(id_table_liste_plots %in% unique_ids_plots, id_type_sub_plot == 27) %>%
        dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                           dplyr::select(plot_name, id_liste_plots),
                         by = c("id_table_liste_plots" = "id_liste_plots")) %>%
        dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                           dplyr::select(type, id_subplotype),
                         by = c("id_type_sub_plot" = "id_subplotype")) %>%
        dplyr::left_join(dplyr::tbl(mydb, "table_colnam") %>%
                           dplyr::select(id_table_colnam, colnam),
                         by = c("id_colnam" = "id_table_colnam")) %>%
        dplyr::collect()

      if (nrow(censuses) > 0) {

        cli::cli_alert_info("Available census(es) for the concerned plots:")
        censuses %>%
          dplyr::select(plot_name, id_table_liste_plots, year, month, day,
                        typevalue, type, colnam, additional_people) %>%
          as.data.frame() %>%
          print()
        census_chosen <- readline(prompt = "Choose census (typevalue): ")

        chosen_ids_subplots <-
          censuses %>%
          dplyr::filter(typevalue == as.numeric(census_chosen)) %>%
          dplyr::select(id_table_liste_plots, id_sub_plots)

        if (nrow(chosen_ids_subplots) == 0) stop("Chosen census not available")

        missing_census <-
          new_data_renamed %>%
          dplyr::distinct(id_liste_plots) %>%
          dplyr::filter(!id_liste_plots %in% chosen_ids_subplots$id_table_liste_plots,
                        !is.na(id_liste_plots))

        if (nrow(missing_census) > 0) {
          print(missing_census %>%
                  dplyr::left_join(
                    dplyr::tbl(mydb, "data_liste_plots") %>%
                      dplyr::select(id_liste_plots, plot_name) %>%
                      dplyr::collect(),
                    by = "id_liste_plots"
                  ) %>%
                  as.data.frame())
          warning(paste("Missing census for", nrow(missing_census),
                        "plot(s); census chosen:", census_chosen))
        }

        new_data_renamed <- new_data_renamed %>%
          dplyr::left_join(chosen_ids_subplots,
                           by = c("id_liste_plots" = "id_table_liste_plots"))

        if (as.numeric(census_chosen) > 1)
          multiple_census <- TRUE

      } else {
        new_data_renamed <- new_data_renamed %>%
          dplyr::mutate(id_sub_plots = NA_integer_)
        multiple_census <- FALSE
      }

    } else {
      new_data_renamed <- new_data_renamed %>%
        dplyr::mutate(id_sub_plots = NA_integer_)
    }
  }
  
  ### Ensure id_specimen column exists (individual features are linked to individuals, not specimens)
  if (!any(colnames(new_data_renamed) == "id_specimen")) {
    new_data_renamed <- new_data_renamed %>%
      dplyr::mutate(id_specimen = NA_integer_)
  }
  
  ### Checkout connection once for the whole operation (pool-safe)
  is_pool <- inherits(mydb, "Pool")
  transaction_active <- FALSE  # tracks open transaction for interrupt-safe cleanup
  actual_con <- if (is_pool) {
    con_checked_out <- pool::poolCheckout(mydb)
    on.exit({
      # Roll back any open transaction before returning connection to pool.
      # This handles user interrupts (Ctrl+C) which bypass tryCatch handlers.
      if (transaction_active) {
        tryCatch(DBI::dbRollback(actual_con), error = function(e) NULL)
      }
      pool::poolReturn(con_checked_out)
    }, add = TRUE)
    con_checked_out
  } else {
    mydb
  }

  ### preparing dataset to add for each trait
  list_add_data <- vector("list", length(traits_field))
  any_inserted  <- FALSE   # tracks whether at least one insert happened

  tryCatch({

  if (add_data) {
    DBI::dbBegin(actual_con)
    transaction_active <- TRUE
  }

  for (i in seq_along(traits_field)) {

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
          get_traitlist(mydb)[, c("valuetype", "id_trait"), drop = FALSE],
          by = c("id_trait" = "id_trait")
        )
      
      ### Linking individuals
      if (!is.null(tag_col)) {

        data_trait <-
          data_trait %>%
          dplyr::rename(tag = !!tag_col)
        
        
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
            dplyr::mutate(issue_2 = issue_2) %>%
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
      
      
      val_type <- if (valuetype$valuetype %in% c("numeric", "integer", "table_colnam")) {
        "numeric"
      } else if (valuetype$valuetype %in% c("character", "ordinal", "categorical")) {
        "character"
      } else {
        stop(paste("unhandled valuetype:", valuetype$valuetype))
      }

      cli::cli_h3("data_to_add")
      n <- nrow(data_trait)
      .col_or_na <- function(col, na_val) {
        if (col %in% colnames(data_trait)) data_trait[[col]] else rep(na_val, n)
      }
      data_to_add <-
        dplyr::tibble(
          id_table_liste_plots = data_trait$id_liste_plots,
          id_data_individuals  = data_trait$id_data_individuals,
          id_specimen          = data_trait$id_specimen,
          id_diconame          = data_trait$id_diconame,
          id_colnam            = data_trait$id_colnam,
          id_sub_plots         = data_trait$id_sub_plots,
          country              = data_trait$country,
          decimallatitude      = data_trait$decimallatitude,
          decimallongitude     = data_trait$decimallongitude,
          elevation            = .col_or_na("elevation",          NA_real_),
          verbatimlocality     = .col_or_na("verbatimlocality",   NA_character_),
          basisofrecord        = data_trait$basisofrecord,
          references           = .col_or_na("reference",          NA_character_),
          year                 = .col_or_na("year",               NA_integer_),
          month                = .col_or_na("month",              NA_integer_),
          day                  = .col_or_na("day",                NA_integer_),
          measurementremarks   = .col_or_na("measurementremarks", NA_character_),
          measurementmethod    = .col_or_na("measurementmethod",  NA_character_),
          traitid              = data_trait$id_trait,
          traitvalue           = if (val_type == "numeric")   as.numeric(data_trait$trait) else rep(NA_real_,      n),
          traitvalue_char      = if (val_type == "character") as.character(data_trait$trait) else rep(NA_character_, n),
          original_tax_name    = .col_or_na("original_tax_name",  NA_character_),
          original_plot_name   = .col_or_na("original_plot_name", NA_character_),
          original_specimen    = .col_or_na("original_specimen",  NA_character_),
          issue                = data_trait$issue,
          date_modif_d         = data_trait$date_modif_d,
          date_modif_m         = data_trait$date_modif_m,
          date_modif_y         = data_trait$date_modif_y
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
      

      # Enhanced preview showing both measurements and features
      cli::cli_h2("Preview of data to add:")
      cli::cli_h3("Trait measurements: {nrow(data_to_add)} rows for trait '{trait}'")
      print(data_to_add)

      if (!is.null(features_field)) {
        cli::cli_h3("Features to add: {length(features_field)} feature type(s)")
        cli::cli_ul(features_field)
      }

      # Single consolidated prompt for measurements and features
      prompt_msg <- if (!is.null(features_field)) {
        "Confirm add trait measurements and their features?"
      } else {
        "Confirm add trait measurements?"
      }
      response <- choose_prompt(message = prompt_msg)

      if (add_data & response) {

        # Insert measurements with RETURNING to get IDs
        trait_ids <- .execute_trait_insert_with_returning(data_to_add, actual_con)
        any_inserted <- TRUE

        cli::cli_alert_success("Inserted {nrow(data_to_add)} measurement(s) for '{trait}'")

        # If features provided, add them in the same transaction
        if (!is.null(features_field)) {

          # Join by row position: RETURNING preserves insert order, so
          # trait_ids[i, ] corresponds to data_trait[i, ] unambiguously.
          # Joining on id_data_individuals would fan out when the same
          # individual has multiple measurements (allow_multiple_value = TRUE).
          data_feats <-
            data_trait %>%
            dplyr::select(dplyr::all_of(features_field)) %>%
            dplyr::mutate(id_trait_measures = trait_ids$id_trait_measures) %>%
            dplyr::select(id_trait_measures, dplyr::all_of(features_field))

          .add_trait_features_internal(
            new_data            = data_feats,
            id_trait_measures   = "id_trait_measures",
            features            = features_field,
            con                 = actual_con,
            allow_multiple_value = allow_multiple_value,
            interactive         = FALSE
          )

          cli::cli_alert_success("Inserted features for {nrow(data_feats)} measurement(s)")
        }

      } else {
        cli::cli_alert_info("Skipped '{trait}' (user declined or add_data = FALSE)")
      }
      
    } else {
      cli::cli_alert_info("No data to add for '{trait}' - all values are 0 or NA")
    }
  } # end trait loop

  # Single commit covering all traits and their features
  if (add_data && any_inserted) {
    DBI::dbCommit(actual_con)
    transaction_active <<- FALSE
    cli::cli_alert_success("Transaction committed — all traits and features inserted successfully")
  } else if (add_data) {
    DBI::dbRollback(actual_con)
    transaction_active <<- FALSE
    cli::cli_alert_info("Transaction rolled back — no data was confirmed for insertion")
  }

  }, error = function(e) {
    tryCatch({
      DBI::dbRollback(actual_con)
      transaction_active <<- FALSE
    }, error = function(e2) cli::cli_alert_danger("Rollback failed: {e2$message}")
    )
    # Distinguish connection loss from logic errors for a clearer message
    if (grepl("terminating connection|connection.*lost|server.*shut|EOF|SSL SYSCALL",
              e$message, ignore.case = TRUE)) {
      cli::cli_alert_danger(paste(
        "Database connection was lost during import. No data was committed.",
        "Please reconnect with call.mydb() and retry."
      ))
    } else {
      cli::cli_alert_danger("Transaction rolled back due to error: {e$message}")
    }
    stop(e)
  }) # end tryCatch

  if (exists("unlinked_individuals"))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))

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
    dplyr::mutate(data_modif_d = lubridate::day(Sys.Date()),
                  data_modif_m = lubridate::month(Sys.Date()),
                  data_modif_y = lubridate::year(Sys.Date()))
  
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
                      new_comments = NULL,
                      new_category = NULL,
                      con = NULL,
                      interactive = TRUE) {
  
  if (is.null(con)) {
    mydb <- call.mydb()
  } else {
    mydb <- con
  }

  # Handle pool connections
  actual_con <- if (inherits(mydb, "Pool")) {
    pool::poolCheckout(mydb)
  } else {
    mydb
  }

  on.exit({
    if (inherits(mydb, "Pool") && !is.null(actual_con)) {
      pool::poolReturn(actual_con)
    }
  }, add = TRUE)

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
    if (!is.null(new_maxallowedvalue) && !is.numeric(new_maxallowedvalue) &
        !is.integer(new_maxallowedvalue))
      stop("valuetype numeric of integer and max value not of this type")
  if (new_valuetype == "numeric" | new_valuetype == "integer")
    if (!is.null(new_minallowedvalue) && !is.numeric(new_minallowedvalue) &
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
    comments = ifelse(is.null(new_comments), NA, new_comments),
    category = ifelse(is.null(new_category), "Other", new_category)
  )

  if (interactive) {
    print(new_data_renamed)
    Q <- choose_prompt(message = "confirm adding this trait ?")
  } else {
    Q <- TRUE
  }

  if(Q) {
    DBI::dbWriteTable(actual_con, "traitlist", new_data_renamed, append = TRUE, row.names = FALSE)
    .invalidate_traitlist_cache()
  }

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
                                   interactive = TRUE,
                                   con = NULL) {

  # Connection management
  if (is.null(con)) con <- call.mydb()
  is_pool <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)

  for (i in 1:length(traits_field))
    if (!any(colnames(new_data) == traits_field[i]))
      stop(paste("traits_field provide not found in new_data", traits_field[i]))

  if (!is.null(features_field)) for (i in 1:length(features_field))
    if (!any(colnames(new_data) == features_field[i]))
      stop(paste("features_field provide not found in new_data", features_field[i]))

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
  }

  ## removing entries with NA values for traits
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(traits_field), ~ !is.na(.x)))

  if (nrow(new_data_renamed) == 0)
    stop("no values for selected trait(s)")

  ### Linking collectors names
  if(!is.null(collector)) {
    new_data_renamed <-
      .rename_data(dataset = new_data,
                   col_old = collector,
                   col_new = "colnam")

    new_data_renamed <-
      .link_table(
        data_stand = new_data_renamed,
        column_searched = "colnam",
        column_name = "colnam",
        id_field = "id_colnam",
        id_table_name = "id_table_colnam",
        db_connection = actual_con,
        table_name = "table_colnam"
      )
  } else {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(idcolnam = NA_real_)
  }

  # Helper for optional columns
  .optional_column <- function(df, col_name, default = NA) {
    if (col_name %in% names(df)) df[[col_name]] else rep(default, nrow(df))
  }

  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(traits_field))
  for (i in 1:length(traits_field)) {

    trait <- traits_field[i]
    if (!any(colnames(new_data_renamed) == trait))
      stop(paste("trait field not found", trait))

    data_trait <- new_data_renamed
    data_trait <- data_trait %>%
      dplyr::rename("trait" := dplyr::all_of(trait))

    data_trait <- data_trait %>%
      dplyr::filter(!is.na(trait))

    if (is.numeric(data_trait$trait) && any(data_trait$trait == 0, na.rm = TRUE)) {
      if (interactive) {
        add_0 <- choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
      } else {
        add_0 <- TRUE
      }

      if(!add_0)
        data_trait <- data_trait %>% dplyr::filter(trait != 0)
    }

    if(nrow(data_trait) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_trait <-
        .link_sp_trait(data_stand = data_trait, trait = trait, con = actual_con)

      queried_trait <-
        query_trait(id_trait = data_trait %>%
                      dplyr::distinct(id_trait) %>%
                      pull(),
                    con = actual_con)

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
          data_trait %>% filter(is.na(trait))
        }
      }

      if (valuetype$valuetype == "numeric")
        TypeValue <- "numeric"

      ### choosing kind of measures
      if (!any(colnames(data_trait) == "basisofrecord")) {
        if (!is.null(basisofrecord)) {
          data_trait <- data_trait %>%
            mutate(basisofrecord = rep(basisofrecord, nrow(.)))
        } else if (interactive) {
          cli::cli_h3("basis")
          choices <- dplyr::tibble(
            basis = c('LivingSpecimen', 'PreservedSpecimen', 'FossilSpecimen',
                      'literatureData', 'traitDatabase', 'expertKnowledge')
          )
          print(choices)
          selected_basisofrecord <- readline(prompt = "Choose basisofrecord : ")
          data_trait <- data_trait %>%
            mutate(basisofrecord = rep(choices$basis[as.numeric(selected_basisofrecord)], nrow(.)))
        } else {
          stop("basisofrecord must be provided in non-interactive mode")
        }
      }

      ### choosing measurementremarks if none
      if (!any(colnames(data_trait) == "measurementremarks")) {
        if (!is.null(measurementremarks) && nchar(trimws(measurementremarks)) > 0) {
          data_trait <- data_trait %>%
            mutate(measurementremarks = rep(measurementremarks, nrow(.)))
        } else if (interactive) {
          cli::cli_h3("remarks")
          selected_measurementremarks <- readline(prompt = "Add measurementremarks ? 'enter if none : ")
          if (selected_measurementremarks != "") {
            data_trait <- data_trait %>%
              mutate(measurementremarks = rep(selected_measurementremarks, nrow(.)))
          }
        }
      }

      ### checking if any duplicates in data to add
      if (data_trait %>% dplyr::distinct() %>% nrow() != nrow(data_trait)) {
        duplicates_lg <- duplicated(data_trait)
        cli::cli_alert_warning("Duplicates in new data for {trait} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        if (interactive) {
          cf_merge <- choose_prompt(message = "confirm merging duplicates?")
        } else {
          cf_merge <- TRUE
        }

        if (cf_merge) {
          data_trait <- data_trait %>% dplyr::distinct()
        } else{
          stop()
        }
      }

      cli::cli_h3(".add_modif_field")
      data_trait <- .add_modif_field(dataset = data_trait)

      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          idtax = data_trait$idtax,
          decimallatitude = .optional_column(data_trait, "decimallatitude"),
          decimallongitude = .optional_column(data_trait, "decimallongitude"),
          elevation = .optional_column(data_trait, "elevation"),
          verbatimlocality = .optional_column(data_trait, "verbatimlocality"),
          basisofrecord = data_trait$basisofrecord,
          reference = .optional_column(data_trait, "reference"),
          id_citation = .optional_column(data_trait, "id_citation"),
          year = .optional_column(data_trait, "year"),
          month = .optional_column(data_trait, "month"),
          day = .optional_column(data_trait, "day"),
          measurementremarks = .optional_column(data_trait, "measurementremarks"),
          measurementmethod = .optional_column(data_trait, "measurementmethod"),
          fk_id_trait = data_trait$id_trait,
          traitvalue = if (any(TypeValue == "numeric")) {
            suppressWarnings(as.numeric(data_trait$trait))
          } else {
            rep(NA_real_, nrow(data_trait))
          },
          traitvalue_char = if (any(TypeValue == "character")) {
            as.character(data_trait$trait)
          } else {
            rep(NA_character_, nrow(data_trait))
          },
          original_tax_name = .optional_column(data_trait, "original_tax_name"),
          issue = data_trait$issue,
          date_modif_d = data_trait$date_modif_d,
          date_modif_m = data_trait$date_modif_m,
          date_modif_y = data_trait$date_modif_y
        )

      list_add_data[[i]] <- data_to_add
      print(data_to_add)

      ### identify if measures are already within DB
      cli::cli_alert_info("Identifying if imported values are already in DB")

      trait_id <- unique(data_to_add$fk_id_trait)
      selected_data_traits <-
        data_to_add %>%
        dplyr::select(idtax, traitvalue_char, traitvalue, issue,
                      basisofrecord, fk_id_trait, measurementremarks)

      all_vals <-
        dplyr::tbl(actual_con, "taxa_traits_measures") %>%
        dplyr::select(idtax, traitvalue_char, traitvalue, issue,
                      basisofrecord, fk_id_trait, measurementremarks) %>%
        dplyr::filter(fk_id_trait %in% !!trait_id) %>%
        dplyr::collect()

      if (TypeValue == "numeric") {
        all_vals <- all_vals %>%
          dplyr::select(-traitvalue_char) %>%
          rename(trait = traitvalue)
        selected_data_traits <- selected_data_traits %>%
          dplyr::select(-traitvalue_char) %>%
          rename(trait = traitvalue)
      }

      if (TypeValue == "character") {
        all_vals <- all_vals %>%
          dplyr::select(-traitvalue) %>%
          rename(trait = traitvalue_char)
        selected_data_traits <- selected_data_traits %>%
          dplyr::select(-traitvalue) %>%
          rename(trait = traitvalue_char)
      }

      # Tag rows by source before binding so that within-file duplicates
      # (multiple rows for the same taxon/trait in the import file) are not
      # mistakenly counted as "already in DB".
      duplicated_rows <-
        dplyr::bind_rows(
          selected_data_traits %>% dplyr::filter(is.na(issue)) %>% dplyr::mutate(.src = "new"),
          all_vals            %>% dplyr::filter(is.na(issue)) %>% dplyr::mutate(.src = "db")
        ) %>%
        dplyr::group_by(idtax, fk_id_trait, trait, basisofrecord, measurementremarks) %>%
        dplyr::filter(any(.src == "new") & any(.src == "db")) %>%
        dplyr::filter(.src == "new") %>%
        dplyr::ungroup() %>%
        dplyr::select(-".src")

      if (nrow(duplicated_rows) == 0) {
        cli::cli_alert_success("No duplicates found in DB for {trait} ({nrow(data_to_add)} rows ready)")
      }

      if (nrow(duplicated_rows) > 0) {
        cli::cli_alert_danger("Some values are already in DB")
        print(duplicated_rows %>%
                dplyr::ungroup() %>%
                dplyr::select(idtax, fk_id_trait, basisofrecord))

        if (interactive) {
          exclud_yes <- choose_prompt(message = "Exclude duplicated rows ?")
        } else {
          exclud_yes <- TRUE
        }

        if (exclud_yes) {
          cli::cli_alert_danger("Excluding {nrow(duplicated_rows)} values because already in DB")
          data_to_add <- data_to_add %>%
            dplyr::filter(!idtax %in% duplicated_rows$idtax)
          data_trait <- data_trait %>%
            dplyr::filter(!idtax %in% duplicated_rows$idtax)
        }

        if(nrow(data_to_add) < 1) stop("no new values anymore to import after excluding duplicates")
      }

      if (ask_before_update && interactive) {
        response <- choose_prompt(message = "Confirm add these data to taxa_traits_measures table ?")
      } else {
        response <- TRUE
      }

      if(add_data & response) {
        # Use transaction for safety
        DBI::dbBegin(actual_con)
        tryCatch({
          # Use INSERT...RETURNING for reliable ID retrieval
          col_names_str <- paste(names(data_to_add), collapse = ", ")

          # Build values clause row by row with proper type handling
          values_list <- lapply(seq_len(nrow(data_to_add)), function(r) {
            vals <- sapply(names(data_to_add), function(col) {
              v <- data_to_add[[col]][r]
              if (is.na(v)) "NULL"
              else if (is.numeric(v)) as.character(v)
              else paste0("'", gsub("'", "''", as.character(v)), "'")
            })
            paste0("(", paste(vals, collapse = ", "), ")")
          })
          values_clause <- paste(values_list, collapse = ",\n")

          insert_sql <- sprintf(
            "INSERT INTO taxa_traits_measures (%s) VALUES %s RETURNING id_trait_measures, idtax",
            col_names_str, values_clause
          )
          returned_ids <- DBI::dbGetQuery(actual_con, insert_sql)

          cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")

          if (!is.null(features_field)) {
            data_feats <-
              data_trait %>%
              select(all_of(features_field), idtax) %>%
              mutate(id_trait_measures = returned_ids$id_trait_measures,
                     idtax = returned_ids$idtax)

            add_sp_traits_measures_features(
              new_data = data_feats,
              id_trait_measures = "id_trait_measures",
              features = features_field,
              add_data = TRUE,
              interactive = interactive,
              con = actual_con,
              in_transaction = TRUE
            )
          }

          DBI::dbCommit(actual_con)
        }, error = function(e) {
          tryCatch(DBI::dbRollback(actual_con), error = function(x) NULL)
          stop("Insert failed: ", e$message)
        })
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
                                            add_data = FALSE,
                                            interactive = TRUE,
                                            con = NULL,
                                            in_transaction = FALSE) {

  # Connection management
  if (is.null(con)) con <- call.mydb()
  is_pool <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)

  for (i in 1:length(features))
    if (!any(colnames(new_data) == features[i]))
      stop(paste("features field provide not found in new_data", features[i]))

  new_data_renamed <- new_data

  ## removing entries with NA values for traits
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(features), ~ !is.na(.x)))

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
      try_open_postgres_table(table = "taxa_traits_measures", con = actual_con) %>%
        dplyr::select(id_trait_measures) %>%
        dplyr::filter(id_trait_measures %in% !!unique(new_data_renamed$id_trait_measures)) %>%
        dplyr::collect() %>%
        dplyr::mutate(rrr = 1),
      by = c("id_trait_measures" = "id_trait_measures")
    )

  if (dplyr::filter(link_trait_measures, is.na(rrr)) %>%
      nrow() > 0) {
    print(dplyr::filter(link_trait_measures, is.na(rrr)))
    stop("provided trait_measures not found in taxa_traits_measures")
  }

  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(features))
  for (i in 1:length(features)) {

    feat <- features[i]
    if(!any(colnames(new_data_renamed) == feat))
      stop(paste("feat field not found", feat))

    data_feat <- new_data_renamed
    data_feat <- data_feat %>%
      dplyr::rename("trait" := dplyr::all_of(feat))

    data_feat <- data_feat %>%
      dplyr::filter(!is.na(trait))

    if(nrow(data_feat) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_feat <-
        .link_sp_trait(data_stand = data_feat, trait = feat, con = actual_con)

      ## see what type of value numeric of character
      vt_info <-
        data_feat %>%
        dplyr::distinct(id_trait) %>%
        dplyr::left_join(
          get_traitlist(actual_con)[, c("valuetype", "id_trait"), drop = FALSE],
          by = c("id_trait" = "id_trait")
        )

      vt <- vt_info$valuetype[1]
      cli::cli_alert_info("valuetype for '{feat}': {ifelse(is.na(vt), 'NA', vt)}")
      if (is.na(vt)) {
        cli::cli_alert_warning("No valuetype found for feature '{feat}', defaulting to 'numeric'")
        vt <- "numeric"
      }

      if(vt == "table_colnam") {
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
          db_connection = actual_con,
          table_name = "table_colnam"
        )

        data_feat <- add_col_sep
      }

      if (is.numeric(data_feat$trait) && any(data_feat$trait == 0, na.rm = TRUE)) {
        if (interactive) {
          add_0 <- choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
        } else {
          add_0 <- TRUE
        }

        if(!add_0)
          data_feat <- data_feat %>% dplyr::filter(trait != 0)
      }

      cli::cli_h3(".add_modif_field")
      data_feat <- .add_modif_field(dataset = data_feat)

      if (vt %in% c("ordinal", "character"))
        val_type <- "character"

      if (vt %in% c("numeric", "table_colnam"))
        val_type <- "numeric"

      if (vt == "integer")
        val_type <- "numeric"

      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          id_trait_measures = data_feat$id_trait_measures,
          id_trait = data_feat$id_trait,
          typevalue = ifelse(
            rep(val_type == "numeric", nrow(data_feat)),
            suppressWarnings(as.numeric(data_feat$trait)), NA_real_
          ),
          typevalue_char = ifelse(
            rep(val_type == "character", nrow(data_feat)),
            as.character(data_feat$trait), NA_character_
          ),
          date_modif_d = data_feat$date_modif_d,
          date_modif_m = data_feat$date_modif_m,
          date_modif_y = data_feat$date_modif_y
        )

      list_add_data[[i]] <- data_to_add
      print(data_to_add)

      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {
        duplicates_lg <- duplicated(data_to_add)
        cli::cli_alert_warning("Duplicates in new data for {feat} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        if (interactive) {
          cf_merge <- choose_prompt(message = "confirm merging duplicates?")
        } else {
          cf_merge <- TRUE
        }

        if (cf_merge) {
          data_to_add <- data_to_add %>% dplyr::distinct()
        } else {
          if (!allow_multiple_value) stop()
        }
      }

      if (interactive) {
        response <- choose_prompt(message = "Confirm add these data to taxa_traits_measures_feat table?")
      } else {
        response <- TRUE
      }

      if(add_data & response) {
        if (!in_transaction) DBI::dbBegin(actual_con)
        tryCatch({
          DBI::dbWriteTable(actual_con, "taxa_traits_measures_feat",
                            data_to_add, append = TRUE, row.names = FALSE)
          if (!in_transaction) DBI::dbCommit(actual_con)
          cli::cli_alert_success("Adding features data : {nrow(data_to_add)} values added for {feat}")
        }, error = function(e) {
          if (!in_transaction) tryCatch(DBI::dbRollback(actual_con), error = function(x) NULL)
          stop("Insert features failed: ", e$message)
        })
      }

    } else{
      cli::cli_alert_info("no added data for {feat} - no values different of 0")
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
                           new_comments = NULL,
                           con = NULL,
                           interactive = TRUE) {

  message("Note: add_trait_taxa() now writes to traitlist on the main database.")

  add_trait(
    new_trait = new_trait,
    new_relatedterm = new_relatedterm,
    new_valuetype = new_valuetype,
    new_maxallowedvalue = new_maxallowedvalue,
    new_minallowedvalue = new_minallowedvalue,
    new_traitdescription = new_traitdescription,
    new_factorlevels = new_factorlevels,
    new_expectedunit = new_expectedunit,
    new_comments = new_comments,
    con = con,
    interactive = interactive
  )
}


#' Add a trait to the trait list
#'
#' Add trait and associated descriptors to the traitlist table in the main database.
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
#' @param con Database connection. If NULL, uses call.mydb().
#' @param interactive logical whether to prompt user for confirmation
#'
#' @description
#' See https://terminologies.gfbio.org/terms/ets/pages/index.html for description of each field
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
                      new_comments = NULL,
                      new_category = NULL,
                      con = NULL,
                      interactive = TRUE) {

  if(is.null(new_trait)) stop("define new trait")
  if(is.null(new_valuetype)) stop("define new_valuetype")

  if (!any(new_valuetype == c('numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character')))
    stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', or 'character'")

  if(new_valuetype=="numeric" | new_valuetype=="integer") {
    if(!is.null(new_maxallowedvalue) && !is.numeric(new_maxallowedvalue) && !is.integer(new_maxallowedvalue))
      stop("valuetype numeric or integer and max value not of this type")
    if(!is.null(new_minallowedvalue) && !is.numeric(new_minallowedvalue) && !is.integer(new_minallowedvalue))
      stop("valuetype numeric or integer and min value not of this type")
  }

  if (is.null(con)) con <- call.mydb()
  is_pool <- inherits(con, "Pool")
  actual_con <- if (is_pool) pool::poolCheckout(con) else con
  on.exit(if (is_pool) pool::poolReturn(actual_con), add = TRUE)

  new_data_renamed <- tibble(trait = new_trait,
                             relatedterm = ifelse(is.null(new_relatedterm), NA, new_relatedterm),
                             valuetype = new_valuetype,
                             maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
                             minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
                             traitdescription = ifelse(is.null(new_traitdescription), NA, new_traitdescription),
                             factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
                             expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
                             comments = ifelse(is.null(new_comments), NA, new_comments),
                             category = ifelse(is.null(new_category), "Other", new_category))

  print(new_data_renamed)

  if (interactive) {
    Q <- choose_prompt(message = "confirm adding this trait ?")
  } else {
    Q <- TRUE
  }

  if(Q) DBI::dbWriteTable(actual_con, "traitlist", new_data_renamed, append = TRUE, row.names = FALSE)
}



# add_sp_trait_measures_features <- function(new_data,
#                                            id_trait_measures = "id_trait_measures",
#                                            features,
#                                            allow_multiple_value = FALSE,
#                                            add_data = FALSE) {
#   
#   for (i in 1:length(features))
#     if (!any(colnames(new_data) == features[i]))
#       stop(paste("features field provide not found in new_data", features[i]))
#   
#   new_data_renamed <- new_data
#   
#   ## removing entries with NA values for traits
#   new_data_renamed <-
#     new_data_renamed %>%
#     dplyr::filter_at(dplyr::vars(!!features), dplyr::any_vars(!is.na(.)))
#   
#   if (nrow(new_data_renamed) == 0)
#     stop("no values for selected features(s)")
#   
#   new_data_renamed <-
#     new_data_renamed %>%
#     mutate(id_new_data = 1:nrow(.))
#   
#   new_data_renamed <-
#     new_data_renamed %>%
#     rename(id_trait_measures := all_of(id_trait_measures))
#   
#   link_trait_measures <-
#     new_data_renamed %>%
#     dplyr::left_join(
#       try_open_postgres_table(table = "table_traits_measures", con = mydb_taxa) %>%
#         dplyr::select(id_trait_measures) %>%
#         dplyr::filter(id_trait_measures %in% !!unique(new_data_renamed$id_trait_measures)) %>%
#         dplyr::collect() %>%
#         dplyr::mutate(rrr = 1),
#       by = c("id_trait_measures" = "id_trait_measures")
#     )
#   
#   if (dplyr::filter(link_trait_measures, is.na(rrr)) %>%
#       nrow() > 0) {
#     print(dplyr::filter(link_trait_measures, is.na(rrr)))
#     stop("provided trait_measures not found in table_traits_measures")
#   }
#   
#   
#   ### preparing dataset to add for each trait
#   list_add_data <- vector('list', length(features))
#   for (i in 1:length(features)) {
#     
#     feat <- features[i]
#     if(!any(colnames(new_data_renamed) == feat))
#       stop(paste("feat field not found", feat))
#     
#     data_feat <-
#       new_data_renamed
#     
#     data_feat <-
#       data_feat %>%
#       dplyr::filter(!is.na(!!sym(feat)))
#     
#     if(nrow(data_feat) > 0) {
#       ### adding trait id and adding potential issues based on trait
#       data_feat <-
#         .link_sp_trait(data_stand = data_feat, trait = feat)
#       
#       ## see what type of value numeric of character
#       valuetype <-
#         data_feat %>%
#         dplyr::distinct(id_trait) %>%
#         dplyr::left_join(
#           dplyr::tbl(mydb, "traitlist") %>%
#             dplyr::select(valuetype, id_trait) %>%
#             dplyr::collect(),
#           by = c("id_trait" = "id_trait")
#         )
#       
#       if (valuetype$valuetype == "table_colnam") {
#         
#         add_col_sep <-
#           data_feat %>%
#           tidyr::separate_rows(trait, sep = ",") %>%
#           mutate(trait = stringr::str_squish(trait))
#         
#         add_col_sep <- .link_colnam(
#           data_stand = add_col_sep,
#           column_searched = "trait",
#           column_name = "colnam",
#           id_field = "trait",
#           id_table_name = "id_table_colnam",
#           db_connection = mydb,
#           table_name = "table_colnam"
#         )
#         
#         data_feat <-add_col_sep
#         
#       }
#       
#       if (any(data_feat$trait == 0)) {
#         
#         add_0 <- choose_prompt(message = "Some value are equal to 0. Do you want to add these values anyway ??")
#         
#         if(!add_0)
#           data_feat <-
#             data_feat %>%
#             dplyr::filter(trait != 0)
#         
#       }
#       
#       
#       
#       cli::cli_h3(".add_modif_field")
#       data_feat <-
#         .add_modif_field(dataset = data_feat)
#       
#       
#       if (valuetype$valuetype == "ordinal" |
#           valuetype$valuetype == "character")
#         val_type <- "character"
#       
#       if (valuetype$valuetype == "numeric" | valuetype$valuetype == "table_colnam")
#         val_type <- "numeric"
#       
#       if (valuetype$valuetype == "integer")
#         val_type <- "numeric"
#       
#       cli::cli_h3("data_to_add")
#       data_to_add <-
#         dplyr::tibble(
#           id_trait_measures = data_feat$id_trait_measures,
#           id_trait = data_feat$id_trait,
#           typevalue = ifelse(
#             rep(val_type == "numeric", nrow(data_feat)),
#             data_feat$trait,
#             NA
#           ),
#           typevalue_char = ifelse(
#             rep(val_type == "character", nrow(data_feat)),
#             as.character(data_feat$trait),
#             NA
#           ),
#           date_modif_d = data_feat$date_modif_d,
#           date_modif_m = data_feat$date_modif_m,
#           date_modif_y = data_feat$date_modif_y
#         )
#       
#       list_add_data[[i]] <-
#         data_to_add
#       
#       print(data_to_add)
#       
#       if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {
#         
#         duplicates_lg <- duplicated(data_to_add)
#         
#         cli::cli_alert_warning("Duplicates in new data for {feat} concerning {length(duplicates_lg[duplicates_lg])} id(s)")
#         
#         cf_merge <- 
#           choose_prompt(message = "confirm merging duplicates?")
#         
#         if (cf_merge) {
#           
#           # issues_dup <- data_to_add %>%
#           #   filter(id_trait_measures %in% data_to_add[duplicates_lg, "id_trait_measures"]) %>%
#           #   dplyr::select(issue, id_trait_measures)
#           
#           ## resetting issue
#           if(any(grepl("identical value", issues_dup$issue))) {
#             
#             issues_dup_modif_issue <-
#               issues_dup[grepl("identical value", issues_dup$issue),]
#             
#             data_to_add <-
#               data_to_add %>%
#               mutate(issue = replace(issue, id_trait_measures %in% issues_dup_modif_issue$id_trait_measures, NA))
#             
#           }
#           
#           data_to_add <- data_to_add %>% dplyr::distinct()
#         } else {
#           if (!allow_multiple_value) stop()
#         }
#         
#       }
#       
#       response <-
#         choose_prompt(message = "Confirm add these data to table_traits_measures_feat table?")
#       
#       if(add_data & response) {
#         
#         DBI::dbWriteTable(mydb_taxa, "table_traits_measures_feat",
#                           data_to_add,
#                           append = TRUE,
#                           row.names = FALSE)
#         
#         cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")
#       }
#       
#     } else{
#       
#       cli::cli_alert_info("no added data for {trait} - no values different of 0")
#       
#     }
#   }
#   
#   
#   return(list(list_features_add = list_add_data))
#   
# }




