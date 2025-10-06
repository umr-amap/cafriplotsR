

#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param column_searched string vector
#' @param column_name name of the column that will store the id
#' @param id_field string name of the column of the output that will contain the id
#' @param id_table_name string name of the database table that contain the id
#' @param db_connection PqConnection  connection of the database
#' @param table_name string name of the table in the database to search into
#' @param keep_columns string vector of the columns in the database table to keep in the output, by default NULL
#'
#' @examples
#' # .link_table(data_stand = data_stand, column_searched = "method", column_name = "method", id_field = "id_method", db_connection = mydb, table_name = "methodslist")
#'
#'
#' @export
.link_table <- function(data_stand,
                        column_searched,
                        column_name,
                        id_field,
                        id_table_name,
                        db_connection = mydb,
                        table_name,
                        keep_columns = NULL,
                        keep_original_value = FALSE) {
  
  mydb <- call.mydb()

  var <- rlang::enquo(column_searched)
  var_new <- "name"

  data_stand <-
    data_stand %>%
    dplyr::rename(!!var_new := dplyr::all_of(!!var))

  all_names_ <-
    dplyr::distinct(data_stand, name) %>% filter(name != "")

  all_names <-
    try_open_postgres_table(table = table_name, con = db_connection) %>%
    # dplyr::tbl(mydb, "table_countries") %>%
    dplyr::collect() %>%
    select(!!sym(id_table_name), !!sym(column_name), all_of(keep_columns))

  col_name <- rlang::as_name(column_name)
  all_names_ <-
    all_names_ %>%
       dplyr::left_join(all_names, by = c("name" = col_name))

  id_ <- data_stand %>%
    left_join(all_names_ %>% select(name, {{id_table_name}}),
              by = c("name" = "name")) %>%
    pull({{id_table_name}})

  all_names_no_match <-
    all_names_ %>%
    filter(is.na(!!rlang::sym(id_table_name)))

  if (nrow(all_names_no_match) > 0) {

    for (i in 1:nrow(all_names_no_match)) {

      sorted_matches <-
        .find_cat(
          value_to_search = dplyr::pull(all_names_no_match, name)[i],
          compared_table = all_names,
          column_name = column_name,
          prompt_message = "Choose feature (type 'G' for pattern searching, 0 is no match), 'enter' for scrolling the list: "
        )

      if (sorted_matches$selected_name != 0) {

        selected_name_id <-
          sorted_matches$sorted_matches %>%
          slice(sorted_matches$selected_name) %>%
          pull({{id_table_name}})

      } else {

        selected_name_id <-
          0

      }

      id_[data_stand$name == dplyr::pull(all_names_no_match, name)[i]] <-
        selected_name_id

    }

  }

  data_stand <-
    data_stand %>%
    dplyr::mutate(id_field_ = id_)

  var <- "id_field_"
  var_new <- id_field
  data_stand <-
    data_stand %>%
    dplyr::rename(!!var_new := dplyr::all_of(!!var))

  if (keep_original_value) {

    var <- "name"
    var_new <- paste0("original_",column_name)

    data_stand <-
      data_stand %>%
      dplyr::rename(!!var_new := !!var)

  } else {
    data_stand <-
      data_stand %>% dplyr::select(-name)

  }


  if (!is.null(keep_columns)) {
    data_stand <-
      join_help_function(data_stand,
                         all_names,
                         id_field,
                         id_table_name,
                         keep_columns = keep_columns)

  }

  # mutate(id_country = id_)

  return(data_stand)
}



#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param trait string vector
#' @param issues string vector
#'
#' @export
.link_trait <- function(data_stand, trait, column_name = "trait", issues = NULL) {

  mydb <- call.mydb()
  
  trait_newnames <- "trait"

  data_stand <- data_stand %>%
    dplyr::rename_with(.cols = dplyr::all_of(trait),
                       .fn = ~ trait_newnames)

  all_traits <-
    dplyr::tbl(mydb, "traitlist") %>%
    dplyr::collect()


  selected_name_res <- .find_cat(value_to_search = trait,
                             compared_table = all_traits,
                             column_name = "trait")

  # sorted_matches <-
  #   .find_similar_string(input = trait,
  #                        compared_table = all_traits,
  #                        column_name = column_name)
  # print(trait)
  #
  # selected_name <- ""
  # slide <- 0
  # while (selected_name == "") {
  #   slide <- slide + 1
  #   sorted_matches %>%
  #     tibble::add_column(ID = seq(1, nrow(.), 1)) %>%
  #     dplyr::select(ID, trait, traitdescription) %>%
  #     dplyr::slice((1 + (slide - 1) * 10):((slide) * 10)) %>%
  #     print()
  #   selected_name <-
  #     readline(prompt = "Choose ID whose trait fit (if none enter 0): ")
  #   if (slide * 10 > nrow(sorted_matches))
  #     slide <- 0
  # }

  selected_name <- selected_name_res$selected_name

  if(is.na(selected_name))
    stop("Provide integer value for standardizing trait name")

  selected_trait_id <-
    selected_name_res$sorted_matches %>%
    dplyr::slice(selected_name) %>%
    dplyr::select(id_trait) %>%
    dplyr::pull()

  select_trait_features <-
    selected_name_res$sorted_matches  %>%
    dplyr::slice(selected_name)

  if (select_trait_features$valuetype == "numeric") {

    if (!is.numeric(data_stand$trait)) {

      cli::cli_alert_warning("Expected numeric values but some are not")

      data_stand %>%
        mutate(trait = as.numeric(trait)) %>%
        filter(is.na(trait)) %>%
        print()

      cli::cli_alert_warning("Removing non numeric values")

      data_stand <-
        data_stand %>%
        mutate(trait = as.numeric(trait)) %>%
        filter(!is.na(trait))

    }
  }


  if (is.null(issues)) issues <- vector(mode = "character", length = nrow(data_stand))

  if (select_trait_features$valuetype == "numeric") {
    if (any(data_stand$trait < select_trait_features$minallowedvalue)) {
      warning(
        paste(
          trait,
          "values lower than minallowedvalue for",
          trait,
          "for",
          sum(data_stand$trait < select_trait_features$minallowedvalue),
          "entries"
        )
      )
      issues[data_stand$trait < select_trait_features$minallowedvalue] <-
        paste(select_trait_features$trait, "lower than minallowedvalue")
    }

    if (any(data_stand$trait > select_trait_features$maxallowedvalue)) {
      warning(
        paste(
          trait,
          "values higher than maxallowedvalue for",
          trait,
          "for",
          sum(data_stand$trait > select_trait_features$maxallowedvalue),
          "entries"
        )
      )
      issues[data_stand$trait > select_trait_features$maxallowedvalue] <-
        paste(select_trait_features$trait,
              "higher than maxallowedvalue")
    }
  }

  issues[issues==""] <- NA

  data_stand <-
    data_stand %>%
    mutate(id_trait = rep(selected_trait_id, nrow(.)))

  data_stand <-
    data_stand %>%
    mutate(issue = issues)

  return(data_stand)
}


#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param subplotype string vector
#'
#' @export
.link_subplotype <- function(data_stand, subplotype) {

  subplotype_newnames <- "subplotype"
  
  data_stand <- data_stand %>%
    dplyr::rename_with(.cols = dplyr::all_of(subplotype),
                       .fn = ~ subplotype_newnames)
  
  all_subplotype <-
    try_open_postgres_table(table = "subplotype_list", con = mydb) %>%
    dplyr::collect()
  
  
  sorted_matches <- .find_cat(value_to_search = subplotype,
                                 compared_table = all_subplotype,
                                 column_name = "type",
                                 prompt_message = "Choose subplot feature (G for pattern searching): ")
  
  # sorted_matches <-
  #   .find_cat(
  #     value_to_search = subplotype,
  #     compared_table = all_subplotype,
  #     column_name = "type",
  #     prompt_message = "Choose subplot feature (G for pattern searching): "
  #   )

  selected_name <- as.integer(sorted_matches$selected_name)

  if(is.na(selected_name))
    stop("Provide integer value for standardizing subplotype name")

  selected_type_id <-
    sorted_matches$sorted_matches %>%
    dplyr::slice(selected_name) %>%
    dplyr::select(id_subplotype) %>%
    dplyr::pull()

  select_type_features <-
    sorted_matches$sorted_matches %>%
    dplyr::slice(selected_name)

  if(select_type_features$valuetype == "numeric") {
    if(any(is.na(as.numeric(data_stand$subplotype)))) {
      warning("Numeric value expected but some are not")
      print(data_stand[which(is.na(as.numeric(data_stand$subplotype))),])
    }

    data_stand$subplotype <-
      as.numeric(data_stand$subplotype)
  }

  issues <- vector(mode = "character", length = nrow(data_stand))
  if(select_type_features$valuetype == "numeric") {
    if(any(data_stand$subplotype[!is.na(data_stand$subplotype)] < select_type_features$minallowedvalue)) {
      warning(paste(subplotype, "values lower than minallowedvalue for", subplottype, "for",
                    sum(data_stand$subplottype < select_type_features$minallowedvalue), "entries"))
      issues[data_stand$subplotype < select_type_features$minallowedvalue] <-
        paste(subplotype, "lower than minallowedvalue")
    }
  }

  if (select_type_features$valuetype == "numeric") {
    if (any(data_stand$subplotype[!is.na(data_stand$subplotype)] > select_type_features$maxallowedvalue)) {
      warning(paste(subplottype, "values higher than maxallowedvalue for", subplotype, "for",
                    sum(data_stand$subplotype > select_type_features$maxallowedvalue), "entries"))
      issues[data_stand$subplottype > select_type_features$maxallowedvalue] <-
        paste(subplotype, "higher than maxallowedvalue")
    }
  }

  issues[issues == ""] <- NA

  data_stand <-
    data_stand %>%
    dplyr::mutate(id_subplottype = rep(selected_type_id, nrow(.)))

  data_stand <-
    data_stand %>%
    dplyr::mutate(issue = issues)

  return(data_stand)
}



#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param trait string vector
#'
#' @export
.link_sp_trait <- function(data_stand, trait) {

  all_traits <-
    try_open_postgres_table(table = "table_traits", con = mydb_taxa) %>%
    collect()

  selected_name <- .find_cat(value_to_search = trait,
                             compared_table = all_traits,
                             column_name = "trait")

  sorted_matches <- selected_name$sorted_matches
  selected_name <- as.integer(selected_name$selected_name)

  if(is.na(selected_name))
    stop("Provide integer value for standardizing trait name")

  selected_trait_id <-
    sorted_matches %>%
    dplyr::slice(selected_name) %>%
    dplyr::select(id_trait) %>%
    dplyr::pull()

  select_trait_features <-
    sorted_matches %>%
    dplyr::slice(selected_name)

  issues <- vector(mode = "character", length = nrow(data_stand))
  if(select_trait_features$valuetype == "numeric") {
    if(any(data_stand$trait<select_trait_features$minallowedvalue)) {
      warning(paste(trait, "values lower than minallowedvalue for", trait, "for",
                    sum(data_stand$trait<select_trait_features$minallowedvalue), "entries"))
      issues[data_stand$trait<select_trait_features$minallowedvalue] <-
        paste(select_trait_features$trait, "lower than minallowedvalue")
    }

    if(any(data_stand$trait>select_trait_features$maxallowedvalue)) {
      warning(paste(trait, "values higher than maxallowedvalue for", trait, "for",
                    sum(data_stand$trait>select_trait_features$maxallowedvalue), "entries"))
      issues[data_stand$trait>select_trait_features$maxallowedvalue] <-
        paste(select_trait_features$trait, "higher than maxallowedvalue")
    }
  }

  issues[issues == ""] <- NA

  data_stand <-
    data_stand %>%
    tibble::add_column(id_trait = rep(selected_trait_id, nrow(.)))

  data_stand <-
    data_stand %>%
    tibble::add_column(issue = issues)

  return(data_stand)
}


#' Internal function
#'
#' Semi automatic matching with a table for comparison
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param value_to_search string vector of one element
#' @param compared_table tibble with one column where the value should be compared
#' @param column_name string name of the column of compared_table
#'
#' @export
.find_cat <- function(value_to_search, compared_table, column_name, prompt_message = "Choose") {

  print(value_to_search)

  compared_table <- .rename_data(dataset = compared_table,
                                 col_old = column_name,
                                 col_new = "comp_value")

  compared_table <-
    compared_table %>%
    mutate(perfect_match = comp_value == value_to_search)


  if(any(compared_table$perfect_match)) {

    sorted_matches <-
      compared_table

    selected_name <- which(compared_table$perfect_match)

  } else {

    selected_name <- "S"
    slide <- 0
    while(any(selected_name == c("", "G", "S"))) {

      slide <- slide + 1

      if(any(selected_name == c("S"))) {
        slide = 1
        sorted_matches <-
          .find_similar_string(input = value_to_search,
                               compared_table = compared_table,
                               column_name = "comp_value")
      }


      if (any(selected_name == c("G"))) {

        # var <- rlang::parse_expr(rlang::quo_name(rlang::enquo(column_name)))

        slide = 1
        grep_name <-
          readline(prompt = "Which string to look for:")
        sorted_matches <-
          compared_table %>%
          filter(grepl(grep_name, comp_value))

      }

      if(nrow(sorted_matches) > 0) {

        sel_loc <-
          sorted_matches %>%
          dplyr::mutate(ID = seq(1, nrow(.), 1)) %>%
          dplyr::relocate(ID, .before = comp_value) %>%
          dplyr::slice((1 + (slide - 1) * 10):((slide) * 10))

      } else {

        sel_loc <-
          sorted_matches

      }


      # print(sel_loc)

      sel_loc_html <-
        sel_loc %>%
        kableExtra::kable(format = "html", escape = F) %>%
        kableExtra::kable_styling("striped", full_width = F)

      print(sel_loc_html)

      print(value_to_search)

      selected_name <-
        readline(prompt = prompt_message)

      if (slide * 10 > nrow(sorted_matches))
        slide <- 0
    }

    selected_name <- as.integer(selected_name)

  }



  return(list(selected_name = selected_name,
              sorted_matches = sorted_matches))

}



.link_colnam <- function(data_stand,
                         column_searched,
                         column_name = "colnam",
                         id_field = "id_colnam",
                         id_table_name = "id_table_colnam",
                         db_connection = mydb,
                         table_name = "table_colnam") {

  mydb <- call.mydb()
  
  data_stand <-
    .link_table(
      data_stand = data_stand,
      column_searched = column_searched,
      column_name = column_name,
      id_field = id_field,
      id_table_name = id_table_name,
      db_connection = db_connection,
      table_name = table_name,
      keep_original_value = TRUE
    )

  original_ <- paste0("original_", column_name)

  quo_var <- rlang::parse_expr(rlang::as_name(rlang::enquo(id_field)))

  missing_colnams <- data_stand %>%
    dplyr::select(all_of(c(id_field, original_))) %>%
    filter(!!quo_var == 0)

  if (nrow(missing_colnams) > 0) {

    original_enquo <- rlang::parse_expr(rlang::as_name(enquo(original_)))
    missing_colnams_unique <- missing_colnams %>% distinct(!!original_enquo)
    for (i in 1:nrow(missing_colnams_unique)) {

      print(missing_colnams_unique$original_colnam[i])

      add <- 
        choose_prompt(message = "Add a new name?")
      

      if(add) {
        new_colname <-
          readline(prompt="Provide a new collector name following same format: ")

        new_family_name <-
          readline(prompt="Provide a new family_name name following same format: ")

        new_surname <-
          readline(prompt="Provide a new surname name following same format: ")

        new_nationality <-
          readline(prompt="Provide a nationality following same format: ")

        new_rec <- tibble::tibble(
          colnam = new_colname,
          family_name = new_family_name,
          surname = new_surname,
          nationality = new_nationality
        )

        DBI::dbWriteTable(mydb, "table_colnam", new_rec, append = TRUE,
                          row.names = FALSE)

        selected_name_id <-
          dplyr::tbl(mydb, "table_colnam") %>%
          dplyr::filter(colnam == new_colname) %>%
          dplyr::select(id_table_colnam) %>%
          dplyr::collect() %>%
          dplyr::pull()

        data_stand <-
          data_stand %>%
          mutate(!!sym(id_field) := replace(!!sym(id_field),
                                     original_colnam == missing_colnams_unique$original_colnam[i],
                                     selected_name_id))

      }

    }

  }

  return(data_stand)
}







#' Internal function
#'
#' Looking for similar name
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param input string vector of one value containing the string to compare
#' @param compared_table tibble including one column containing the different strings to which input should be compared
#' @param column_name string the column name of compared_table containing the compared values
.find_similar_string <- function(input, compared_table, column_name){
  dist. <-
    RecordLinkage::levenshteinSim(tolower(input),
                                  tolower(compared_table %>%
                                            dplyr::select(!!column_name) %>%
                                            dplyr::pull()))

  arranged_values <-
    compared_table %>%
    tibble::add_column(dist = dist.) %>%
    dplyr::arrange(dplyr::desc(dist))

  return(arranged_values)
}





join_help_function <- function(df1, df2, col1, col2, keep_columns) {
  helper <- col2
  names(helper) <- col1
  df2 <-
    df2 %>% dplyr::select(dplyr::all_of(c(keep_columns, col2)))

  output_dataframe <- dplyr::left_join(df1, df2, by = helper)

  return(output_dataframe)

}



#' Internal function
#'
#' Compare values between of given columns and identify different values based on id matches
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble contain values to compare and id for matching
#' @param col_new string vector containing column names of dataset
#' @param id_col_nbr string vector
#' @param type_data string indicate which table of database is targetted. e.g. 'individuals'
#'
#' @export
.find_ids <- function(dataset, col_new, id_col_nbr, type_data) {
  
  ids_new_data <-
    dataset %>%
    dplyr::select(!!col_new[id_col_nbr]) %>%
    dplyr::pull()
  
  if(type_data=="taxa")
    corresponding_data <-
      dplyr::tbl(mydb_taxa, "table_taxa")
  
  if(type_data=="individuals")
    corresponding_data <-
      dplyr::tbl(mydb, "data_individuals")
  
  if(type_data == "trait")
    corresponding_data <-
      dplyr::tbl(mydb_taxa, "table_traits")
  
  if(type_data == "sp_trait_measures")
    corresponding_data <-
      dplyr::tbl(mydb, "table_traits_measures")
  
  if(type_data == "plot_data")
    corresponding_data <-
      dplyr::tbl(mydb, "data_liste_plots")
  
  if(type_data == "data_liste_sub_plots")
    corresponding_data <-
      dplyr::tbl(mydb, "data_liste_sub_plots")
  
  if(type_data == "specimens")
    corresponding_data <-
      dplyr::tbl(mydb, "specimens")
  
  if(type_data == "methodslist")
    corresponding_data <-
      dplyr::tbl(mydb, "methodslist")  
  
  if(type_data == "data_link_specimens")
    corresponding_data <-
      dplyr::tbl(mydb, "data_link_specimens")
  
  if(type_data == "trait_measures") {
    # all_data <-
    #   dplyr::tbl(mydb, "data_traits_measures")
    
    if(col_new[id_col_nbr] %in% c("id_n", "id_old")) {
      
      corresponding_data <-
        .get_trait_individuals_values(traits = col_new[-id_col_nbr])
      
      corresponding_data <- corresponding_data[[1]]
    }
    
    if(col_new[id_col_nbr] %in% c("id_trait_measures")) {
      
      # new_name <- col_new[-id_col_nbr]
      
      corresponding_data <-
        dplyr::tbl(mydb, "data_traits_measures")
      # %>%
      #   dplyr::select(id_trait_measures, traitvalue, issue, measurementremarks)
      # %>%
      # dplyr::collect() %>%
      # rename(!!new_name := traitvalue)
      
    }
  }
  
  id <- "id"
  
  corresponding_data_full <-
    corresponding_data %>%
    dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id) %>%
    dplyr::filter(id %in% ids_new_data)
  
  corresponding_data <-
    corresponding_data %>%
    dplyr::select(dplyr::all_of(col_new)) %>%
    dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id) %>%
    dplyr::filter(id %in% ids_new_data) %>%
    dplyr::collect()
  
  all_tb_update <- vector('list', length(col_new[-id_col_nbr]))
  for (i in 1:length(col_new[-id_col_nbr])) {
    cat(" ", col_new[-id_col_nbr][i])
    # var <- enquo(col_names_corresp[-id_col][i])
    
    var_ <- col_new[-id_col_nbr][i]
    
    var <- rlang::enquo(var_)
    
    var_new <- paste0(col_new[-id_col_nbr][i], "_new")
    var_old <- paste0(col_new[-id_col_nbr][i], "_old")
    id <- col_new[id_col_nbr]
    var_id <- rlang::enquo(id)
    
    quo_var <- rlang::quo_name(rlang::enquo(id))
    
    select_col_new <-
      dplyr::select(dataset, !!var_id, !!var) %>%
      dplyr::rename(!!var_new := !!var)
    
    id <- "id"
    select_col_new <-
      select_col_new %>%
      dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id)
    
    select_col_old <-
      dplyr::select(corresponding_data, "id", !!var) %>%
      dplyr::rename(!!var_old := !!var)
    
    matches <-
      dplyr::left_join(select_col_new, select_col_old, by = c("id"="id"))
    
    matches <- replace_NA(df = matches)
    
    
    # matches[,2] <-
    #   replace_NA(vec = matches[,2])
    # matches[,3] <-
    #   replace_NA(vec = matches[,3])
    
    # matches[is.na(matches[,2]), 2] <- -9999
    # matches[is.na(matches[,3]), 3] <- -9999
    
    quo_var <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_new)))
    quo_var_old <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_old)))
    
    matches <-
      matches %>%
      dplyr::filter(!!quo_var != !!quo_var_old)
    
    if (nrow(matches) > 0) {
      
      matches[, 2] <- replace_NA(df = matches[,2], inv = T)
      matches[, 3] <- replace_NA(df = matches[,3], inv = T)
      
    }
    
    all_tb_update[[i]] <- matches
    names(all_tb_update)[i] <- col_new[-id_col_nbr][i]
  }
  
  return(list(corresponding_data_full, all_tb_update))
}


