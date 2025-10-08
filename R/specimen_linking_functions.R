# Specimen Linking Functions
#
# This file contains functions for linking individual trees to herbarium specimens.
# These functions manage the relationships between field observations (individuals)
# and herbarium collections (specimens) in the database.
#
# Main functions:
# - .add_link_specimens(): Add links between individuals and herbarium specimens
# - get_ref_specimen_ind(): Find reference specimen for individuals by collector
#
# Dependencies: DBI, dplyr, cli

#' Add link between specimen and individual
#'
#' Generate link between individual and specimens
#'
#' @return A tibble of all subplots
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param new_data tibble fuzzy person name to look for
#' @param col_names_select a vector of string that select columns of new_data to consider, it must be 3 columns
#' @param col_names_corresp a vector of string of same length of col_names_select, should not be changed
#' @param launch_adding_data logical, if TRUE links are added, by default it is FALSE for security
#'
#' @export
.add_link_specimens <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = c("id_specimen", "id_n", "type"),
                                launch_adding_data = FALSE) {

  if (is.null(col_names_select))
    col_names_select <- names(new_data)

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)
  
  if (!any(names(new_data_renamed) == "id_n"))
    new_data_renamed <- 
    new_data_renamed %>% 
    mutate(id_n = NA_integer_)
  
  if (!any(names(new_data_renamed) == "id_liste_plots"))
    new_data_renamed <- 
    new_data_renamed %>% 
    mutate(id_liste_plots = NA_integer_)
  
  message(paste0("Number of new links: ", nrow(new_data_renamed)))

  message(paste0("adding link for: ", nrow(distinct(new_data_renamed, id_specimen)), " different specimens"))

  cli::cli_alert_info("Prepare to add links for: {nrow(distinct(new_data_renamed, id_specimen))} different specimens")

  check_dup <-
    tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n, id_specimen, id_liste_plots) %>%
    collect() %>%
    bind_rows(new_data_renamed %>%
                dplyr::select(id_n, id_specimen, id_liste_plots)) %>%
    group_by(id_n, id_specimen, id_liste_plots) %>%
    count() %>%
    filter(n > 1) %>%
    filter(id_n %in% unique(new_data_renamed$id_n)) %>%
    ungroup()

  if(nrow(check_dup) > 0) {

    print(check_dup)
    message("some link to be added are already in the database")
    message("Excluding existing link from the new data")

    new_data_renamed <-
      new_data_renamed %>%
      filter(!id_n %in% check_dup$id_n)

    message(paste0("New links to be added are now: ", nrow(new_data_renamed)))

  }

  data_to_add <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  print(data_to_add)

  if(launch_adding_data) {


    DBI::dbWriteTable(mydb, "data_link_specimens",
                      data_to_add, append = TRUE, row.names = FALSE)
    
    
    cli::cli_alert_success("Added links : {nrow(data_to_add)} rows to link table")
  }
}




#' Find unlinked individual
#'
#' Extract individuals for which no links exist with herbarium specimens
#'
#' @return list
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param ids vector of id_n of individuals for which we want to explore links
#'
#'
#' @export
get_ref_specimen_ind <- function(collector = NULL, ids = NULL) {

  mydb <- call.mydb()
  
  if (!is.null(collector)) {

    # collector <-

    collector <-
      .link_colnam(
      data_stand = tibble(colnam = collector),
      column_searched = "colnam",
      column_name = "colnam",
      id_field = "id_colnam",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )

  }

  all_id_individuals_links <-
    tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n) %>%
    distinct(id_n) %>%
    dplyr::collect()

  if (!is.null(ids))
    all_herb_individuals <-
    tbl(mydb, "data_individuals") %>%
    dplyr::select(id_n, id_specimen, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type) %>%
    dplyr::filter(!id_n %in% !!all_id_individuals_links$id_n) %>%
    dplyr::filter(id_n %in% !!ids) %>%
    dplyr::collect()

  if (is.null(ids))
    all_herb_individuals <-
    tbl(mydb, "data_individuals") %>%
    dplyr::select(id_n, id_specimen, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type) %>%
    dplyr::filter(!id_n %in% !!all_id_individuals_links$id_n) %>%
    collect()

  ## all individual with a link to specimen not linked to specimen table
  all_herb_not_linked <-
    all_herb_individuals %>%
    filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char))

  ## getting number of specimen
  all_herb_not_linked <-
    all_herb_not_linked %>%
    filter(!is.na(herbarium_nbe_char)) %>%
    mutate(herbarium_nbe_char = stringr::str_replace(string = herbarium_nbe_char,
                                                     pattern = "-",
                                                     replacement = " ")) %>%
    mutate(herbarium_nbe_char = stringr::str_replace_all(string = herbarium_nbe_char,
                                                     pattern = "[.]",
                                                     replacement = " ")) %>%
    mutate(nbrs = readr::parse_number(herbarium_nbe_char)) %>%
    mutate(nbrs = ifelse(nbrs < 1, nbrs*-1, nbrs)) %>%
    arrange(desc(nbrs))

  ## getting collector of specimen
  all_herb_not_linked <-
    all_herb_not_linked %>%
    # filter(grepl("IDU", herbarium_nbe_char)) %>%
    mutate(coll = stringr::str_replace(string = herbarium_nbe_char,
                                       pattern = as.character(nbrs),
                                       replacement = "")) %>%
    mutate(coll = str_trim(coll))


  ## linking collector to collector table
  # all_herb_not_linked <-
  #   .link_colnam(data_stand = all_herb_not_linked, collector_field = "coll")

  all_herb_not_linked <- .link_colnam(
    data_stand = all_herb_not_linked,
    column_searched = "coll",
    column_name = "colnam",
    id_field = "id_colnam",
    id_table_name = "id_table_colnam",
    db_connection = mydb,
    table_name = "table_colnam"
  )

  # if (!is.null(collector)) {
  #   cli::cli_alert_info("filtering on collector : {collector$col_name}")
  #
  #   all_herb_not_linked <-
  #     all_herb_not_linked %>%
  #     filter(id_colnam == collector$id_colnam)
  #
  # }

  ## getting information from data_liste_plot
  all_herb_not_linked <-
    all_herb_not_linked %>%
    left_join(tbl(mydb, "data_individuals") %>%
                select(id_n, id_table_liste_plots_n, idtax_n) %>%
                collect(),
              by=c("id_n"="id_n")) %>%
    left_join(tbl(mydb, "data_liste_plots") %>%
                select(id_liste_plots, plot_name, team_leader) %>%
                collect(),
              by=c("id_table_liste_plots_n"="id_liste_plots"))

  all_linked_individuals <-
    tbl(mydb, "data_link_specimens") %>%
    distinct(id_n) %>%
    collect()

  ### selection of all individuals with specimens linked but not included in link table
  # all_herb_missing_link <-
  #   all_herb_not_linked %>%
  #   filter(!id_n %in% all_linked_individuals$id_n)
  #
  # all_herb_missing_link_unique <-
  #   all_herb_missing_link %>%
  #   group_by(id_colnam, nbrs) %>%
  #   count() %>%
  #   ungroup()
  #
  # cli::cli_alert_info("Missing link for {nrow(all_herb_missing_link_unique)} specimens")
  # print(all_herb_missing_link_unique %>%
  #         dplyr::select(-n) %>%
  #         group_by(id_colnam) %>%
  #         count())

  return(list(all_herb_not_linked = all_herb_not_linked,
              all_linked_individuals = all_linked_individuals))


  # all_herb_missing_link = all_herb_missing_link,

}



#' Query link between specimens and individuals
#'
#' Query links between specimens and individuals
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_ind integer
#' @param id_specimen integer
#'
#' @return No values
#' @export
query_link_individual_specimen <- function(id_ind = NULL,
                                           id_specimen = NULL) {
  
  mydb <- call.mydb()
  
  
  if(!is.null(id_ind)) {
    
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_n %in% !!id_ind) %>%
      dplyr::collect() %>%
      as.data.frame()
    
  }
  
  if(!is.null(id_specimen)) {
    
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen %in% !!id_specimen) %>%
      dplyr::collect() %>%
      as.data.frame()
    
  }
  
  return(selected_link %>% as_tibble())
  
}



#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
.query_unmatched_specimens <- function() {
  
  all_herbarium_individuals <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::select(herbarium_nbe_char, herbarium_code_char, herbarium_nbe_type, id_diconame_n, id_specimen, id_n) %>%
    dplyr::filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char) | !is.na(herbarium_nbe_type)) %>%
    dplyr::collect()
  
  ### all specimens with more than one id_diconame in table individuals
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n>1)
  
  #### all specimens with more than one id_diconame in table individuals with names
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                       dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
                       dplyr::collect(),
                     by=c("id_diconame_n"="id_n")) %>%
    dplyr::filter(herbarium_nbe_char %in% dplyr::pull(all_herbarium_individuals_not_linked_diff_tax,
                                                      herbarium_nbe_char)) %>%
    dplyr::arrange(herbarium_nbe_char)
  
  ### all specimens with more than one genus in table individuals
  herb_specimen_diff_gen <-
    all_herbarium_individuals_not_linked_diff_tax %>%
    dplyr::distinct(herbarium_nbe_char, tax_gen) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n>1)
  
  ### all individuals concerned by specimens with more than one genus in table individuals
  data_individuals_concerned <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(herbarium_nbe_char %in% !!herb_specimen_diff_gen$herbarium_nbe_char) %>%
    dplyr::collect() %>%
    dplyr::select(dbh, code_individu,
                  # sous_plot_name,
                  tag, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type, id_diconame_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                       dplyr::select(id_n, full_name_no_auth, tax_gen, tax_esp, tax_fam) %>%
                       dplyr::collect(),
                     by=c("id_diconame_n"="id_n")) %>%
    dplyr::arrange(herbarium_nbe_char) %>%
    dplyr::collect()
  
  ### extraction of all specimens not linked to specimens table excluding problematic specimens
  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::filter(!herbarium_nbe_char %in%
                    unique(dplyr::pull(all_herbarium_individuals_not_linked_diff_tax,
                                       herbarium_nbe_char)))
  
  regexp <- "[[:digit:]]+"
  num_extracted <-
    stringr::str_extract(all_herbarium_individuals_not_linked$herbarium_nbe_char, regexp)
  
  df <-
    dplyr::tibble(full = all_herbarium_individuals_not_linked$herbarium_nbe_char,
                  num = num_extracted)
  
  coll_extracted <-
    apply(df, MARGIN = 1, FUN = function(x) gsub(x[2], "", x[1]))
  coll_extracted <-
    trimws(coll_extracted)
  
  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals_not_linked %>%
    tibble::add_column(col_name = coll_extracted) %>%
    tibble::add_column(colnbr = num_extracted)
  
  # all_herbarium_individuals_not_linked <-
  #   .link_colnam(data_stand = all_herbarium_individuals_not_linked,
  #                collector_field = 3)
  
  all_herbarium_individuals_not_linked <-
    .link_colnam(
      data_stand = all_herbarium_individuals_not_linked,
      column_searched = "col_name",
      column_name = "colnam",
      id_field = "id_colnam",
      id_table_name = "id_table_colnam",
      db_connection = mydb,
      table_name = "table_colnam"
    )
  
  return(list(all_herbarium_individuals_not_linked_diff_tax = all_herbarium_individuals_not_linked_diff_tax,
              data_individuals_not_linked_diff_tax_concerned = data_individuals_concerned,
              all_herbarium_individuals_not_linked = all_herbarium_individuals_not_linked))
}

