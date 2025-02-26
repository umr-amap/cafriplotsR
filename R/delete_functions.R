


.delete_taxa <- function(id) {
  
  if (exists("mydb_taxa")) rm(mydb_taxa)
  if (!exists("mydb_taxa")) call.mydb.taxa()
  
  # DBI::dbExecute(mydb,
  #                "DELETE FROM table_taxa WHERE idtax_n=$1", params=list(id)
  # )
  
  query <- "DELETE FROM table_taxa WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("idtax_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb_taxa, query)
  DBI::dbClearResult(rs)
}








#' Delete an entry in traits measurements table
#'
#' Delete an entry in traits measurements table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_sp_trait_measure <- function(id) {
  
  if (!exists("mydb_taxa")) call.mydb.taxa()
  
  tbl <- "table_traits_measures"
  sql <- glue::glue_sql("SELECT * FROM {`tbl`} WHERE id_trait_measures IN ({vals*})",
                        vals = id, .con = mydb_taxa)
  
  valuetype <- func_try_fetch(con = mydb_taxa, sql = sql)
  
}



#' Delete an entry in subplotype_list table
#'
#' Delete an entry in subplotype_list table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_subplottype <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  askYesNo(msg = "You are about to delete entries in the table that contain subplot features types. Do you confirm ?")
  
  query <- "DELETE FROM subplotype_list WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_subplotype IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
  
}

#' Delete an entry in data_liste_sub_plots features
#'
#' Delete an entry in data_liste_sub_plots features using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_subplotfeature <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  query <- "DELETE FROM data_liste_sub_plots WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_sub_plots IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
  
  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_liste_sub_plots WHERE id_sub_plots=$1", params=list(id)
  # )
}

#' Delete an entry in trait measurement table (individuals features)
#'
#' Delete an entry in trait measurement table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_trait_measure <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_traits_measures WHERE id_trait_measures=$1", params=list(id)
  # )
  
  feats <- query_traits_measures_features(id_trait_measures = id)
  
  if (!is.null(dim(feats$all_feat_pivot))) {
    
    feats$all_feat_pivot <-
      test <-
      feats$all_feat_pivot %>%
      mutate(id_ind_meas_feat_n = str_extract_all(id_ind_meas_feat, "[[:digit:]]+"))
    
    
    print(feats)
    
    rm_feats <- askYesNo(msg = "Remove associated features")
    
    if (rm_feats)
      .delete_entry_trait_measure_features(id = as.numeric(unlist(feats$all_feat_pivot$id_ind_meas_feat_n)))
    
  }
  
  query <- "DELETE FROM data_traits_measures WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_trait_measures IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
}


#' Delete an entry in trait measurement features table
#'
#' Delete an entry in trait measurement features table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_trait_measure_features <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  query <- "DELETE FROM data_ind_measures_feat WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_ind_meas_feat IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
}


#' Delete an entry in trait measurement table
#'
#' Delete an entry in trait measurement table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_ind integer
#' @param id_specimen integer
#'
#' @return No values
#' @export
.delete_link_individual_specimen <- function(id_ind = NULL,
                                             id_specimen = NULL,
                                             id_link = NULL) {
  
  if(!exists("mydb")) call.mydb()
  
  if(!is.null(id_ind)) {
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_n %in% !!id_ind) %>%
      dplyr::collect() %>%
      as.data.frame()
    
    print(selected_link)
    confirm <-
      utils::askYesNo(msg = "Confirm removing these links?")
    
    if(confirm)
      for (i in 1:nrow(selected_link))
        DBI::dbExecute(mydb,
                       "DELETE FROM data_link_specimens WHERE id_n=$1",
                       params=list(selected_link$id_n[i]))
  }
  
  if(!is.null(id_specimen)) {
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen %in% !!id_specimen) %>%
      dplyr::collect() %>%
      as.data.frame()
    
    print(selected_link)
    
    confirm <-
      utils::askYesNo(msg = "Confirm removing these links?")
    
    if(confirm) {
      query <- "DELETE FROM data_link_specimens WHERE MMM"
      query <-
        gsub(
          pattern = "MMM",
          replacement = paste0("id_specimen IN ('",
                               paste(unique(selected_link$id_specimen), collapse = "', '"), "')"),
          x = query
        )
      
      rs <- DBI::dbSendQuery(mydb, query)
      DBI::dbClearResult(rs)
    }
    
    
  }
  
  if (!is.null(id_link)) {
    
    query <- "DELETE FROM data_link_specimens WHERE MMM"
    query <-
      gsub(
        pattern = "MMM",
        replacement = paste0("id_link_specimens IN ('",
                             paste(unique(id_link), collapse = "', '"), "')"),
        x = query
      )
    
    rs <- DBI::dbSendQuery(mydb, query)
    DBI::dbClearResult(rs)
    
  }
  
  
}

#' Delete an entry in individuals table
#'
#' Delete an entry in individuals table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_individuals <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_individuals WHERE id_n=$1", params=list(id)
  # )
  
  
  ind_feat <- query_individual_features(id = id, pivot_table = F, remove_obs_with_issue = F)
  
  if (length(ind_feat$traits_char) > 0 | 
      length(ind_feat$traits_num) > 0) {
    
    print(ind_feat)
    
    rm_feats <- askYesNo(msg = "Remove associated individual features ?")
    
    if (rm_feats) {
      if (length(ind_feat$traits_char) > 0)
        .delete_entry_trait_measure(id = ind_feat$traits_char[[1]]$id_trait_measures)
      
      if (length(ind_feat$traits_num) > 0)
        .delete_entry_trait_measure(id = ind_feat$traits_num[[1]]$id_trait_measures)      
    }
  }
  
  query <- "DELETE FROM data_individuals WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
  
}


#' Delete an entry in specimen table
#'
#' Delete an entry in specimen table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_specimens <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  query <- "DELETE FROM specimens WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_specimen IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )
  
  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
  
}


#' Delete an entry in plot meta-data
#'
#' Delete an entry in plot meta-data table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_plot <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  DBI::dbExecute(mydb,
                 "DELETE FROM data_liste_plots WHERE id_liste_plots=$1", params=list(id)
  )
}


#' Delete an entry in colnam table
#'
#' Delete an entry in colnam table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_colnam <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
  
  
  DBI::dbExecute(mydb,
                 "DELETE FROM table_colnam WHERE id_table_colnam=$1", params=list(id)
  )
}




#' Delete an entry in trait list
#'
#' Delete an entry in traitlist entry using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_trait_list <- function(id) {
  
  if(!exists("mydb")) call.mydb()
  
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
