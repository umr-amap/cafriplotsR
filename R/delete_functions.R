


.delete_taxa <- function(id) {

  mydb_taxa <- call.mydb.taxa()

  # DBI::dbExecute(mydb_taxa,
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


# .delete_entry_trait_measure <- function(id) {
#   
#   if(!exists("mydb")) call.mydb()
#   
#   # DBI::dbExecute(mydb,
#   #                "DELETE FROM data_traits_measures WHERE id_trait_measures=$1", params=list(id)
#   # )
#   
#   feats <- query_traits_measures_features(id_trait_measures = id)
#   
#   if (nrow(feats) > 0) {
#     
#     feat <-
#       feats %>%
#       mutate(id_ind_meas_feat_n = str_extract_all(id_ind_meas_feat, "[[:digit:]]+"))
#     
#     
#     print(feat)
#     
#     # askYesNo(msg = "Remove associated features")
#     rm_feats <- choose_prompt(message = "Remove associated features ?")
#     
#     if (rm_feats)
#       .delete_entry_trait_measure_features(id = as.numeric(unlist(feat$id_ind_meas_feat_n)))
#     
#   }
#   
#   query <- "DELETE FROM data_traits_measures WHERE MMM"
#   query <-
#     gsub(
#       pattern = "MMM",
#       replacement = paste0("id_trait_measures IN ('",
#                            paste(unique(id), collapse = "', '"), "')"),
#       x = query
#     )
#   
#   rs <- DBI::dbSendQuery(mydb, query)
#   DBI::dbClearResult(rs)
# }




# .delete_link_individual_specimen <- function(id_ind = NULL,
#                                              id_specimen = NULL,
#                                              id_link = NULL) {
#   
#   if(!exists("mydb")) call.mydb()
#   
#   if(!is.null(id_ind)) {
#     selected_link <-
#       dplyr::tbl(mydb, "data_link_specimens") %>%
#       dplyr::filter(id_n %in% !!id_ind) %>%
#       dplyr::collect() %>%
#       as.data.frame()
#     
#     
#     # confirm <-
#     #   utils::askYesNo(msg = "Confirm removing these links?")
#     
#     if (nrow(selected_link) > 0) {
#       print(selected_link)
#       
#       confirm <- 
#         choose_prompt(message = "Confirm removing these links?")
#       
#       if(confirm)
#         for (i in 1:nrow(selected_link))
#           DBI::dbExecute(mydb,
#                          "DELETE FROM data_link_specimens WHERE id_n=$1",
#                          params=list(selected_link$id_n[i]))
#     }
#     
#     
#   }
#   
#   if(!is.null(id_specimen)) {
#     selected_link <-
#       dplyr::tbl(mydb, "data_link_specimens") %>%
#       dplyr::filter(id_specimen %in% !!id_specimen) %>%
#       dplyr::collect() %>%
#       as.data.frame()
#     
#     print(selected_link)
#     
#     confirm <- 
#       choose_prompt(message = "Confirm removing these links?")
#     
#     if(confirm) {
#       query <- "DELETE FROM data_link_specimens WHERE MMM"
#       query <-
#         gsub(
#           pattern = "MMM",
#           replacement = paste0("id_specimen IN ('",
#                                paste(unique(selected_link$id_specimen), collapse = "', '"), "')"),
#           x = query
#         )
#       
#       rs <- DBI::dbSendQuery(mydb, query)
#       DBI::dbClearResult(rs)
#     }
#     
#     
#   }
#   
#   if (!is.null(id_link)) {
#     
#     query <- "DELETE FROM data_link_specimens WHERE MMM"
#     query <-
#       gsub(
#         pattern = "MMM",
#         replacement = paste0("id_link_specimens IN ('",
#                              paste(unique(id_link), collapse = "', '"), "')"),
#         x = query
#       )
#     
#     rs <- DBI::dbSendQuery(mydb, query)
#     DBI::dbClearResult(rs)
#     
#   }
#   
#   
# }


# .delete_individuals <- function(id) {
#   
#   if(!exists("mydb")) call.mydb()
#   
#   # DBI::dbExecute(mydb,
#   #                "DELETE FROM data_individuals WHERE id_n=$1", params=list(id)
#   # )
#   
#   
#   ind_feat <- query_individual_features(individual_ids = id, format = "long", remove_issues = FALSE)
#   
#   link_specimens <- 
#     try_open_postgres_table(table = "data_link_specimens", con = mydb) %>% 
#     filter(id_n %in% !!id) %>% 
#     collect()
#   
#   if (length(ind_feat$traits_char) > 0 | 
#       length(ind_feat$traits_num) > 0) {
#     
#     print(ind_feat)
#     
#     # rm_feats <- askYesNo(msg = "Remove associated individual features ?")
#     
#     rm_feats <- 
#       choose_prompt(message = "Remove associated individual features ?")
#     
#     if (rm_feats) {
#       if (length(ind_feat$traits_char) > 0)
#         .delete_entry_trait_measure(id = ind_feat$traits_char[[1]]$id_trait_measures)
#       
#       if (length(ind_feat$traits_num) > 0)
#         .delete_entry_trait_measure(id = ind_feat$traits_num[[1]]$id_trait_measures)      
#     }
#   }
#   
#   if (nrow(link_specimens) > 0) {
#     
#     print(link_specimens)
#     
#     rm_link <- 
#       choose_prompt(message = "Remove links to specimens ?")
#     
#     if (rm_link)
#       .delete_link_individual_specimen(id_ind = id)
#     
#   }
#   
#   query <- "DELETE FROM data_individuals WHERE MMM"
#   query <-
#     gsub(
#       pattern = "MMM",
#       replacement = paste0("id_n IN ('",
#                            paste(unique(id), collapse = "', '"), "')"),
#       x = query
#     )
#   
#   rs <- DBI::dbSendQuery(mydb, query)
#   DBI::dbClearResult(rs)
#   
# }


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

  mydb <- call.mydb()

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



#' Delete an entry in country table
#'
#' Delete an entry in country table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_country <- function(id) {

  mydb <- call.mydb()

  query <- "DELETE FROM table_countries WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_country IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

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

  mydb <- call.mydb()

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

  mydb <- call.mydb()

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



