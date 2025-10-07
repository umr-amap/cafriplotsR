




#' Get database foreign keys
#'
#' @description
#' Get a figure showing main tables and primary and foreign keys
#' 
#' @param con A database connection. Optional; if `NULL`, will call `call.mydb()`.
#'
#' @returns 
#' A database structure diagram showing foreign key relationships between
#' the specified tables.
#' 
#' @importFrom dm dm_from_con dm_draw
#'
#' @export
get_database_fk <- function(con) {
  
  if (is.null(con)) con <- call.mydb()
  
  dm_subset <-
    dm::dm_from_con(
      con,
      table_names = c(
        "data_liste_plots",
        "table_countries",
        "data_traits_measures",
        "data_individuals",
        "data_ind_measures_feat",
        "traitlist",
        "table_colnam",
        "data_liste_sub_plots",
        "subplotype_list",
        "data_subplot_feat"
      ),
      learn_keys = T
    )
  
  fig_struc <- 
    dm_subset %>% dm::dm_draw()
  
  return(fig_struc)
}


