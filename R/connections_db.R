

# Internal environment to store connections
.db_env <- new.env(parent = emptyenv())

credentials <- new.env()

#' Create local DB config file
#'
#' Writes DB connection config to ~/.mydb_config.R
#' @export
create_db_config <- function() {
  path <- file.path(Sys.getenv("HOME"), ".mydb_config.R")
  if (file.exists(path)) {
    return(invisible(FALSE))
  }
  
  cat(
    'db_host <- "dg474899-001.dbaas.ovh.net"\n',
    'db_port <- 35699\n',
    'db_name <- "plots_transects"\n',
    'db_name_taxa <- "rainbio"\n',
    file = path
  )
  message("Database config file created at: ", path)
  
  config_path <- file.path(Sys.getenv("HOME"), ".mydb_config.R")
  source(config_path, local = F)
  
  invisible(TRUE)
}


#' Get primary database connection
#'
#' Connects to the 'plots_transects' database. Asks for password and user if not supplied.
#' 
#' 
#' 
#' 
#' @param pass Password (string). If NULL, will prompt using rstudioapi.
#' @param user Username (string). If NULL, will prompt using rstudioapi.
#' @param offline If TRUE, return offline/mock version of tables.
#'
#'
#' @importFrom getPass getPass
#' @importFrom RPostgres Postgres
#' @importFrom DBI dbConnect
#' 
#' 
#' @return A DBI connection or a named list of mock tables (offline mode)
#' @export
call.mydb <- function(pass = NULL, user = NULL) {
  
  create_db_config()
  
  
  if (!is.null(.db_env$mydb)) return(.db_env$mydb)

  if (is.null(pass)) {
    
    if (!exists("password", envir = credentials)) {
      credentials$password <- tryCatch(
        getPass::getPass("Enter your password name"),
        error = function(e) readline("password: ")
      )
    }
    pass <- credentials$password
    
  }
  
  if (is.null(user)) {
    if (!exists("user_db", envir = credentials)) {
      credentials$user_db <- tryCatch(
        getPass::getPass("Enter your user name"),
        error = function(e) readline("User: ")
      )
    }
    user <- credentials$user_db
  }
  
  .db_env$mydb <- DBI::dbConnect(
      RPostgres::Postgres(),
      dbname = db_name,
      host = db_host,
      port = db_port,
      user = user,
      password = pass
    )
  
  
  .db_env$mydb
}



#' Get taxa database connection
#'
#' Connects to the 'rainbio' database.
#'
#' @param pass Password (string). If NULL, will prompt using rstudioapi.
#' @param user Username (string). If NULL, will prompt using rstudioapi.
#'
#' @return A DBI connection
#' @export
call.mydb.taxa <- function(pass = "Anyuser2022", user = "common", reset = FALSE) {
  
  create_db_config()
  
  if (reset) 
    cli::cli_alert_warning("For adding entries of modify the taxonomic backbone, you must have the right to do it. Enter password and login.")
  
  if (reset) .db_env$mydb_taxa <- NULL
  
  if (!is.null(.db_env$mydb_taxa)) return(.db_env$mydb_taxa)
  
  if (is.null(pass) | reset == TRUE) {
    
    if (!exists("password_mydbtaxa", envir = credentials) | reset == TRUE) {
      credentials$password_mydbtaxa <- tryCatch(
        getPass::getPass("Enter your password name"),
        error = function(e) readline("password: ")
      )
    }
    pass <- credentials$password_mydbtaxa
    
  }
  
  if (is.null(user) | reset == TRUE) {
    if (!exists("user_dbtaxa", envir = credentials) | reset == TRUE) {
      credentials$user_dbtaxa <- tryCatch(
        getPass::getPass("Enter your user name"),
        error = function(e) readline("User: ")
      )
    }
    user <- credentials$user_dbtaxa
  }
  
  # .db_env$mydb_taxa <- DBI::dbConnect(
  #   RPostgres::Postgres(),
  #   dbname = "plots_transects",
  #   host = "dg474899-001.dbaas.ovh.net",
  #   port = 35699,
  #   user = user,
  #   password = pass
  # )
  
  .db_env$mydb_taxa <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = db_name_taxa,
    host = db_host,
    port = db_port,
    user = user,
    password = pass
  )
  
  .db_env$mydb_taxa
  
}



# call.mydb <- function(pass=NULL, user=NULL, offline = FALSE) {
#   
#   if(!exists("mydb")) {
#     
#     if(!offline) {
#       
#       if(is.null(pass))
#         pass <- rstudioapi::askForPassword("Please enter your password")
#       
#       if(is.null(user))
#         user <- rstudioapi::askForPassword("Please enter your user name")
#       
#       
#       # mydb <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
#       #                   host = 'localhost',
#       #                   port = 5432, # or any other port specified by your DBA
#       #                   user = 'postgres',
#       #                   password = pass)
#       
#       mydb <- DBI::dbConnect(RPostgres::Postgres(),
#                              dbname = 'plots_transects',
#                              host = 'dg474899-001.dbaas.ovh.net',
#                              port = 35699, # or any other port specified by your DBA
#                              user = user,
#                              password = pass)
#       
#     } else {
#       
#       # mydb <-
#       #   list(data_liste_plots = dplyr::tbl(mydb, "data_liste_plots"),
#       #        data_liste_sub_plots = dplyr::tbl(mydb, "data_liste_sub_plots"),
#       #        table_colnam = dplyr::tbl(mydb, "table_colnam"),
#       #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
#       #        specimens = dplyr::tbl(mydb, "specimens"),
#       #        diconame = dplyr::tbl(mydb, "diconame"),
#       #        data_individuals = dplyr::tbl(mydb, "data_individuals"),
#       #        data_link_specimens = dplyr::tbl(mydb, "data_link_specimens"),
#       #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
#       #        traitlist = dplyr::tbl(mydb, "traitlist"),
#       #        data_traits_measures = dplyr::tbl(mydb, "data_traits_measures"))
#       
#       mydb <-
#         list(data_liste_plots = data_liste_plots,
#              data_liste_sub_plots = data_liste_sub_plots,
#              table_colnam = table_colnam,
#              subplotype_list = subplotype_list,
#              specimens = specimens,
#              diconame = diconame,
#              data_individuals = data_individuals,
#              data_link_specimens = data_link_specimens,
#              subplotype_list = subplotype_list,
#              traitlist = traitlist,
#              data_traits_measures = data_traits_measures)
#       
#       
#     }
#     
#     assign("mydb", mydb, envir = .GlobalEnv)
#     
#     # return(mydb)
#   }
# }


# call.mydb.taxa <- function(pass=NULL, user=NULL) {
#   
#   if(!exists("mydb_taxa")) {
#     
#     if(is.null(pass))
#       pass <- rstudioapi::askForPassword("Please enter your password")
#     
#     if(is.null(user))
#       user <- rstudioapi::askForPassword("Please enter your user name")
#     
#     mydb_taxa <- DBI::dbConnect(
#       RPostgres::Postgres(),
#       dbname = 'rainbio',
#       host = 'dg474899-001.dbaas.ovh.net',
#       port = 35699,
#       # or any other port specified by your DBA
#       user = user,
#       password = pass
#     )
#     
#     assign("mydb_taxa", mydb_taxa, envir = .GlobalEnv)
#     
#   }
# }


