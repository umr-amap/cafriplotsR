#' 
#' 
# Internal environment to store connections
.db_env <- new.env(parent = emptyenv())

credentials <- new.env()

#' Test database connection
#'
#' @description
#' To test if the database connection if valid
#' 
#' @param con A database connection object.
#'
#' @returns 
#' `TRUE` if the connection is valid and a test query succeeds, `FALSE` otherwise.
#'
#' @export
test_connection <- function(con) {
  if (is.null(con)) return(FALSE)
  
  tryCatch({
    # Test simple avec une requête légère
    DBI::dbGetQuery(con, "SELECT 1 as test")
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}


#' Connect to database
#'
#' @description
#' A short description...
#' 
#' @param db_type One of `"main"` or `"taxa"`.
#' @param pass A single string. Optional.
#' @param user A single string. Optional.
#' @param reset A single logical value. Optional.
#' @param retry A single logical value. Optional.
#'
#' @returns 
#' A database connection object. The function will error if the connection
#' fails after the maximum number of attempts.
#'
#' @export
connect_database <- function(db_type = c("main", "taxa"), pass = NULL, user = NULL, reset = FALSE, retry = TRUE) {
  db_type <- match.arg(db_type)
  create_db_config()
  
  # Utilisation des mêmes credentials pour les deux bases
  user_key <- "user_db"
  pass_key <- "password"
  
  # Définir les variables selon le type de DB
  if (db_type == "main") {
    conn_var <- "mydb"
    db_name_var <- db_name
  } else {
    conn_var <- "mydb_taxa"
    db_name_var <- db_name_taxa
  }
  
  # Reset: close existing connection and clear cache
  if (reset) {
    if (!is.null(.db_env[[conn_var]])) {
      try(DBI::dbDisconnect(.db_env[[conn_var]]), silent = TRUE)
      .db_env[[conn_var]] <- NULL
    }
    # Pour reset, on nettoie les credentials partagés
    if (exists(user_key, envir = credentials)) rm(list = user_key, envir = credentials)
    if (exists(pass_key, envir = credentials)) rm(list = pass_key, envir = credentials)
  }
  
  # Test existing connection
  if (!is.null(.db_env[[conn_var]]) && test_connection(.db_env[[conn_var]])) {
    return(.db_env[[conn_var]])
  } else if (!is.null(.db_env[[conn_var]])) {
    # Connection exists but is broken
    cli::cli_alert_warning("{stringr::str_to_title(db_type)} database connection lost, reconnecting...")
    try(DBI::dbDisconnect(.db_env[[conn_var]]), silent = TRUE)
    .db_env[[conn_var]] <- NULL
  }
  
  # Get credentials (partagés entre les deux bases)
  if (is.null(pass)) {
    if (!exists(pass_key, envir = credentials)) {
      credentials[[pass_key]] <- get_password_secure("Enter database password: ")
    }
    pass <- credentials[[pass_key]]
  }
  
  if (is.null(user)) {
    if (!exists(user_key, envir = credentials)) {
      credentials[[user_key]] <- get_username_secure("Enter database username: ")
    }
    user <- credentials[[user_key]]
  }
  
  # Attempt connection with retry logic
  max_attempts <- if (retry) 3 else 1
  
  for (attempt in 1:max_attempts) {
    tryCatch({
      .db_env[[conn_var]] <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname   = db_name_var,
        host     = db_host,
        port     = db_port,
        user     = user,
        password = pass,
        connect_timeout = 10
      )
      
      # Vérification des droits pour la base taxa
      if (db_type == "taxa") {
        check_taxa_permissions(.db_env[[conn_var]])
      }
      
      cli::cli_alert_success("Connected to {db_type} database successfully")
      return(.db_env[[conn_var]])
      
    }, error = function(e) {
      if (attempt == max_attempts) {
        cli::cli_alert_danger("Failed to connect to {db_type} database after {max_attempts} attempts: {e$message}")
        stop("Database connection failed: ", e$message, call. = FALSE)
      } else {
        cli::cli_alert_warning("Connection attempt {attempt} failed, retrying...")
        Sys.sleep(2)
      }
    })
  }
}

list_user_policies <- function(con, user = NULL, table = NULL) {
  sql_base <- "
    SELECT 
      schemaname,
      tablename,
      policyname,
      roles,
      cmd,
      qual
    FROM pg_policies
  "
  
  conditions <- c()
  if (!is.null(user)) {
    conditions <- c(conditions, glue::glue("'{user}' = ANY(roles)"))
  }
  if (!is.null(table)) {
    conditions <- c(conditions, glue::glue("tablename = '{table}'"))
  }
  
  if (length(conditions) > 0) {
    sql_base <- paste0(sql_base, " WHERE ", paste(conditions, collapse = " AND "))
  }
  
  sql_base <- paste0(sql_base, " ORDER BY schemaname, tablename, policyname;")
  
  DBI::dbGetQuery(con, sql_base)
}

# Fonction helper pour des cas d'usage courants
define_read_only_policy <- function(con, user, ids, table = "data_liste_plots") {
  define_user_policy(con, user, ids, table, operations = "SELECT")
}

define_full_access_policy <- function(con, user, ids, table = "data_liste_plots") {
  define_user_policy(con, user, ids, table, operations = "ALL")
}

define_read_write_policy <- function(con, user, ids, table = "data_liste_plots") {
  define_user_policy(con, user, ids, table, operations = c("SELECT", "INSERT", "UPDATE"))
}

#' Check taxa database permissions
#'
#' @description
#' A short description...
#' 
#' @param con A database connection object.
#'
#' @returns 
#' Called for side effects. Prints messages about database access permissions
#' and warns if write access is detected.
#'
#' @export
check_taxa_permissions <- function(con) {
  tryCatch({
    # Test des droits de lecture
    test_query <- "SELECT 1 LIMIT 1"
    DBI::dbGetQuery(con, test_query)
    
    # Test des droits d'écriture (doit échouer normalement)
    write_test <- tryCatch({
      # Utilise une table temporaire pour tester
      DBI::dbExecute(con, "CREATE TEMP TABLE test_write_permissions (id integer)")
      DBI::dbExecute(con, "DROP TABLE test_write_permissions")
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
    
    if (write_test) {
      cli::cli_alert_warning("Warning: You have write access to taxa database. Use with caution!")
    } else {
      cli::cli_alert_info("Taxa database: Read-only access confirmed")
    }
    
  }, error = function(e) {
    cli::cli_alert_warning("Could not verify taxa database permissions: {e$message}")
  })
}

# Fonctions wrapper simplifiées
call.mydb <- function(pass = NULL, user = NULL, reset = FALSE, retry = TRUE) {
  connect_database("main", pass, user, reset, retry)
}

call.mydb.taxa <- function(pass = NULL, user = NULL, reset = FALSE, retry = TRUE) {
  if (reset) {
    cli::cli_alert_info("Taxa database: Remember that write operations are restricted")
  }
  connect_database("taxa", pass, user, reset, retry)
}

# Amélioration 3: Gestion sécurisée des credentials
get_password_secure <- function(prompt) {
  if (interactive()) {
    # Utilise getPass si disponible, sinon rstudioapi si dans RStudio
    if (requireNamespace("getPass", quietly = TRUE)) {
      return(getPass::getPass(prompt))
    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      return(rstudioapi::askForPassword(prompt))
    } else {
      warning("No secure password input available, using readline")
      return(readline(paste0(prompt, " (WARNING: will be visible) ")))
    }
  } else {
    stop("Cannot prompt for password in non-interactive session. Please provide password parameter.")
  }
}

get_username_secure <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    stop("Cannot prompt for username in non-interactive session. Please provide user parameter.")
  }
}

# Amélioration 4: Configuration plus flexible
create_db_config <- function(config_path = NULL) {
  if (is.null(config_path)) {
    config_path <- file.path(path.expand("~"), ".mydb_config.R")
  }
  
  if (file.exists(config_path)) {
    tryCatch({
      source(config_path, local = FALSE)
      return(invisible(FALSE))
    }, error = function(e) {
      cli::cli_alert_warning("Error loading config file: {e$message}")
      cli::cli_alert_info("Creating new config file...")
    })
  }
  
  # Template de configuration plus complet
  config_content <- '
# Database Configuration
# Main database
db_host <- "dg474899-001.dbaas.ovh.net"
db_port <- 35699
db_name <- "plots_transects"

# Taxa database  
db_name_taxa <- "rainbio"

# Connection settings
db_connect_timeout <- 10
db_max_retries <- 3

# Optional: SSL settings
# db_sslmode <- "require"
'
  
  cat(config_content, file = config_path)
  message("Database config file created at: ", config_path)
  source(config_path, local = FALSE)
  
  invisible(TRUE)
}

# Amélioration 5: Fonction de nettoyage des connexions
cleanup_connections <- function() {
  if (!is.null(.db_env$mydb)) {
    try(DBI::dbDisconnect(.db_env$mydb), silent = TRUE)
    .db_env$mydb <- NULL
  }
  
  if (!is.null(.db_env$mydb_taxa)) {
    try(DBI::dbDisconnect(.db_env$mydb_taxa), silent = TRUE)
    .db_env$mydb_taxa <- NULL
  }
  
  # Clear credentials
  rm(list = ls(envir = credentials), envir = credentials)
  
  cli::cli_alert_success("All connections closed and credentials cleared")
}

# Amélioration 6: Policy management amélioré
define_user_policy <- function(con, user, ids, 
                               table = "data_liste_plots", 
                               policy_name = NULL,
                               operations = "SELECT",
                               drop_existing = TRUE) {
  
  # Validation des paramètres
  valid_ops <- c("SELECT", "INSERT", "UPDATE", "DELETE", "ALL")
  
  stopifnot(
    "Connection must be valid" = !is.null(con) && test_connection(con),
    "User must be specified" = !is.null(user) && nchar(user) > 0,
    "IDs must be provided" = length(ids) > 0 && all(is.finite(ids)),
    "Operations must be valid" = all(operations %in% valid_ops)
  )
  
  # Si "ALL" est spécifié, on l'utilise seul
  if ("ALL" %in% operations) {
    operations <- "ALL"
    cli::cli_alert_info("Using 'ALL' operations (overrides specific operations)")
  }
  
  if (is.null(policy_name)) {
    policy_name <- paste0("policy_", gsub("[^a-zA-Z0-9_]", "_", user))
  }
  
  id_list <- paste(ids, collapse = ", ")
  
  tryCatch({
    # Enable RLS first (idempotent)
    sql_enable_rls <- glue::glue("ALTER TABLE {DBI::dbQuoteIdentifier(con, table)} ENABLE ROW LEVEL SECURITY;")
    DBI::dbExecute(con, sql_enable_rls)
    
    # Drop existing policies if requested
    if (drop_existing) {
      # Drop policies with the same base name (could be multiple)
      existing_policies <- list_user_policies(con, user = user, table = table)
      policies_to_drop <- existing_policies[grepl(paste0("^", policy_name), existing_policies$policyname), ]
      
      for (policy in policies_to_drop$policyname) {
        sql_drop <- glue::glue("DROP POLICY IF EXISTS {DBI::dbQuoteIdentifier(con, policy)} ON {DBI::dbQuoteIdentifier(con, table)};")
        DBI::dbExecute(con, sql_drop)
        cli::cli_alert_info("Dropped existing policy: {policy}")
      }
    }
    
    # Create policies for each operation (ou une seule si ALL)
    if (length(operations) == 1 && operations == "ALL") {
      # Une seule policy pour ALL
      sql_create <- glue::glue("
        CREATE POLICY {DBI::dbQuoteIdentifier(con, policy_name)}
        ON {DBI::dbQuoteIdentifier(con, table)}
        FOR ALL
        TO {DBI::dbQuoteIdentifier(con, user)}
        USING (id_liste_plots IN ({id_list}));
      ")
      DBI::dbExecute(con, sql_create)
      cli::cli_alert_success("Policy '{policy_name}' created for ALL operations")
      
    } else {
      # Une policy par opération
      for (i in seq_along(operations)) {
        op <- operations[i]
        current_policy_name <- if (length(operations) > 1) {
          paste0(policy_name, "_", tolower(op))
        } else {
          policy_name
        }
        
        sql_create <- glue::glue("
          CREATE POLICY {DBI::dbQuoteIdentifier(con, current_policy_name)}
          ON {DBI::dbQuoteIdentifier(con, table)}
          FOR {op}
          TO {DBI::dbQuoteIdentifier(con, user)}
          USING (id_liste_plots IN ({id_list}));
        ")
        DBI::dbExecute(con, sql_create)
        cli::cli_alert_success("Policy '{current_policy_name}' created for {op} operations")
      }
    }
    
    # Log the policy creation
    cli::cli_alert_info("User '{user}' granted {paste(operations, collapse = ', ')} access to plot IDs: {paste(ids, collapse = ', ')}")
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create policy: {e$message}")
    stop("Policy creation failed: ", e$message, call. = FALSE)
  })
}

# Fonction pour obtenir des informations sur les connexions actives
get_connection_info <- function() {
  info <- list()
  
  # Main database
  if (!is.null(.db_env$mydb)) {
    main_info <- tryCatch({
      query_result <- DBI::dbGetQuery(.db_env$mydb, "
        SELECT 
          current_database() as database,
          current_user as user,
          inet_server_addr() as host,
          inet_server_port() as port
      ")
      list(
        status = "connected",
        database = query_result$database,
        user = query_result$user,
        host = query_result$host,
        port = query_result$port,
        connection_valid = test_connection(.db_env$mydb)
      )
    }, error = function(e) {
      list(status = "error", message = e$message)
    })
    info$main <- main_info
  } else {
    info$main <- list(status = "disconnected")
  }
  
  # Taxa database
  if (!is.null(.db_env$mydb_taxa)) {
    taxa_info <- tryCatch({
      query_result <- DBI::dbGetQuery(.db_env$mydb_taxa, "
        SELECT 
          current_database() as database,
          current_user as user,
          inet_server_addr() as host,
          inet_server_port() as port
      ")
      list(
        status = "connected",
        database = query_result$database,
        user = query_result$user,
        host = query_result$host,
        port = query_result$port,
        connection_valid = test_connection(.db_env$mydb_taxa)
      )
    }, error = function(e) {
      list(status = "error", message = e$message)
    })
    info$taxa <- taxa_info
  } else {
    info$taxa <- list(status = "disconnected")
  }
  
  return(info)
}

# Fonction pour afficher un résumé des connexions
print_connection_status <- function() {
  info <- get_connection_info()
  
  cli::cli_h2("Database Connections Status")
  
  # Main database
  if (info$main$status == "connected") {
    cli::cli_alert_success("Main DB: Connected to {info$main$database} as {info$main$user}")
    if (!info$main$connection_valid) {
      cli::cli_alert_warning("  ⚠ Connection appears to be broken")
    }
  } else if (info$main$status == "error") {
    cli::cli_alert_danger("Main DB: Error - {info$main$message}")
  } else {
    cli::cli_alert_info("Main DB: Not connected")
  }
  
  # Taxa database
  if (info$taxa$status == "connected") {
    cli::cli_alert_success("Taxa DB: Connected to {info$taxa$database} as {info$taxa$user}")
    if (!info$taxa$connection_valid) {
      cli::cli_alert_warning("  ⚠ Connection appears to be broken")
    }
  } else if (info$taxa$status == "error") {
    cli::cli_alert_danger("Taxa DB: Error - {info$taxa$message}")
  } else {
    cli::cli_alert_info("Taxa DB: Not connected")
  }
}

# Fonction pour un diagnostic complet
db_diagnostic <- function() {
  cli::cli_h1("Database Diagnostic")
  
  # Status des connexions
  print_connection_status()
  
  # Informations de configuration
  cli::cli_h2("Configuration")
  cli::cli_alert_info("Host: {db_host}:{db_port}")
  cli::cli_alert_info("Main database: {db_name}")
  cli::cli_alert_info("Taxa database: {db_name_taxa}")
  
  # Test de connectivité basique
  cli::cli_h2("Connectivity Tests")
  
  # Test main DB
  main_test <- tryCatch({
    con <- call.mydb()
    result <- DBI::dbGetQuery(con, "SELECT version() as version")
    cli::cli_alert_success("Main DB: Connection test passed")
    cli::cli_alert_info("  PostgreSQL version: {substr(result$version, 1, 50)}...")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Main DB: Connection test failed - {e$message}")
    FALSE
  })
  
  # Test taxa DB
  taxa_test <- tryCatch({
    con <- call.mydb.taxa()
    result <- DBI::dbGetQuery(con, "SELECT version() as version")
    cli::cli_alert_success("Taxa DB: Connection test passed")
    cli::cli_alert_info("  PostgreSQL version: {substr(result$version, 1, 50)}...")
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Taxa DB: Connection test failed - {e$message}")
    FALSE
  })
  
  return(invisible(list(main = main_test, taxa = taxa_test)))
}


#' 
#' #' Create local DB config file
#' #'
#' #' Writes DB connection config to ~/.mydb_config.R
#' create_db_config <- function() {
#'   path <- file.path(path.expand("~"), ".mydb_config.R")
#'   if (file.exists(path)) {
#'     config_path <- file.path(path.expand("~"), ".mydb_config.R")
#'     # print(config_path)
#'     source(config_path, local = F)
#'     return(invisible(FALSE))
#'   }
#'   
#'   
#'   cat(
#'     'db_host <- "dg474899-001.dbaas.ovh.net"\n',
#'     'db_port <- 35699\n',
#'     'db_name <- "plots_transects"\n',
#'     'db_name_taxa <- "rainbio"\n',
#'     file = path
#'   )
#'   message("Database config file created at: ", path)
#'   
#' 
#'   
#'   invisible(TRUE)
#' }
#' 
#' 
#' 
#' 
#' call.mydb <- function(pass = NULL, user = NULL, reset = FALSE) {
#'   create_db_config()
#'   
#'   # Reset: close existing connection and clear cache
#'   if (reset && !is.null(.db_env$mydb)) {
#'     try(DBI::dbDisconnect(.db_env$mydb), silent = TRUE)
#'     .db_env$mydb <- NULL
#'     rm(list = c("user_db", "password"), envir = credentials)
#'   }
#'   
#'   # Return cached connection if available
#'   if (!is.null(.db_env$mydb)) return(.db_env$mydb)
#'   
#'   # Prompt for password if not provided
#'   if (is.null(pass)) {
#'     if (!exists("password", envir = credentials)) {
#'       credentials$password <- tryCatch(
#'         getPass::getPass("Enter your password: "),
#'         error = function(e) readline("Password: ")
#'       )
#'     }
#'     pass <- credentials$password
#'   }
#'   
#'   # Prompt for user if not provided
#'   if (is.null(user)) {
#'     if (!exists("user_db", envir = credentials)) {
#'       credentials$user_db <- tryCatch(
#'         getPass::getPass("Enter your user name: "),
#'         error = function(e) readline("User: ")
#'       )
#'     }
#'     user <- credentials$user_db
#'   }
#'   
#'   # Connect
#'   .db_env$mydb <- DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     dbname   = db_name,
#'     host     = db_host,
#'     port     = db_port,
#'     user     = user,
#'     password = pass
#'   )
#'   
#'   .db_env$mydb
#' }
#' 
#' 
#' 
#' call.mydb.taxa <- function(pass = "Anyuser2022", user = "common", reset = FALSE) {
#'   
#'   create_db_config()
#'   
#'   if (reset) 
#'     cli::cli_alert_warning("For adding entries of modify the taxonomic backbone, you must have the right to do it. Enter password and login.")
#'   
#'   if (reset) .db_env$mydb_taxa <- NULL
#'   
#'   if (!is.null(.db_env$mydb_taxa)) return(.db_env$mydb_taxa)
#'   
#'   if (is.null(pass) | reset == TRUE) {
#'     
#'     if (!exists("password_mydbtaxa", envir = credentials) | reset == TRUE) {
#'       credentials$password_mydbtaxa <- tryCatch(
#'         getPass::getPass("Enter your password name"),
#'         error = function(e) readline("password: ")
#'       )
#'     }
#'     pass <- credentials$password_mydbtaxa
#'     
#'   }
#'   
#'   if (is.null(user) | reset == TRUE) {
#'     if (!exists("user_dbtaxa", envir = credentials) | reset == TRUE) {
#'       credentials$user_dbtaxa <- tryCatch(
#'         getPass::getPass("Enter your user name"),
#'         error = function(e) readline("User: ")
#'       )
#'     }
#'     user <- credentials$user_dbtaxa
#'   }
#'   
#'   # .db_env$mydb_taxa <- DBI::dbConnect(
#'   #   RPostgres::Postgres(),
#'   #   dbname = "plots_transects",
#'   #   host = "dg474899-001.dbaas.ovh.net",
#'   #   port = 35699,
#'   #   user = user,
#'   #   password = pass
#'   # )
#'   
#'   .db_env$mydb_taxa <- DBI::dbConnect(
#'     RPostgres::Postgres(),
#'     dbname = db_name_taxa,
#'     host = db_host,
#'     port = db_port,
#'     user = user,
#'     password = pass
#'   )
#'   
#'   .db_env$mydb_taxa
#'   
#' }
#' 
#' 
#' 
#' define_user_policy <- function(con, user, ids, 
#'                                table = "data_liste_plots", policy_name = NULL) {
#'   stopifnot(length(ids) > 0, !is.null(user))
#'   
#'   if (is.null(policy_name)) {
#'     policy_name <- paste0("policy_", user)
#'   }
#'   
#'   id_list <- paste(ids, collapse = ", ")
#'   
#'   # Drop the policy if it already exists
#'   sql_drop <- glue::glue(
#'     "DROP POLICY IF EXISTS {DBI::dbQuoteIdentifier(con, policy_name)} ON {DBI::dbQuoteIdentifier(con, table)};"
#'   )
#'   
#'   # Create the new policy
#'   sql_create <- glue::glue("
#'     CREATE POLICY {DBI::dbQuoteIdentifier(con, policy_name)}
#'     ON {DBI::dbQuoteIdentifier(con, table)}
#'     FOR SELECT
#'     TO {DBI::dbQuoteIdentifier(con, user)}
#'     USING (id_liste_plots IN ({id_list}));
#'   ")
#'   
#'   # Enable RLS on the table (only needs to be done once per table)
#'   sql_enable_rls <- glue::glue("ALTER TABLE {DBI::dbQuoteIdentifier(con, table)} ENABLE ROW LEVEL SECURITY;")
#'   
#'   dbExecute(con, sql_enable_rls)
#'   dbExecute(con, sql_drop)
#'   dbExecute(con, sql_create)
#'   
#'   cli::cli_alert_success("Policy {policy_name} updated for user {user} on table {table}")
#'   
#'   invisible(TRUE)
#' }
#' 
#' 
#' 
#' # call.mydb <- function(pass=NULL, user=NULL, offline = FALSE) {
#' #   
#' #   if(!exists("mydb")) {
#' #     
#' #     if(!offline) {
#' #       
#' #       if(is.null(pass))
#' #         pass <- rstudioapi::askForPassword("Please enter your password")
#' #       
#' #       if(is.null(user))
#' #         user <- rstudioapi::askForPassword("Please enter your user name")
#' #       
#' #       
#' #       # mydb <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
#' #       #                   host = 'localhost',
#' #       #                   port = 5432, # or any other port specified by your DBA
#' #       #                   user = 'postgres',
#' #       #                   password = pass)
#' #       
#' #       mydb <- DBI::dbConnect(RPostgres::Postgres(),
#' #                              dbname = 'plots_transects',
#' #                              host = 'dg474899-001.dbaas.ovh.net',
#' #                              port = 35699, # or any other port specified by your DBA
#' #                              user = user,
#' #                              password = pass)
#' #       
#' #     } else {
#' #       
#' #       # mydb <-
#' #       #   list(data_liste_plots = dplyr::tbl(mydb, "data_liste_plots"),
#' #       #        data_liste_sub_plots = dplyr::tbl(mydb, "data_liste_sub_plots"),
#' #       #        table_colnam = dplyr::tbl(mydb, "table_colnam"),
#' #       #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
#' #       #        specimens = dplyr::tbl(mydb, "specimens"),
#' #       #        diconame = dplyr::tbl(mydb, "diconame"),
#' #       #        data_individuals = dplyr::tbl(mydb, "data_individuals"),
#' #       #        data_link_specimens = dplyr::tbl(mydb, "data_link_specimens"),
#' #       #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
#' #       #        traitlist = dplyr::tbl(mydb, "traitlist"),
#' #       #        data_traits_measures = dplyr::tbl(mydb, "data_traits_measures"))
#' #       
#' #       mydb <-
#' #         list(data_liste_plots = data_liste_plots,
#' #              data_liste_sub_plots = data_liste_sub_plots,
#' #              table_colnam = table_colnam,
#' #              subplotype_list = subplotype_list,
#' #              specimens = specimens,
#' #              diconame = diconame,
#' #              data_individuals = data_individuals,
#' #              data_link_specimens = data_link_specimens,
#' #              subplotype_list = subplotype_list,
#' #              traitlist = traitlist,
#' #              data_traits_measures = data_traits_measures)
#' #       
#' #       
#' #     }
#' #     
#' #     assign("mydb", mydb, envir = .GlobalEnv)
#' #     
#' #     # return(mydb)
#' #   }
#' # }
#' 
#' 
#' # call.mydb.taxa <- function(pass=NULL, user=NULL) {
#' #   
#' #   if(!exists("mydb_taxa")) {
#' #     
#' #     if(is.null(pass))
#' #       pass <- rstudioapi::askForPassword("Please enter your password")
#' #     
#' #     if(is.null(user))
#' #       user <- rstudioapi::askForPassword("Please enter your user name")
#' #     
#' #     mydb_taxa <- DBI::dbConnect(
#' #       RPostgres::Postgres(),
#' #       dbname = 'rainbio',
#' #       host = 'dg474899-001.dbaas.ovh.net',
#' #       port = 35699,
#' #       # or any other port specified by your DBA
#' #       user = user,
#' #       password = pass
#' #     )
#' #     
#' #     assign("mydb_taxa", mydb_taxa, envir = .GlobalEnv)
#' #     
#' #   }
#' # }
#' 
#' 
