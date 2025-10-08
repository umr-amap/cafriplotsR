# Internal environment to store connections
.db_env <- new.env(parent = emptyenv())

credentials <- new.env()



#' Setup credentials storage in environment variables
#'
#' @description
#' Helper function to configure credentials in .Renviron file.
#' WARNING: Credentials will be stored in plain text. Only use on secure personal computers.
#' 
#' @param user Username for database
#' @param pass Password for database
#' 
#' @export
setup_db_credentials <- function(user = NULL, pass = NULL) {
  
  cli::cli_alert_warning("WARNING: Credentials will be stored in plain text in ~/.Renviron")
  cli::cli_alert_warning("Only proceed if this is your personal, secure computer")
  
  if (interactive()) {
    proceed <- readline("Do you want to continue? (yes/no): ")
    if (tolower(proceed) != "yes") {
      cli::cli_alert_info("Operation cancelled")
      return(invisible(FALSE))
    }
  }
  
  # Demander les credentials si non fournis
  if (is.null(user)) {
    user <- readline("Enter database username: ")
  }
  
  if (is.null(pass)) {
    pass <- get_password_secure("Enter database password: ")
  }
  
  # Chemin du fichier .Renviron
  renviron_path <- file.path(path.expand("~"), ".Renviron")
  
  # Lire le contenu existant
  if (file.exists(renviron_path)) {
    existing_lines <- readLines(renviron_path)
    # Supprimer les anciennes entrées MYDB si elles existent
    existing_lines <- existing_lines[!grepl("^MYDB_USER=", existing_lines)]
    existing_lines <- existing_lines[!grepl("^MYDB_PASS=", existing_lines)]
  } else {
    existing_lines <- character(0)
  }
  
  # Ajouter les nouvelles credentials
  new_lines <- c(
    existing_lines,
    paste0("MYDB_USER=", user),
    paste0("MYDB_PASS=", pass)
  )
  
  # Écrire dans .Renviron
  writeLines(new_lines, renviron_path)
  
  cli::cli_alert_success("Credentials saved to ~/.Renviron")
  cli::cli_alert_info("Restart R session for changes to take effect: .rs.restartR()")
  cli::cli_alert_info("To remove credentials later, use: remove_db_credentials()")
  
  invisible(TRUE)
}

#' Remove stored credentials
#' 
#' @export
remove_db_credentials <- function() {
  renviron_path <- file.path(path.expand("~"), ".Renviron")
  
  if (!file.exists(renviron_path)) {
    cli::cli_alert_info("No .Renviron file found")
    return(invisible(FALSE))
  }
  
  existing_lines <- readLines(renviron_path)
  
  # Filtrer les lignes MYDB
  has_mydb <- any(grepl("^MYDB_USER=|^MYDB_PASS=", existing_lines))
  
  if (!has_mydb) {
    cli::cli_alert_info("No stored credentials found")
    return(invisible(FALSE))
  }
  
  # Supprimer les entrées
  new_lines <- existing_lines[!grepl("^MYDB_USER=|^MYDB_PASS=", existing_lines)]
  
  writeLines(new_lines, renviron_path)
  
  cli::cli_alert_success("Credentials removed from ~/.Renviron")
  cli::cli_alert_info("Restart R session for changes to take effect: .rs.restartR()")
  
  # Nettoyer aussi le cache en mémoire
  rm(list = c("user_db", "password"), envir = credentials, inherits = FALSE)
  
  invisible(TRUE)
}



#' Test database connection
#'
#' @description
#' To test if the database connection is valid
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
    DBI::dbGetQuery(con, "SELECT 1 as test")
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#' Get password securely
#' @keywords internal
get_password_secure <- function(prompt) {
  if (interactive()) {
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

#' Get username securely
#' @keywords internal
get_username_secure <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    stop("Cannot prompt for username in non-interactive session. Please provide user parameter.")
  }
}

#' Create local DB config file
#'
#' Writes DB connection config to ~/.mydb_config.R
#' @export
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
  
  # Template de configuration
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
'
  
  cat(config_content, file = config_path)
  message("Database config file created at: ", config_path)
  source(config_path, local = FALSE)
  
  invisible(TRUE)
}



#' Connect to database
#'
#' @description
#' Generic function to connect to main or taxa database
#' 
#' @param db_type One of `"main"` or `"taxa"`.
#' @param pass Password. If NULL, will check environment then prompt.
#' @param user Username. If NULL, will check environment then prompt.
#' @param reset If TRUE, forces new credential prompt.
#' @param retry If TRUE, retry on failure.
#' @param use_env_credentials If TRUE, tries to use MYDB_USER and MYDB_PASS from .Renviron (default: FALSE)
#'
#' @returns A database connection object.
#' @export
connect_database <- function(db_type = c("main", "taxa"), 
                             pass = NULL, 
                             user = NULL, 
                             reset = FALSE, 
                             retry = TRUE,
                             use_env_credentials = FALSE) {
  db_type <- match.arg(db_type)
  create_db_config()
  
  # Variables selon le type de DB
  user_key <- "user_db"
  pass_key <- "password"
  
  if (db_type == "main") {
    conn_var <- "mydb"
    db_name_var <- db_name
  } else {
    conn_var <- "mydb_taxa"
    db_name_var <- db_name_taxa
  }
  
  # Reset
  if (reset) {
    if (!is.null(.db_env[[conn_var]])) {
      try(DBI::dbDisconnect(.db_env[[conn_var]]), silent = TRUE)
      .db_env[[conn_var]] <- NULL
    }
    if (exists(user_key, envir = credentials)) rm(list = user_key, envir = credentials)
    if (exists(pass_key, envir = credentials)) rm(list = pass_key, envir = credentials)
  }
  
  # Test connexion existante
  if (!is.null(.db_env[[conn_var]]) && test_connection(.db_env[[conn_var]])) {
    return(.db_env[[conn_var]])
  } else if (!is.null(.db_env[[conn_var]])) {
    cli::cli_alert_warning("{stringr::str_to_title(db_type)} database connection lost, reconnecting...")
    try(DBI::dbDisconnect(.db_env[[conn_var]]), silent = TRUE)
    .db_env[[conn_var]] <- NULL
  }
  
  # Essayer credentials d'environnement si activé
  if (use_env_credentials && is.null(user) && is.null(pass)) {
    env_user <- Sys.getenv("MYDB_USER")
    env_pass <- Sys.getenv("MYDB_PASS")
    
    if (env_user != "" && env_pass != "") {
      user <- env_user
      pass <- env_pass
      cli::cli_alert_info("Using stored credentials from environment")
    }
  }
  
  # Get credentials
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
  
  # Tentative de connexion avec retry
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
      
      # Vérification des droits pour taxa
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

#' Get primary database connection (wrapper)
#' @export
call.mydb <- function(pass = NULL, user = NULL, reset = FALSE, retry = TRUE, use_env_credentials = FALSE) {
  connect_database("main", pass, user, reset, retry, use_env_credentials)
}

#' Get taxa database connection (wrapper)
#' @export
call.mydb.taxa <- function(pass = NULL, user = NULL, reset = FALSE, retry = TRUE, use_env_credentials = FALSE) {
  if (reset) {
    cli::cli_alert_info("Taxa database: Remember that write operations are restricted")
  }
  connect_database("taxa", pass, user, reset, retry, use_env_credentials)
}



#' Check taxa database permissions
#' @export
check_taxa_permissions <- function(con) {
  tryCatch({
    test_query <- "SELECT 1 LIMIT 1"
    DBI::dbGetQuery(con, test_query)
    
    write_test <- tryCatch({
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

#' Cleanup all database connections
#' @export
cleanup_connections <- function() {
  if (!is.null(.db_env$mydb)) {
    try(DBI::dbDisconnect(.db_env$mydb), silent = TRUE)
    .db_env$mydb <- NULL
  }
  
  if (!is.null(.db_env$mydb_taxa)) {
    try(DBI::dbDisconnect(.db_env$mydb_taxa), silent = TRUE)
    .db_env$mydb_taxa <- NULL
  }
  
  rm(list = ls(envir = credentials), envir = credentials)
  
  cli::cli_alert_success("All connections closed and credentials cleared")
}

#' Get connection information
#' @export
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

#' Print connection status
#' @export
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

#' Complete database diagnostic
#' @export
db_diagnostic <- function() {
  cli::cli_h1("Database Diagnostic")
  
  print_connection_status()
  
  cli::cli_h2("Configuration")
  cli::cli_alert_info("Host: {db_host}:{db_port}")
  cli::cli_alert_info("Main database: {db_name}")
  cli::cli_alert_info("Taxa database: {db_name_taxa}")
  
  cli::cli_h2("Connectivity Tests")
  
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

#' Define user policy for row-level security
#' @export
define_user_policy <- function(con, user, ids, 
                               table = "data_liste_plots", 
                               policy_name = NULL,
                               operations = "SELECT",
                               drop_existing = TRUE) {
  
  valid_ops <- c("SELECT", "INSERT", "UPDATE", "DELETE", "ALL")
  
  stopifnot(
    "Connection must be valid" = !is.null(con) && test_connection(con),
    "User must be specified" = !is.null(user) && nchar(user) > 0,
    "IDs must be provided" = length(ids) > 0 && all(is.finite(ids)),
    "Operations must be valid" = all(operations %in% valid_ops)
  )
  
  if ("ALL" %in% operations) {
    operations <- "ALL"
    cli::cli_alert_info("Using 'ALL' operations (overrides specific operations)")
  }
  
  if (is.null(policy_name)) {
    policy_name <- paste0("policy_", gsub("[^a-zA-Z0-9_]", "_", user))
  }
  
  id_list <- paste(ids, collapse = ", ")
  
  tryCatch({
    sql_enable_rls <- glue::glue("ALTER TABLE {DBI::dbQuoteIdentifier(con, table)} ENABLE ROW LEVEL SECURITY;")
    DBI::dbExecute(con, sql_enable_rls)
    
    if (drop_existing) {
      existing_policies <- list_user_policies(con, user = user, table = table)
      policies_to_drop <- existing_policies[grepl(paste0("^", policy_name), existing_policies$policyname), ]
      
      for (policy in policies_to_drop$policyname) {
        sql_drop <- glue::glue("DROP POLICY IF EXISTS {DBI::dbQuoteIdentifier(con, policy)} ON {DBI::dbQuoteIdentifier(con, table)};")
        DBI::dbExecute(con, sql_drop)
        cli::cli_alert_info("Dropped existing policy: {policy}")
      }
    }
    
    if (length(operations) == 1 && operations == "ALL") {
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
    
    cli::cli_alert_info("User '{user}' granted {paste(operations, collapse = ', ')} access to plot IDs: {paste(ids, collapse = ', ')}")
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to create policy: {e$message}")
    stop("Policy creation failed: ", e$message, call. = FALSE)
  })
}

#' List user policies
#' @export
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

#' Helper functions for common policy scenarios
#' @export
define_read_only_policy <- function(con, user, ids, table = "data_liste_plots") {
  define_user_policy(con, user, ids, table, operations = "SELECT")
}

#' @export
define_full_access_policy <- function(con, user, ids, table = "data_liste_plots") {
  define_user_policy(con, user, ids, table, operations = "ALL")
}

#' @export
define_read_write_policy <- function(con, user, ids, table = "data_liste_plots") {
  define_user_policy(con, user, ids, table, operations = c("SELECT", "INSERT", "UPDATE"))
}




# Database Query Utilities -----------------------------------------------

#' Safely execute a SQL query with automatic retry
#'
#' This function attempts to execute a SQL query using \code{DBI::dbSendQuery()} and \code{DBI::dbFetch()},
#' with automatic retries in case of transient database failures such as connection loss
#' or query preparation errors.
#'
#' @param con A DBI connection object.
#' @param sql A SQL query string, typically created using \code{glue::glue_sql()}.
#' @param max_attempts Integer. Maximum number of attempts before giving up. Default is 10.
#' @param wait_seconds Numeric. Time in seconds to wait between retries. Default is 1.
#' @param verbose Logical. If \code{TRUE}, displays informative messages. Default is \code{TRUE}.
#'
#' @return A tibble containing the query results, with unique column names.
#'
#' @details
#' This function is designed for read queries (e.g., \code{SELECT}) that return results.
#' For write queries (e.g., \code{UPDATE}, \code{INSERT}, \code{DELETE}), use a variant that uses \code{dbExecute()} or \code{dbSendStatement()}.
#'
#' If the database connection is lost, the function stops immediately.
#' If the query fails to prepare (e.g., due to a lock or temporary issue), the function retries up to \code{max_attempts}.
#'
#'
#' @export
func_try_fetch <-
  function(con,
           sql,
           max_attempts = 10,
           wait_seconds = 1,
           verbose = TRUE) {
    attempt <- 1
    success <- FALSE
    result <- NULL
    last_error <- NULL

    while (attempt <= max_attempts && !success) {
      if (verbose)
        cli::cli_alert_info("Attempt {attempt} of {max_attempts}...")

      try_result <- try({
        rs <- DBI::dbSendQuery(con, sql)
        result <- DBI::dbFetch(rs)
        DBI::dbClearResult(rs)
      }, silent = TRUE)

      if (inherits(try_result, "try-error")) {
        error_message <- conditionMessage(attr(try_result, "condition"))

        if (grepl("Lost connection to database",
                  error_message,
                  ignore.case = TRUE)) {
          stop("❌ Lost connection to database. Aborting.")
        }

        if (grepl("Failed to prepare query", error_message, ignore.case = TRUE)) {
          cli::cli_alert_warning("Failed to prepare query (attempt {attempt}): {error_message}")
          last_error <- error_message
          attempt <- attempt + 1
          Sys.sleep(wait_seconds)
        } else {
          stop(glue::glue("❌ Unhandled error: {error_message}"))
        }

      } else {
        success <- TRUE
      }
    }

    if (!success) {
      stop(
        glue::glue(
          "❌ Failed to fetch query after {max_attempts} attempts. Last error: {last_error}"
        )
      )
    }

    if (success && verbose) {
      cli::cli_alert_success("✅ Successfully connected and fetched {nrow(result)} rows.")
    }


    # Return as tibble (with unique column names)
    tibble::as_tibble(result, .name_repair = "unique")
  }


#' Try to open PostgreSQL table
#'
#' @description
#' Access to postgresql database table with repeating if no successful
#'
#' @param table A table name.
#' @param con A database connection.
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @returns
#' A `tbl` object representing the PostgreSQL table. The function will error
#' if the connection to the database is lost or if it fails to connect after
#' 10 attempts.
#'
#' @export
try_open_postgres_table <- function(table, con) {

  mydb <- call.mydb()

  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({table_postgre <- dplyr::tbl(con, table)}, silent = TRUE)

    if (any(grepl("Lost connection to database", res_q[1])))
      stop("Lost connection to database")

    if (any(grepl("Failed to prepare query", res_q[1]))) {
      rep <- TRUE
      cli::cli_alert_warning("---")
      rep_try <- rep_try + 1
    } else {
      rep <- FALSE
    }

    if (rep_try == 10)
      stop("Failed to connect to database")
  }

  return(table_postgre)
}