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
#' Falls back to in-memory configuration if file creation fails.
#' @export
create_db_config <- function(config_path = NULL) {

  # Define default configuration values
  define_config_vars <- function() {
    # Assign to global environment so they're accessible
    assign("db_host", "dg474899-001.dbaas.ovh.net", envir = .GlobalEnv)
    assign("db_port", 35699, envir = .GlobalEnv)
    assign("db_name", "plots_transects", envir = .GlobalEnv)
    assign("db_name_taxa", "rainbio", envir = .GlobalEnv)
    assign("db_connect_timeout", 10, envir = .GlobalEnv)
    assign("db_max_retries", 3, envir = .GlobalEnv)
  }

  # If config_path is NULL, try default location
  if (is.null(config_path)) {
    config_path <- file.path(path.expand("~"), ".mydb_config.R")
  }

  # If config file exists, try to load it
  if (file.exists(config_path)) {
    tryCatch({
      source(config_path, local = FALSE)
      return(invisible(FALSE))
    }, error = function(e) {
      cli::cli_alert_warning("Error loading config file: {e$message}")
      cli::cli_alert_info("Using default configuration in memory")
      define_config_vars()
      return(invisible(TRUE))
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

  # Try to create the config file
  result <- suppressWarnings(tryCatch({
    # Ensure parent directory exists
    config_dir <- dirname(config_path)
    if (!dir.exists(config_dir)) {
      dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Write config file
    cat(config_content, file = config_path)
    message("Database config file created at: ", config_path)
    source(config_path, local = FALSE)
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning("Could not create config file at: {config_path}")
    cli::cli_alert_info("Reason: {e$message}")
    cli::cli_alert_info("Using in-memory configuration instead")
    define_config_vars()
    FALSE
  }))

  invisible(result)
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
  
  # If credentials are provided explicitly, cache them so subsequent calls
  # (e.g. call.mydb.taxa() inside query_plots) reuse them without prompting
  if (!is.null(pass)) credentials[[pass_key]] <- pass
  if (!is.null(user)) credentials[[user_key]] <- user

  # Get credentials (from cache or interactive prompt)
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
#'
#' @description
#' Returns a database connection. In Shiny apps with connection pools,
#' this function automatically uses the pool if available in .db_env.
#'
#' @export
call.mydb <- function(pass = NULL, user = NULL, reset = FALSE, retry = TRUE, use_env_credentials = FALSE) {
  # Check if a pool is available in the environment (set by Shiny apps)
  if (!is.null(.db_env$pool_main)) {
    return(.db_env$pool_main)
  }

  # Otherwise use regular connection
  connect_database("main", pass, user, reset, retry, use_env_credentials)
}

#' Get taxa database connection (wrapper)
#'
#' @description
#' Returns a database connection. In Shiny apps with connection pools,
#' this function automatically uses the pool if available in .db_env.
#'
#' @export
call.mydb.taxa <- function(pass = NULL, user = NULL, reset = FALSE, retry = TRUE, use_env_credentials = FALSE) {
  # Check if a pool is available in the environment (set by Shiny apps)
  if (!is.null(.db_env$pool_taxa)) {
    return(.db_env$pool_taxa)
  }

  # Otherwise use regular connection
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
  # Close regular DBI connections
  if (!is.null(.db_env$mydb)) {
    try(DBI::dbDisconnect(.db_env$mydb), silent = TRUE)
    .db_env$mydb <- NULL
  }

  if (!is.null(.db_env$mydb_taxa)) {
    try(DBI::dbDisconnect(.db_env$mydb_taxa), silent = TRUE)
    .db_env$mydb_taxa <- NULL
  }

  # Close pool connections (used by Shiny apps)
  if (!is.null(.db_env$pool_main)) {
    try(pool::poolClose(.db_env$pool_main), silent = TRUE)
    .db_env$pool_main <- NULL
  }

  if (!is.null(.db_env$pool_taxa)) {
    try(pool::poolClose(.db_env$pool_taxa), silent = TRUE)
    .db_env$pool_taxa <- NULL
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
#'
#' @description
#' Creates or updates row-level security policies for a user on a specified table.
#' Supports different modes for managing plot access: replace (default), add, or remove.
#'
#' **Access model**: Users have access to plots through two mechanisms:
#' 1. **Creator access** (automatic): Users can always access plots they created
#'    (via global `creator_access_*` policies using the `created_by` column)
#' 2. **Explicit grants** (via this function): Admin grants access to other users' plots
#'
#' This function creates per-user policies for explicit grants. The global policies
#' handle INSERT (open to all) and creator access (automatic).
#'
#' @param con A database connection object.
#' @param user Character. The username to create the policy for.
#' @param ids Integer vector. Plot IDs to grant additional SELECT/UPDATE/DELETE access to.
#'   These are plots created by OTHER users that this user should also access.
#' @param table Character. The table to apply the policy to. Default is "data_liste_plots".
#' @param policy_name Character. Custom policy name (optional). If NULL, generates from username.
#' @param operations Character vector. Operations to allow: "SELECT", "INSERT", "UPDATE",
#'   "DELETE", or "ALL". Default is "SELECT". Note: INSERT is handled globally and skipped.
#' @param drop_existing Logical. Whether to drop existing policies before creating new ones.
#'   Default TRUE. Ignored when mode is "add" or "remove".
#' @param mode Character. How to handle existing plot access:
#'   - "replace" (default): Replace existing access with new IDs
#'   - "add": Add new IDs to existing access
#'   - "remove": Remove specified IDs from existing access
#' @param grant_table_privileges Logical. Whether to automatically grant table-level
#'   SELECT, INSERT, UPDATE, DELETE privileges to the user. Default TRUE. These are
#'   required for the RLS policies to work - RLS controls which rows, table privileges
#'   control which operations. Without these privileges, RLS policies have no effect.
#'
#' @returns Invisibly returns TRUE on success, FALSE on failure.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Replace all access with new plots
#' define_user_policy(con, "user1", c(1, 2, 3), operations = "ALL")
#'
#' # Add plots to existing access
#' define_user_policy(con, "user1", c(4, 5), operations = "ALL", mode = "add")
#'
#' # Remove plots from existing access
#' define_user_policy(con, "user1", c(2), operations = "ALL", mode = "remove")
#' }
#'
#' @export
define_user_policy <- function(con, user, ids,
                               table = "data_liste_plots",
                               policy_name = NULL,
                               operations = "SELECT",
                               drop_existing = TRUE,
                               mode = c("replace", "add", "remove"),
                               grant_table_privileges = TRUE) {

  valid_ops <- c("SELECT", "INSERT", "UPDATE", "DELETE", "ALL")
  mode <- match.arg(mode)

  stopifnot(
    "Connection must be valid" = !is.null(con) && test_connection(con),
    "User must be specified" = !is.null(user) && nchar(user) > 0,
    "IDs must be provided" = length(ids) > 0 && all(is.finite(ids)),
    "Operations must be valid" = all(operations %in% valid_ops)
  )

  # Handle add/remove modes by combining with existing IDs
  if (mode %in% c("add", "remove")) {
    existing_access <- get_user_accessible_plots(con, user, table)

    if (is.null(existing_access) || nrow(existing_access) == 0) {
      if (mode == "remove") {
        cli::cli_alert_warning("No existing policies found for user '{user}'. Nothing to remove.")
        return(invisible(FALSE))
      }
      # For "add" mode with no existing policy, treat as replace
      existing_ids <- integer(0)
      cli::cli_alert_info("No existing policy found. Creating new policy with provided IDs.")
    } else {
      # Get all unique plot IDs from existing policies
      existing_ids <- unique(unlist(existing_access$plot_ids))
    }

    if (mode == "add") {
      # Combine existing and new IDs
      ids <- sort(unique(c(existing_ids, ids)))
      cli::cli_alert_info("Adding {length(ids) - length(existing_ids)} new plot IDs to existing {length(existing_ids)} IDs")
    } else if (mode == "remove") {
      # Remove specified IDs from existing
      new_ids <- setdiff(existing_ids, ids)
      removed_count <- length(existing_ids) - length(new_ids)

      if (length(new_ids) == 0) {
        cli::cli_alert_danger("Removing all IDs would leave user with no access. Operation aborted.")
        cli::cli_alert_info("To remove all access, use drop_user_policies() instead.")
        return(invisible(FALSE))
      }

      cli::cli_alert_info("Removing {removed_count} plot IDs from existing {length(existing_ids)} IDs")
      ids <- sort(new_ids)
    }

    # Force drop_existing for add/remove modes since we're updating
    drop_existing <- TRUE
  }

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

    # Grant table-level privileges if requested
    if (grant_table_privileges) {
      sql_grant <- glue::glue("GRANT SELECT, INSERT, UPDATE, DELETE ON {DBI::dbQuoteIdentifier(con, table)} TO {DBI::dbQuoteIdentifier(con, user)};")
      tryCatch({
        DBI::dbExecute(con, sql_grant)
        cli::cli_alert_success("Granted SELECT, INSERT, UPDATE, DELETE privileges on '{table}' to user '{user}'")
      }, error = function(e) {
        cli::cli_alert_warning("Could not grant table privileges to '{user}': {e$message}")
        cli::cli_alert_info("User may need to request these privileges from database admin")
      })
    }

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
      # Create SELECT/UPDATE/DELETE policies restricted to plot IDs
      # Note: INSERT is handled by global 'insert_open' policy (from migration)
      # Note: Creator access is handled by global 'creator_access_*' policies

      # Policy for SELECT: Restricted to specific plot IDs (adds to creator access)
      sql_create_select <- glue::glue("
        CREATE POLICY {DBI::dbQuoteIdentifier(con, paste0(policy_name, '_select'))}
        ON {DBI::dbQuoteIdentifier(con, table)}
        FOR SELECT
        TO {DBI::dbQuoteIdentifier(con, user)}
        USING (id_liste_plots IN ({id_list}));
      ")
      DBI::dbExecute(con, sql_create_select)

      sql_create_update <- glue::glue("
        CREATE POLICY {DBI::dbQuoteIdentifier(con, paste0(policy_name, '_update'))}
        ON {DBI::dbQuoteIdentifier(con, table)}
        FOR UPDATE
        TO {DBI::dbQuoteIdentifier(con, user)}
        USING (id_liste_plots IN ({id_list}));
      ")
      DBI::dbExecute(con, sql_create_update)

      sql_create_delete <- glue::glue("
        CREATE POLICY {DBI::dbQuoteIdentifier(con, paste0(policy_name, '_delete'))}
        ON {DBI::dbQuoteIdentifier(con, table)}
        FOR DELETE
        TO {DBI::dbQuoteIdentifier(con, user)}
        USING (id_liste_plots IN ({id_list}));
      ")
      DBI::dbExecute(con, sql_create_delete)

      cli::cli_alert_success("Policy '{policy_name}_select/update/delete' created for SELECT/UPDATE/DELETE operations")
      cli::cli_alert_info("User can access plot IDs: {paste(ids, collapse = ', ')} (plus any plots they created)")

    } else {
      # Handle specific operations
      # Note: INSERT is skipped - handled by global 'insert_open' policy
      # Note: Creator access is handled by global 'creator_access_*' policies
      ops_to_create <- setdiff(operations, "INSERT")

      if ("INSERT" %in% operations) {
        cli::cli_alert_info("INSERT skipped - handled by global 'insert_open' policy")
      }

      for (i in seq_along(ops_to_create)) {
        op <- ops_to_create[i]
        current_policy_name <- if (length(ops_to_create) > 1) {
          paste0(policy_name, "_", tolower(op))
        } else {
          policy_name
        }

        # SELECT/UPDATE/DELETE policies: Restricted to plot IDs
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
#'
#' @description
#' Lists row-level security policies from pg_policies system catalog.
#'
#' @param con A database connection object.
#' @param user Character. Filter policies by username (optional).
#' @param table Character. Filter by table name. Default is "data_liste_plots".
#'   Use NULL to see policies on all tables.
#'
#' @returns A data frame with policy information (schemaname, tablename,
#'   policyname, roles, cmd, qual).
#'
#' @export
list_user_policies <- function(con, user = NULL, table = "data_liste_plots") {
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
    # Cast to name type for proper comparison with name[] array
    conditions <- c(conditions, glue::glue("'{user}'::name = ANY(roles)"))
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

#' Get plot IDs accessible to a user
#'
#' @description
#' Extracts the plot IDs that a user has access to based on their
#' row-level security policies on data_liste_plots.
#'
#' @param con A database connection object.
#' @param user Character. The username to check.
#' @param table Character. The table to check policies on.
#'   Default is "data_liste_plots".
#'
#' @returns A data frame with columns:
#'   - user: the username
#'   - cmd: the operation type (SELECT, INSERT, UPDATE, DELETE, ALL)
#'   - plot_ids: vector of accessible plot IDs
#'
#'   Returns NULL if no policies found for the user.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Get plots accessible to user 'klein'
#' get_user_accessible_plots(con, "klein")
#'
#' # Get just the plot IDs as a vector
#' result <- get_user_accessible_plots(con, "klein")
#' plot_ids <- result$plot_ids[[1]]
#' }
#'
#' @export
get_user_accessible_plots <- function(con, user, table = "data_liste_plots") {

  stopifnot(
    "Connection must be valid" = !is.null(con) && test_connection(con),
    "User must be specified" = !is.null(user) && nchar(user) > 0
  )

  result_rows <- list()

  # --- 1. Plots created by the user (creator_access_* policies) ---
  if (table == "data_liste_plots") {
    created_ids <- tryCatch({
      DBI::dbGetQuery(con, sprintf(
        "SELECT id_liste_plots FROM data_liste_plots WHERE created_by = '%s'",
        gsub("'", "''", user)
      ))$id_liste_plots
    }, error = function(e) {
      cli::cli_alert_warning("Could not query created_by plots: {e$message}")
      integer(0)
    })

    if (length(created_ids) > 0) {
      created_ids <- sort(unique(as.integer(created_ids)))
      cli::cli_alert_info("{length(created_ids)} plot(s) created by '{user}' (creator access)")
      result_rows[[length(result_rows) + 1]] <- data.frame(
        user = user,
        policyname = "creator_access (automatic)",
        cmd = "ALL",
        stringsAsFactors = FALSE
      )
      result_rows[[length(result_rows)]]$plot_ids <- list(created_ids)
    }
  }

  # --- 2. Plots from explicit per-user policies (define_user_policy grants) ---
  policies <- list_user_policies(con, user = user, table = table)

  if (nrow(policies) > 0) {
    for (i in seq_len(nrow(policies))) {
      qual <- policies$qual[i]
      cmd  <- policies$cmd[i]

      # Extract plot IDs from the qual expression
      # Handles both ARRAY[...] and IN (...) syntax
      ids <- as.integer(unlist(regmatches(qual, gregexpr("\\d+", qual))))
      ids <- ids[ids > 0]
      ids <- sort(unique(ids))

      if (length(ids) > 0) {
        row_df <- data.frame(
          user = user,
          policyname = policies$policyname[i],
          cmd = cmd,
          stringsAsFactors = FALSE
        )
        row_df$plot_ids <- list(ids)
        result_rows[[length(result_rows) + 1]] <- row_df
      }
    }
  }

  # --- Combine results ---
  if (length(result_rows) == 0) {
    cli::cli_alert_warning("No accessible plots found for user '{user}' on table '{table}'")
    return(NULL)
  }

  result_df <- do.call(rbind, result_rows)

  all_unique <- length(unique(unlist(result_df$plot_ids)))
  cli::cli_alert_success("Found {all_unique} unique plot IDs for user '{user}'")

  return(as_tibble(result_df))
}

#' Define read-only policy for a user
#'
#' @description
#' Convenience wrapper for \code{\link{define_user_policy}} that grants SELECT-only access.
#'
#' @inheritParams define_user_policy
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Grant read-only access to plots 1, 2, 3
#' define_read_only_policy(con, "user1", c(1, 2, 3))
#'
#' # Add more plots to existing read access
#' define_read_only_policy(con, "user1", c(4, 5), mode = "add")
#' }
#'
#' @export
define_read_only_policy <- function(con, user, ids, table = "data_liste_plots",
                                    mode = c("replace", "add", "remove")) {
  mode <- match.arg(mode)
  define_user_policy(con, user, ids, table, operations = "SELECT", mode = mode)
}

#' Define full access policy for a user
#'
#' @description
#' Convenience wrapper for \code{\link{define_user_policy}} that grants ALL operations
#' (SELECT, INSERT, UPDATE, DELETE).
#'
#' @inheritParams define_user_policy
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Grant full access to plots 1, 2, 3
#' define_full_access_policy(con, "user1", c(1, 2, 3))
#'
#' # Add more plots to existing access
#' define_full_access_policy(con, "user1", c(4, 5), mode = "add")
#'
#' # Remove a plot from access
#' define_full_access_policy(con, "user1", c(2), mode = "remove")
#' }
#'
#' @export
define_full_access_policy <- function(con, user, ids, table = "data_liste_plots",
                                      mode = c("replace", "add", "remove")) {
  mode <- match.arg(mode)
  define_user_policy(con, user, ids, table, operations = "ALL", mode = mode)
}

#' Define read-write policy for a user
#'
#' @description
#' Convenience wrapper for \code{\link{define_user_policy}} that grants SELECT, INSERT,
#' and UPDATE operations (but not DELETE).
#'
#' @inheritParams define_user_policy
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#'
#' # Grant read-write access to plots 1, 2, 3
#' define_read_write_policy(con, "user1", c(1, 2, 3))
#'
#' # Add more plots to existing access
#' define_read_write_policy(con, "user1", c(4, 5), mode = "add")
#' }
#'
#' @export
define_read_write_policy <- function(con, user, ids, table = "data_liste_plots",
                                     mode = c("replace", "add", "remove")) {
  mode <- match.arg(mode)
  define_user_policy(con, user, ids, table, operations = c("SELECT", "INSERT", "UPDATE"), mode = mode)
}

#' List all database users and their permissions
#'
#' @description
#' Retrieves comprehensive information about all users in both databases,
#' including their roles, privileges, and row-level security policies.
#'
#' @param con_main Connection to main database (optional). If NULL, will connect automatically.
#' @param con_taxa Connection to taxa database (optional). If NULL, will connect automatically.
#' @param include_system_users Logical. Include system users like postgres? Default FALSE.
#'
#' @returns A list with components:
#'   - `users`: Data frame of all users with their role attributes
#'   - `table_privileges`: Data frame of table-level privileges per user
#'   - `policies`: Data frame of row-level security policies (main db only)
#'   - `accessible_plots`: Data frame summarizing plot access per user
#'
#' @examples
#' \dontrun{
#' # Get all user information
#' user_info <- list_database_users()
#'
#' # View users
#' user_info$users
#'
#' # View who can access which plots
#' user_info$accessible_plots
#'
#' # View detailed table privileges
#' user_info$table_privileges
#' }
#'
#' @export
list_database_users <- function(con_main = NULL, con_taxa = NULL, include_system_users = FALSE) {


  # Connect if needed

  if (is.null(con_main)) {
    con_main <- call.mydb()
  }
  if (is.null(con_taxa)) {
    con_taxa <- call.mydb.taxa()
  }

  # System users to exclude

  system_users <- c("postgres", "pg_monitor", "pg_read_all_settings",
                    "pg_read_all_stats", "pg_stat_scan_tables",
                    "pg_read_server_files", "pg_write_server_files",
                    "pg_execute_server_program", "pg_signal_backend",
                    "pg_checkpoint", "pg_database_owner", "pg_read_all_data",
                    "pg_write_all_data", "pg_create_subscription")

  # Query users from main database
cli::cli_h2("Fetching user information")

  # Get all roles/users
  users_query <- "
    SELECT
      rolname as username,
      rolsuper as is_superuser,
      rolinherit as inherits_privileges,
      rolcreaterole as can_create_roles,
      rolcreatedb as can_create_db,
      rolcanlogin as can_login,
      rolreplication as can_replicate,
      rolconnlimit as connection_limit,
      rolvaliduntil as valid_until
    FROM pg_roles
    ORDER BY rolname
  "

  users_main <- DBI::dbGetQuery(con_main, users_query)
  users_main$database <- "main"

  users_taxa <- DBI::dbGetQuery(con_taxa, users_query)
  users_taxa$database <- "taxa"

  # Combine and filter
  all_users <- rbind(users_main, users_taxa)

  if (!include_system_users) {
    all_users <- all_users[!all_users$username %in% system_users, ]
  }

  # Get unique users (they should be same in both DBs)
  unique_users <- unique(all_users[all_users$can_login == TRUE, c("username", "is_superuser",
                                                                   "can_create_roles", "can_create_db",
                                                                   "connection_limit", "valid_until")])

  cli::cli_alert_success("Found {nrow(unique_users)} users with login capability")

  # Get table privileges for main database
  cli::cli_alert_info("Fetching table privileges...")

  privileges_query <- "
    SELECT
      grantee as username,
      table_schema,
      table_name,
      privilege_type,
      is_grantable
    FROM information_schema.table_privileges
    WHERE table_schema = 'public'
    ORDER BY grantee, table_name, privilege_type
  "

  privileges_main <- DBI::dbGetQuery(con_main, privileges_query)
  privileges_main$database <- "main"

  privileges_taxa <- DBI::dbGetQuery(con_taxa, privileges_query)
  privileges_taxa$database <- "taxa"

  all_privileges <- rbind(privileges_main, privileges_taxa)

  if (!include_system_users) {
    all_privileges <- all_privileges[!all_privileges$username %in% system_users, ]
  }

  # Get RLS policies from main database
  cli::cli_alert_info("Fetching row-level security policies...")

  policies <- list_user_policies(con_main, user = NULL, table = NULL)

  # Build summary of accessible plots per user
  cli::cli_alert_info("Building plot access summary...")

  # Get all users who have policies
  policy_users <- unique(unlist(lapply(policies$roles, function(x) {
    # roles is stored as PostgreSQL array string like {user1,user2}
    gsub("[{}]", "", x)
  })))

  accessible_plots_list <- lapply(policy_users, function(user) {
    tryCatch({
      result <- get_user_accessible_plots(con_main, user)
      if (!is.null(result) && nrow(result) > 0) {
        data.frame(
          username = user,
          n_plots = length(unique(unlist(result$plot_ids))),
          operations = paste(unique(result$cmd), collapse = ", "),
          plot_ids = paste(sort(unique(unlist(result$plot_ids))), collapse = ", "),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }, error = function(e) NULL)
  })

  accessible_plots <- do.call(rbind, accessible_plots_list[!sapply(accessible_plots_list, is.null)])

  if (is.null(accessible_plots)) {
    accessible_plots <- data.frame(
      username = character(),
      n_plots = integer(),
      operations = character(),
      plot_ids = character(),
      stringsAsFactors = FALSE
    )
  }

  # Create privilege summary per user
  cli::cli_alert_info("Creating privilege summary...")

  privilege_summary <- stats::aggregate(
    privilege_type ~ username + database + table_name,
    data = all_privileges,
    FUN = function(x) paste(sort(unique(x)), collapse = ", ")
  )
  names(privilege_summary)[4] <- "privileges"

  cli::cli_alert_success("User information compiled successfully")

  # Print summary
  cli::cli_h2("Summary")
  cli::cli_alert_info("Total users: {nrow(unique_users)}")
  cli::cli_alert_info("Users with plot access: {nrow(accessible_plots)}")
  cli::cli_alert_info("Superusers: {sum(unique_users$is_superuser)}")

  return(list(
    users = tibble::as_tibble(unique_users),
    table_privileges = tibble::as_tibble(privilege_summary),
    policies = tibble::as_tibble(policies),
    accessible_plots = tibble::as_tibble(accessible_plots)
  ))
}

#' Print user access summary
#'
#' @description
#' Prints a formatted summary of user access to the database.
#'
#' @param con Connection to main database (optional).
#'
#' @export
print_user_access_summary <- function(con = NULL) {

  if (is.null(con)) {
    con <- call.mydb()
  }

  cli::cli_h1("Database User Access Summary")

  # Get policies
  policies <- list_user_policies(con, user = NULL, table = "data_liste_plots")

  if (nrow(policies) == 0) {
    cli::cli_alert_warning("No row-level security policies found")
    return(invisible(NULL))
  }

  # Extract users from policies
  policy_users <- unique(unlist(lapply(policies$roles, function(x) {
    gsub("[{}]", "", x)
  })))

  cli::cli_alert_info("Found {length(policy_users)} users with policies on data_liste_plots")
  cli::cli_text("")

  for (user in sort(policy_users)) {
    result <- tryCatch({
      get_user_accessible_plots(con, user)
    }, error = function(e) NULL)

    if (!is.null(result) && nrow(result) > 0) {
      n_plots <- length(unique(unlist(result$plot_ids)))
      ops <- paste(unique(result$cmd), collapse = ", ")
      cli::cli_text("{.strong {user}}: {n_plots} plots ({ops})")
    }
  }
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

    # Check if connection is a pool object
    is_pool <- inherits(con, "Pool")

    while (attempt <= max_attempts && !success) {
      if (verbose)
        cli::cli_alert_info("Attempt {attempt} of {max_attempts}...")

      try_result <- try({
        if (is_pool) {
          # Pool objects require dbGetQuery() directly
          result <- DBI::dbGetQuery(con, sql)
        } else {
          # Regular connections use dbSendQuery/dbFetch/dbClearResult
          rs <- DBI::dbSendQuery(con, sql)
          result <- DBI::dbFetch(rs)
          DBI::dbClearResult(rs)
        }
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

  table_postgre <- NULL
  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({dplyr::tbl(con, table)}, silent = TRUE)

    # Check if result is an error
    if (inherits(res_q, "try-error")) {
      error_msg <- as.character(res_q)

      if (any(grepl("Lost connection to database", error_msg)))
        stop("Lost connection to database")

      if (any(grepl("Failed to prepare query", error_msg))) {
        rep <- TRUE
        cli::cli_alert_warning("---")
        rep_try <- rep_try + 1
      } else {
        # Different error - stop with the error message
        stop(error_msg)
      }
    } else {
      # Success - assign the result and exit loop
      table_postgre <- res_q
      rep <- FALSE
    }

    if (rep_try == 10)
      stop("Failed to connect to database after 10 attempts")
  }

  return(table_postgre)
}


#' Create a connection pool for Shiny apps (main database)
#'
#' @description
#' Creates a connection pool using the pool package. This is the recommended
#' approach for Shiny applications as it automatically manages connections,
#' preventing connection leaks and exhaustion.
#'
#' @param pass Character, database password (optional)
#' @param user Character, database username (optional)
#' @param minSize Integer, minimum number of connections in pool (default: 1)
#' @param maxSize Integer, maximum number of connections in pool (default: 5)
#' @param use_env_credentials Logical, use credentials from .Renviron
#'
#' @return A pool object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In your Shiny app server function:
#' pool_main <- create_pool_main()
#' onStop(function() {
#'   pool::poolClose(pool_main)
#' })
#'
#' # Use the pool in queries:
#' data <- pool::poolWithTransaction(pool_main, function(conn) {
#'   DBI::dbGetQuery(conn, "SELECT * FROM table_name")
#' })
#' }
create_pool_main <- function(pass = NULL, user = NULL, minSize = 1, maxSize = 5, use_env_credentials = FALSE) {
  create_db_config()

  # Get credentials
  if (use_env_credentials && is.null(user) && is.null(pass)) {
    env_user <- Sys.getenv("MYDB_USER")
    env_pass <- Sys.getenv("MYDB_PASS")

    if (env_user != "" && env_pass != "") {
      user <- env_user
      pass <- env_pass
      cli::cli_alert_info("Using stored credentials from environment")
    }
  }

  if (is.null(pass)) {
    if (!exists("password", envir = credentials)) {
      credentials$password <- get_password_secure("Enter database password: ")
    }
    pass <- credentials$password
  }

  if (is.null(user)) {
    if (!exists("user_db", envir = credentials)) {
      credentials$user_db <- get_username_secure("Enter database username: ")
    }
    user <- credentials$user_db
  }

  # Create pool with connection validation for resilience
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db_name,
    host = db_host,
    port = db_port,
    user = user,
    password = pass,
    minSize = minSize,
    maxSize = maxSize,
    # Validate connections every 60 seconds (helps detect stale connections after sleep)
    validationInterval = 60,
    # Validate each connection before use (important after laptop wake from sleep)
    onActivate = function(conn) {
      tryCatch({
        DBI::dbGetQuery(conn, "SELECT 1")
        TRUE
      }, error = function(e) {
        # Connection is stale, return FALSE to force pool to create new connection
        FALSE
      })
    }
  )

  cli::cli_alert_success("Created connection pool for main database (size: {minSize}-{maxSize})")

  return(pool)
}


#' Create a connection pool for Shiny apps (taxa database)
#'
#' @description
#' Creates a connection pool for the taxa database. This is the recommended
#' approach for Shiny applications.
#'
#' @param pass Character, database password (optional)
#' @param user Character, database username (optional)
#' @param minSize Integer, minimum number of connections in pool (default: 1)
#' @param maxSize Integer, maximum number of connections in pool (default: 5)
#' @param use_env_credentials Logical, use credentials from .Renviron
#'
#' @return A pool object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # In your Shiny app server function:
#' pool_taxa <- create_pool_taxa()
#' onStop(function() {
#'   pool::poolClose(pool_taxa)
#' })
#' }
create_pool_taxa <- function(pass = NULL, user = NULL, minSize = 1, maxSize = 5, use_env_credentials = FALSE) {
  create_db_config()

  # Get credentials
  if (use_env_credentials && is.null(user) && is.null(pass)) {
    env_user <- Sys.getenv("MYDB_USER")
    env_pass <- Sys.getenv("MYDB_PASS")

    if (env_user != "" && env_pass != "") {
      user <- env_user
      pass <- env_pass
      cli::cli_alert_info("Using stored credentials from environment")
    }
  }

  if (is.null(pass)) {
    if (!exists("password", envir = credentials)) {
      credentials$password <- get_password_secure("Enter database password: ")
    }
    pass <- credentials$password
  }

  if (is.null(user)) {
    if (!exists("user_db", envir = credentials)) {
      credentials$user_db <- get_username_secure("Enter database username: ")
    }
    user <- credentials$user_db
  }

  # Create pool with connection validation for resilience
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = db_name_taxa,
    host = db_host,
    port = db_port,
    user = user,
    password = pass,
    minSize = minSize,
    maxSize = maxSize,
    # Validate connections every 60 seconds (helps detect stale connections after sleep)
    validationInterval = 60,
    # Validate each connection before use (important after laptop wake from sleep)
    onActivate = function(conn) {
      tryCatch({
        DBI::dbGetQuery(conn, "SELECT 1")
        TRUE
      }, error = function(e) {
        # Connection is stale, return FALSE to force pool to create new connection
        FALSE
      })
    }
  )

  cli::cli_alert_success("Created connection pool for taxa database (size: {minSize}-{maxSize})")
  cli::cli_alert_info("Taxa database: Remember that write operations are restricted")

  return(pool)
}