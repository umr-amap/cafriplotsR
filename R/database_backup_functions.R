#' Backup PostgreSQL database with timestamp
#'
#' @description
#' Creates a timestamped backup of the main or taxa database using PostgreSQL's pg_dump utility.
#' Backup files are named with format: `<database>_backup_YYYY-MM-DD_HH-MM-SS.dump`
#'
#' @param backup_dir Directory where backup files will be stored. Defaults to `~/database_backups`.
#'   Directory will be created if it doesn't exist.
#' @param database Which database to backup: `"main"` (plots_transects) or `"taxa"` (rainbio).
#'   Default is `"main"`.
#' @param compress Logical. If TRUE, creates compressed backup (smaller file size, slower).
#'   Default is TRUE.
#' @param user Database username. If NULL, uses stored credentials.
#' @param password Database password. If NULL, uses stored credentials.
#' @param verbose Logical. If TRUE, shows pg_dump output. Default is TRUE.
#' @param sslmode SSL mode passed to pg_dump via the PGSSLMODE environment variable.
#'   Common values: `"require"`, `"prefer"` (default), `"disable"`.
#'   Set to `"require"` for managed cloud databases (OVH, etc.) that enforce SSL.
#' @param exclude_table_data Character vector of table names whose data should be excluded
#'   from the backup (schema is still included). Useful when a table has FORCE ROW LEVEL
#'   SECURITY that blocks pg_dump. Example: `"taxa_traits_measures"`.
#'   The proper fix is `ALTER TABLE <table> NO FORCE ROW LEVEL SECURITY` on the server.
#'
#' @details
#' This function requires PostgreSQL's `pg_dump` utility to be installed and available in your PATH.
#'
#' The function will:
#' - Connect to the database to retrieve connection parameters
#' - Generate a timestamped filename
#' - Execute pg_dump to create the backup
#' - Return the path to the created backup file
#'
#' **Security Note**: The password is passed via the PGPASSWORD environment variable,
#' which is cleared immediately after use.
#'
#' @returns Character string with the full path to the created backup file.
#'
#' @examples
#' \dontrun{
#' # Backup main database to default location
#' backup_file <- backup_database()
#'
#' # Backup taxa database to specific directory
#' backup_file <- backup_database(
#'   backup_dir = "D:/my_backups",
#'   database = "taxa"
#' )
#'
#' # Backup without compression (faster, larger file)
#' backup_file <- backup_database(compress = FALSE)
#' }
#'
#' @export
backup_database <- function(backup_dir = "~/database_backups",
                            database = c("main", "taxa"),
                            compress = TRUE,
                            user = NULL,
                            password = NULL,
                            verbose = TRUE,
                            sslmode = "prefer",
                            exclude_table_data = NULL) {

  database <- match.arg(database)

  # Check if pg_dump is available
  pg_dump_check <- tryCatch({
    system2("pg_dump", "--version", stdout = TRUE, stderr = TRUE)
  }, error = function(e) NULL)

  if (is.null(pg_dump_check)) {
    cli::cli_abort(c(
      "x" = "pg_dump not found in PATH",
      "i" = "Please install PostgreSQL client tools",
      "i" = "Windows: Install from {.url https://www.postgresql.org/download/windows/}",
      "i" = "macOS: brew install postgresql",
      "i" = "Linux: sudo apt-get install postgresql-client"
    ))
  }

  # Create backup directory if needed
  backup_dir <- path.expand(backup_dir)
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
    cli::cli_alert_success("Created backup directory: {.path {backup_dir}}")
  }

  # Load config to get db parameters
  create_db_config()

  # Get database name
  db_name_backup <- if (database == "main") db_name else db_name_taxa

  # Connect to get credentials if not provided
  if (is.null(user) || is.null(password)) {
    con <- if (database == "main") {
      call.mydb(user = user, pass = password)
    } else {
      call.mydb.taxa(user = user, pass = password)
    }

    # Extract credentials from stored environment
    if (is.null(user) && exists("user_db", envir = credentials)) {
      user <- credentials$user_db
    }
    if (is.null(password) && exists("password", envir = credentials)) {
      password <- credentials$password
    }
  }

  if (is.null(user) || is.null(password)) {
    cli::cli_abort("Could not retrieve database credentials")
  }

  # Generate timestamped filename
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  backup_filename <- sprintf("%s_backup_%s.dump", db_name_backup, timestamp)

  # Normalize path - create directory first if needed
  backup_dir <- path.expand(backup_dir)
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }

  backup_path <- file.path(backup_dir, backup_filename)
  if (.Platform$OS.type == "windows") {
    backup_path <- normalizePath(backup_path, winslash = "/", mustWork = FALSE)
  } else {
    backup_path <- normalizePath(backup_path, mustWork = FALSE)
  }

  cli::cli_alert_info("Starting backup of {.strong {db_name_backup}} database...")
  cli::cli_alert_info("Backup path: {.path {backup_path}}")

  # Build pg_dump command
  args <- c(
    "-h", db_host,
    "-p", as.character(db_port),
    "-U", user,
    "-d", db_name_backup,
    "-F", if (compress) "c" else "p",  # c = custom compressed, p = plain SQL
    "-f", .quote_sys_arg(backup_path)
  )

  if (!is.null(exclude_table_data)) {
    for (tbl in exclude_table_data) {
      args <- c(args, "--exclude-table-data", tbl)
    }
    cli::cli_alert_warning("Excluding table data: {.val {exclude_table_data}}")
  }

  # Set password and SSL mode via environment variables (safer than command line)
  old_pgpass <- Sys.getenv("PGPASSWORD")
  old_sslmode <- Sys.getenv("PGSSLMODE")
  Sys.setenv(PGPASSWORD = password, PGSSLMODE = sslmode)
  on.exit({
    if (old_pgpass == "") Sys.unsetenv("PGPASSWORD") else Sys.setenv(PGPASSWORD = old_pgpass)
    if (old_sslmode == "") Sys.unsetenv("PGSSLMODE") else Sys.setenv(PGSSLMODE = old_sslmode)
  }, add = TRUE)

  # Execute pg_dump - capture stdout/stderr to avoid cmd.exe quoting issues on Windows
  pg_output <- suppressWarnings(system2(
    "pg_dump",
    args = args,
    stdout = TRUE,
    stderr = TRUE
  ))
  result <- attr(pg_output, "status")
  if (is.null(result)) result <- 0

  if (length(pg_output) > 0) {
    cat(paste(pg_output, collapse = "\n"), "\n")
  }

  if (result != 0) {
    cli::cli_abort(c(
      "x" = "Backup failed with exit code {result}",
      "i" = "Check database connection and pg_dump version compatibility"
    ))
  }

  # Get file size
  file_size <- file.info(backup_path)$size
  file_size_mb <- round(file_size / 1024^2, 2)

  cli::cli_alert_success("Backup completed successfully!")
  cli::cli_alert_info("File: {.path {backup_path}}")
  cli::cli_alert_info("Size: {file_size_mb} MB")
  cli::cli_alert_info("Timestamp: {timestamp}")

  invisible(backup_path)
}


#' Export tables blocked by FORCE ROW LEVEL SECURITY via DBI
#'
#' @description
#' Exports one or more tables to CSV files using a DBI connection. Use this alongside
#' `backup_database(exclude_table_data = ...)` to cover tables that pg_dump cannot
#' dump due to `FORCE ROW LEVEL SECURITY` on managed servers (e.g. OVH) where
#' superuser access is unavailable.
#'
#' @param tables Character vector of table names to export.
#' @param backup_dir Directory to write CSV files into.
#' @param con A DBI connection to the main database. If NULL, calls `call.mydb()`.
#' @param timestamp Character. Timestamp string to embed in filenames (defaults to
#'   current time, formatted `YYYY-MM-DD_HH-MM-SS`).
#'
#' @returns Named character vector of CSV file paths, one per table.
#'
#' @examples
#' \dontrun{
#' # Use together with backup_database() when FORCE RLS blocks pg_dump
#' backup_database(
#'   backup_dir          = "D:/my_backups",
#'   sslmode             = "require",
#'   exclude_table_data  = "taxa_traits_measures"
#' )
#' backup_rls_tables(
#'   tables     = "taxa_traits_measures",
#'   backup_dir = "D:/my_backups"
#' )
#' }
#'
#' @export
backup_rls_tables <- function(tables,
                              backup_dir,
                              con = NULL,
                              timestamp = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")) {

  backup_dir <- path.expand(backup_dir)
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }

  if (is.null(con)) {
    con <- call.mydb()
  }

  paths <- stats::setNames(character(length(tables)), tables)

  for (tbl in tables) {
    cli::cli_alert_info("Exporting {.val {tbl}} via DBI...")
    dat <- tryCatch(
      DBI::dbReadTable(con, tbl),
      error = function(e) {
        cli::cli_alert_warning("Could not read {.val {tbl}}: {e$message}")
        return(NULL)
      }
    )
    if (is.null(dat)) next

    csv_path <- file.path(backup_dir, sprintf("%s_backup_%s.csv", tbl, timestamp))
    utils::write.csv(dat, csv_path, row.names = FALSE)

    size_mb <- round(file.info(csv_path)$size / 1024^2, 2)
    cli::cli_alert_success("Exported {nrow(dat)} rows to {.path {csv_path}} ({size_mb} MB)")
    paths[[tbl]] <- csv_path
  }

  invisible(paths)
}


#' List available database backups
#'
#' @description
#' Lists all backup files in the specified directory, showing their timestamps and sizes.
#'
#' @param backup_dir Directory containing backup files. Defaults to `~/database_backups`.
#' @param database Filter backups by database: `"main"`, `"taxa"`, or `"all"`.
#'   Default is `"all"`.
#' @param pattern Custom file pattern to match. Default matches standard backup naming.
#'
#' @returns A data.frame with columns: file, database, timestamp, size_mb, path
#'
#' @examples
#' \dontrun{
#' # List all backups
#' backups <- list_backups()
#'
#' # List only main database backups
#' backups <- list_backups(database = "main")
#'
#' # List backups in custom directory
#' backups <- list_backups(backup_dir = "D:/my_backups")
#' }
#'
#' @export
list_backups <- function(backup_dir = "~/database_backups",
                        database = c("all", "main", "taxa"),
                        pattern = "_backup_.*\\.dump$") {

  database <- match.arg(database)
  backup_dir <- path.expand(backup_dir)

  if (!dir.exists(backup_dir)) {
    cli::cli_alert_warning("Backup directory does not exist: {.path {backup_dir}}")
    return(data.frame(
      file = character(),
      database = character(),
      timestamp = character(),
      size_mb = numeric(),
      path = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Find all backup files
  all_files <- list.files(backup_dir, pattern = pattern, full.names = TRUE)

  if (length(all_files) == 0) {
    cli::cli_alert_info("No backup files found in {.path {backup_dir}}")
    return(data.frame(
      file = character(),
      database = character(),
      timestamp = character(),
      size_mb = numeric(),
      path = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Filter by database if specified
  if (database != "all") {
    db_name_filter <- if (database == "main") db_name else db_name_taxa
    all_files <- all_files[grepl(db_name_filter, basename(all_files))]
  }

  if (length(all_files) == 0) {
    cli::cli_alert_info("No {database} database backups found")
    return(data.frame(
      file = character(),
      database = character(),
      timestamp = character(),
      size_mb = numeric(),
      path = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Extract information
  backup_info <- data.frame(
    file = basename(all_files),
    database = ifelse(grepl(db_name, basename(all_files)), "main", "taxa"),
    timestamp = gsub(".*backup_(.+)\\.dump$", "\\1", basename(all_files)),
    size_mb = round(file.info(all_files)$size / 1024^2, 2),
    path = all_files,
    stringsAsFactors = FALSE
  )

  # Sort by timestamp (most recent first)
  backup_info <- backup_info[order(backup_info$timestamp, decreasing = TRUE), ]
  rownames(backup_info) <- NULL

  cli::cli_alert_success("Found {nrow(backup_info)} backup file{?s}")
  print(backup_info[, c("file", "database", "timestamp", "size_mb")])

  invisible(backup_info)
}


#' Restore database from backup
#'
#' @description
#' Restores a PostgreSQL database from a backup file created by `backup_database()`.
#'
#' **WARNING**: This will OVERWRITE the current database! Use with caution.
#'
#' @param backup_file Full path to the backup file (.dump format).
#' @param database Which database to restore to: `"main"` or `"taxa"`.
#'   Must match the database that was backed up.
#' @param user Database username. If NULL, uses stored credentials.
#' @param password Database password. If NULL, uses stored credentials.
#' @param clean If TRUE, drops existing database objects before restoring.
#'   Default is FALSE (safer).
#' @param confirm If TRUE, requires interactive confirmation before restoring.
#'   Default is TRUE.
#' @param verbose Logical. If TRUE, shows pg_restore output. Default is TRUE.
#'
#' @details
#' This function requires PostgreSQL's `pg_restore` utility to be installed and available in your PATH.
#'
#' **IMPORTANT**: This operation will overwrite data in the target database.
#' Always verify you're restoring to the correct database and have a recent backup.
#'
#' @returns Logical. TRUE if restore was successful, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' # List available backups first
#' backups <- list_backups()
#'
#' # Restore from most recent backup
#' restore_database(
#'   backup_file = backups$path[1],
#'   database = "main"
#' )
#'
#' # Restore with clean option (drops existing objects first)
#' restore_database(
#'   backup_file = "~/database_backups/plots_transects_backup_2026-02-13.dump",
#'   database = "main",
#'   clean = TRUE
#' )
#' }
#'
#' @export
restore_database <- function(backup_file,
                             database = c("main", "taxa"),
                             user = NULL,
                             password = NULL,
                             clean = FALSE,
                             confirm = TRUE,
                             verbose = TRUE) {

  database <- match.arg(database)

  # Check if backup file exists
  if (!file.exists(backup_file)) {
    cli::cli_abort("Backup file not found: {.path {backup_file}}")
  }

  # Check if pg_restore is available
  pg_restore_check <- tryCatch({
    system2("pg_restore", "--version", stdout = TRUE, stderr = TRUE)
  }, error = function(e) NULL)

  if (is.null(pg_restore_check)) {
    cli::cli_abort(c(
      "x" = "pg_restore not found in PATH",
      "i" = "Please install PostgreSQL client tools"
    ))
  }

  # Load config
  create_db_config()
  db_name_restore <- if (database == "main") db_name else db_name_taxa

  # Warning message
  cli::cli_alert_danger("WARNING: This will OVERWRITE the current {.strong {db_name_restore}} database!")
  cli::cli_alert_warning("Backup file: {.path {backup_file}}")

  # Confirmation
  if (confirm && interactive()) {
    response <- readline("Type 'YES' to confirm restore: ")
    if (response != "YES") {
      cli::cli_alert_info("Restore cancelled")
      return(invisible(FALSE))
    }
  }

  # Get credentials
  if (is.null(user) || is.null(password)) {
    con <- if (database == "main") {
      call.mydb(user = user, pass = password)
    } else {
      call.mydb.taxa(user = user, pass = password)
    }

    if (is.null(user) && exists("user_db", envir = credentials)) {
      user <- credentials$user_db
    }
    if (is.null(password) && exists("password", envir = credentials)) {
      password <- credentials$password
    }
  }

  if (is.null(user) || is.null(password)) {
    cli::cli_abort("Could not retrieve database credentials")
  }

  cli::cli_alert_info("Starting restore of {.strong {db_name_restore}} database...")

  # Normalize backup file path
  if (.Platform$OS.type == "windows") {
    backup_file <- normalizePath(backup_file, winslash = "/", mustWork = TRUE)
  } else {
    backup_file <- normalizePath(backup_file, mustWork = TRUE)
  }

  cli::cli_alert_info("Restore file: {.path {backup_file}}")

  # Build pg_restore command
  args <- c(
    "-h", db_host,
    "-p", as.character(db_port),
    "-U", user,
    "-d", db_name_restore,
    "-F", "c"  # Custom format
  )

  if (clean) {
    args <- c(args, "--clean")
    cli::cli_alert_warning("Using --clean option: will drop existing objects")
  }

  # Add backup file path
  args <- c(args, .quote_sys_arg(backup_file))

  # Set password via environment variable
  old_pgpass <- Sys.getenv("PGPASSWORD")
  Sys.setenv(PGPASSWORD = password)
  on.exit({
    if (old_pgpass == "") {
      Sys.unsetenv("PGPASSWORD")
    } else {
      Sys.setenv(PGPASSWORD = old_pgpass)
    }
  }, add = TRUE)

  # Execute pg_restore - capture stdout/stderr to avoid cmd.exe quoting issues on Windows
  pg_output <- system2(
    "pg_restore",
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )
  result <- attr(pg_output, "status")
  if (is.null(result)) result <- 0
  if (verbose && length(pg_output) > 0) {
    cat(paste(pg_output, collapse = "\n"), "\n")
  }

  if (result != 0) {
    cli::cli_alert_warning("Restore completed with warnings (exit code {result})")
    cli::cli_alert_info("This is often normal if some objects already exist")
    return(invisible(TRUE))
  }

  cli::cli_alert_success("Database restore completed successfully!")

  invisible(TRUE)
}


#' Delete old database backups
#'
#' @description
#' Removes backup files older than a specified number of days.
#'
#' @param backup_dir Directory containing backup files. Defaults to `~/database_backups`.
#' @param days_to_keep Number of days to keep backups. Backups older than this will be deleted.
#'   Default is 30 days.
#' @param database Filter by database: `"main"`, `"taxa"`, or `"all"`.
#'   Default is `"all"`.
#' @param dry_run If TRUE, shows what would be deleted without actually deleting.
#'   Default is TRUE.
#'
#' @returns Integer. Number of files deleted (or that would be deleted in dry_run mode).
#'
#' @examples
#' \dontrun{
#' # Preview what would be deleted (dry run)
#' cleanup_old_backups(days_to_keep = 30, dry_run = TRUE)
#'
#' # Actually delete old backups
#' cleanup_old_backups(days_to_keep = 30, dry_run = FALSE)
#'
#' # Delete only main database backups older than 7 days
#' cleanup_old_backups(database = "main", days_to_keep = 7, dry_run = FALSE)
#' }
#'
#' @export
cleanup_old_backups <- function(backup_dir = "~/database_backups",
                               days_to_keep = 30,
                               database = c("all", "main", "taxa"),
                               dry_run = TRUE) {

  database <- match.arg(database)
  backup_dir <- path.expand(backup_dir)

  if (!dir.exists(backup_dir)) {
    cli::cli_alert_warning("Backup directory does not exist: {.path {backup_dir}}")
    return(invisible(0))
  }

  # Get all backups
  backups <- list_backups(backup_dir = backup_dir, database = database)

  if (nrow(backups) == 0) {
    cli::cli_alert_info("No backups found")
    return(invisible(0))
  }

  # Calculate cutoff date
  cutoff_date <- Sys.time() - (days_to_keep * 24 * 60 * 60)

  # Get file modification times
  file_times <- file.info(backups$path)$mtime
  old_files <- backups$path[file_times < cutoff_date]

  if (length(old_files) == 0) {
    cli::cli_alert_info("No backups older than {days_to_keep} days found")
    return(invisible(0))
  }

  if (dry_run) {
    cli::cli_alert_info("DRY RUN: Would delete {length(old_files)} backup file{?s}:")
    for (f in old_files) {
      file_age <- round(as.numeric(difftime(Sys.time(), file.info(f)$mtime, units = "days")))
      cli::cli_alert_info("  - {.path {basename(f)}} ({file_age} days old)")
    }
    cli::cli_alert_warning("Run with dry_run = FALSE to actually delete these files")
  } else {
    cli::cli_alert_warning("Deleting {length(old_files)} backup file{?s}...")
    unlink(old_files)
    cli::cli_alert_success("Deleted {length(old_files)} old backup file{?s}")
  }

  invisible(length(old_files))
}


#' Quote a command-line argument for `system2()`
#'
#' @description
#' `system2()` pastes its `args` into a single command line without quoting, so
#' any argument containing a space (a Windows path such as
#' `D:/Mes Donnees/backups`) is split into several arguments by the shell.
#' This helper quotes an argument only when it needs it.
#'
#' @param x Character string. The argument to quote.
#'
#' @returns Character string, quoted if it contains whitespace.
#'
#' @keywords internal
#' @noRd
.quote_sys_arg <- function(x) {
  if (length(x) != 1 || is.na(x) || !grepl("[[:space:]]", x)) {
    return(x)
  }
  if (.Platform$OS.type == "windows") {
    shQuote(x, type = "cmd")
  } else {
    shQuote(x)
  }
}
