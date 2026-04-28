# Tests for configuration functions with DB error handling
# Tests hardcoded paths (no DB) and mocked error paths (fallback behavior)

# =============================================================================
# get_table_columns() - hardcoded paths (no DB needed)
# =============================================================================

test_that("get_table_columns returns hardcoded columns for data_individuals", {
  # This path never touches the database
  result <- get_table_columns("data_individuals", con = NULL)

  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true("plot_name" %in% result)
  expect_true("tag" %in% result)
  expect_true("idtax_n" %in% result)
  expect_true("original_tax_name" %in% result)
})

test_that("get_table_columns returns hardcoded columns for data_liste_plots", {
  result <- get_table_columns("data_liste_plots", con = NULL)

  expect_type(result, "character")
  expect_true("plot_name" %in% result)
  expect_true("method" %in% result)
  expect_true("country" %in% result)
  expect_true("ddlat" %in% result)
  expect_true("ddlon" %in% result)
})

test_that("get_table_columns returns hardcoded columns for specimens", {
  result <- get_table_columns("specimens", con = NULL)

  expect_type(result, "character")
  expect_true("id_colnam" %in% result)
  expect_true("colnbr" %in% result)
  expect_true("idtax_n" %in% result)
})

test_that("get_table_columns returns empty character for unknown table with NULL con", {
  # Unknown table + NULL connection -> tryCatch should return character(0) fallback
  result <- get_table_columns("nonexistent_table_xyz", con = NULL)
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

# =============================================================================
# get_metadata_mappings_individuals() - no DB needed
# =============================================================================

test_that("get_metadata_mappings_individuals returns empty list", {
  result <- get_metadata_mappings_individuals(con = NULL)
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

# =============================================================================
# get_metadata_mappings_plots() - hardcoded part (no DB needed for method/country)
# =============================================================================

test_that("get_metadata_mappings_plots always includes method and country", {
  # Even when DB fails, the hardcoded method + country should be returned
  result <- get_metadata_mappings_plots(con = NULL)
  expect_type(result, "list")
  expect_true("method" %in% names(result))
  expect_true("country" %in% names(result))

  # Check structure of method mapping
  expect_true("id_col" %in% names(result$method))
  expect_true("lookup_table" %in% names(result$method))
  expect_equal(result$method$lookup_table, "methodslist")
  expect_equal(result$country$lookup_table, "table_countries")
})

# =============================================================================
# test_connection() - mocked DB
# =============================================================================

test_that("test_connection returns FALSE for NULL connection", {
  expect_false(test_connection(NULL))
})

test_that("test_connection returns FALSE when DB query fails", {
  # Create a fake connection object that will fail
  mock_con <- structure(list(), class = "mock_connection")

  # test_connection uses tryCatch, so any error returns FALSE
  result <- test_connection(mock_con)
  expect_false(result)
})

# =============================================================================
# get_available_subplot_types() - error fallback
# =============================================================================

test_that("get_available_subplot_types returns empty character on error", {
  # NULL connection will cause dplyr::tbl() to fail
  result <- get_available_subplot_types(con = NULL)
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

# =============================================================================
# get_available_individual_features() - error fallback
# =============================================================================

test_that("get_available_individual_features returns empty character on error", {
  result <- get_available_individual_features(con = NULL)
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

test_that("create_db_config writes config file and loads defaults", {
  config_path <- tempfile(fileext = '.R')
  on.exit({
    if (file.exists(config_path)) file.remove(config_path)
    rm(list = intersect(c('db_host', 'db_port', 'db_name', 'db_name_taxa', 'db_connect_timeout', 'db_max_retries'), ls(.GlobalEnv)), envir = .GlobalEnv)
  }, add = TRUE)

  result <- create_db_config(config_path = config_path)

  expect_true(isTRUE(result))
  expect_true(file.exists(config_path))
  expect_equal(get('db_name', envir = .GlobalEnv), 'plots_transects')
  expect_equal(get('db_name_taxa', envir = .GlobalEnv), 'rainbio')
})
test_that("create_db_config falls back to in-memory defaults when file creation fails", {
  # Use an existing *file* as a parent path component — creating a subdirectory
  # inside a file is impossible on all platforms, making cat() fail reliably
  # without relying on platform-specific paths or filesystem permissions.
  parent_file <- withr::local_tempfile()
  writeLines("", parent_file)
  bad_path <- file.path(parent_file, "impossible", "config.R")

  on.exit({
    rm(list = intersect(c('db_host', 'db_port', 'db_name', 'db_name_taxa', 'db_connect_timeout', 'db_max_retries'), ls(.GlobalEnv)), envir = .GlobalEnv)
  }, add = TRUE)

  result <- create_db_config(config_path = bad_path)

  expect_false(isTRUE(result))
  expect_equal(get('db_host', envir = .GlobalEnv), 'dg474899-001.dbaas.ovh.net')
  expect_equal(get('db_port', envir = .GlobalEnv), 35699)
})

test_that("connect_database uses environment credentials and caches successful connection", {
  old_user <- Sys.getenv('MYDB_USER', unset = NA)
  old_pass <- Sys.getenv('MYDB_PASS', unset = NA)
  old_main <- .db_env$mydb
  on.exit({
    if (is.na(old_user)) Sys.unsetenv('MYDB_USER') else Sys.setenv(MYDB_USER = old_user)
    if (is.na(old_pass)) Sys.unsetenv('MYDB_PASS') else Sys.setenv(MYDB_PASS = old_pass)
    .db_env$mydb <- old_main
    rm(list = ls(envir = credentials), envir = credentials)
    rm(list = intersect(c('db_host', 'db_port', 'db_name', 'db_name_taxa'), ls(.GlobalEnv)), envir = .GlobalEnv)
  }, add = TRUE)

  Sys.setenv(MYDB_USER = 'env_user', MYDB_PASS = 'env_pass')
  assign('db_host', 'host.test', envir = .GlobalEnv)
  assign('db_port', 5432, envir = .GlobalEnv)
  assign('db_name', 'main_db', envir = .GlobalEnv)
  assign('db_name_taxa', 'taxa_db', envir = .GlobalEnv)
  .db_env$mydb <- NULL

  calls <- list()

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    create_db_config = function(config_path = NULL) invisible(FALSE),
    test_connection = function(con) FALSE
  )
  testthat::local_mocked_bindings(
    .package = 'RPostgres',
    Postgres = function() 'mock_driver'
  )
  testthat::local_mocked_bindings(
    .package = 'DBI',
    dbConnect = function(drv, dbname, host, port, user, password, connect_timeout) {
      calls <<- append(calls, list(list(
        drv = drv,
        dbname = dbname,
        host = host,
        port = port,
        user = user,
        password = password,
        connect_timeout = connect_timeout
      )))
      structure(list(dbname = dbname, user = user), class = 'mock_connection')
    }
  )

  con <- connect_database('main', use_env_credentials = TRUE, retry = FALSE)

  expect_s3_class(con, 'mock_connection')
  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$dbname, 'main_db')
  expect_equal(calls[[1]]$user, 'env_user')
  expect_equal(calls[[1]]$password, 'env_pass')
  expect_equal(credentials$user_db, 'env_user')
  expect_equal(credentials$password, 'env_pass')
})

test_that("call.mydb returns pool when available and get_connection_info summarizes connections", {
  old_pool <- .db_env$pool_main
  old_main <- .db_env$mydb
  old_taxa <- .db_env$mydb_taxa
  on.exit({
    .db_env$pool_main <- old_pool
    .db_env$mydb <- old_main
    .db_env$mydb_taxa <- old_taxa
  }, add = TRUE)

  pool_obj <- structure(list(id = 'pool'), class = 'Pool')
  .db_env$pool_main <- pool_obj
  expect_identical(call.mydb(), pool_obj)

  .db_env$pool_main <- NULL
  .db_env$mydb <- structure(list(name = 'main'), class = 'mock_connection')
  .db_env$mydb_taxa <- structure(list(name = 'taxa'), class = 'mock_connection')

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    test_connection = function(con) identical(con$name, 'main')
  )
  testthat::local_mocked_bindings(
    .package = 'DBI',
    dbGetQuery = function(con, sql) {
      if (identical(con$name, 'main')) {
        data.frame(database = 'plots_transects', user = 'alice', host = '127.0.0.1', port = 5432)
      } else {
        data.frame(database = 'rainbio', user = 'alice', host = '127.0.0.1', port = 5433)
      }
    }
  )

  info <- get_connection_info()

  expect_equal(info$main$status, 'connected')
  expect_true(info$main$connection_valid)
  expect_equal(info$taxa$status, 'connected')
  expect_false(info$taxa$connection_valid)
})

test_that("cleanup_connections disconnects DBs, closes pools, and clears cached credentials", {
  old_main <- .db_env$mydb
  old_taxa <- .db_env$mydb_taxa
  old_pool_main <- .db_env$pool_main
  old_pool_taxa <- .db_env$pool_taxa
  on.exit({
    .db_env$mydb <- old_main
    .db_env$mydb_taxa <- old_taxa
    .db_env$pool_main <- old_pool_main
    .db_env$pool_taxa <- old_pool_taxa
    rm(list = ls(envir = credentials), envir = credentials)
  }, add = TRUE)

  .db_env$mydb <- structure(list(id = 'main'), class = 'mock_connection')
  .db_env$mydb_taxa <- structure(list(id = 'taxa'), class = 'mock_connection')
  .db_env$pool_main <- structure(list(id = 'pool_main'), class = 'Pool')
  .db_env$pool_taxa <- structure(list(id = 'pool_taxa'), class = 'Pool')
  credentials$user_db <- 'alice'
  credentials$password <- 'secret'

  calls <- character()

  testthat::local_mocked_bindings(
    .package = 'DBI',
    dbDisconnect = function(con) {
      calls <<- c(calls, paste0('disconnect:', con$id))
      TRUE
    }
  )
  testthat::local_mocked_bindings(
    .package = 'pool',
    poolClose = function(pool) {
      calls <<- c(calls, paste0('poolClose:', pool$id))
      TRUE
    }
  )

  cleanup_connections()

  expect_null(.db_env$mydb)
  expect_null(.db_env$mydb_taxa)
  expect_null(.db_env$pool_main)
  expect_null(.db_env$pool_taxa)
  expect_equal(sort(calls), sort(c('disconnect:main', 'disconnect:taxa', 'poolClose:pool_main', 'poolClose:pool_taxa')))
  expect_equal(length(ls(envir = credentials)), 0)
})

test_that("setup_db_credentials writes credentials to .Renviron and removes prior MYDB entries", {
  home_dir <- withr::local_tempdir()
  renviron_path <- file.path(home_dir, '.Renviron')
  writeLines(c('OTHER_VAR=keep', 'MYDB_USER=old', 'MYDB_PASS=oldpass'), renviron_path)

  testthat::local_mocked_bindings(
    .package = 'base',
    interactive = function() FALSE,
    path.expand = function(path) {
      if (identical(path, '~')) home_dir else path
    }
  )

  result <- setup_db_credentials(user = 'new_user', pass = 'new_pass')
  lines <- readLines(renviron_path)

  expect_true(isTRUE(result))
  expect_equal(sum(grepl('^MYDB_USER=', lines)), 1)
  expect_equal(sum(grepl('^MYDB_PASS=', lines)), 1)
  expect_true('MYDB_USER=new_user' %in% lines)
  expect_true('MYDB_PASS=new_pass' %in% lines)
  expect_true('OTHER_VAR=keep' %in% lines)
})

test_that("setup_db_credentials prompts for missing user and password", {
  home_dir <- withr::local_tempdir()
  renviron_path <- file.path(home_dir, '.Renviron')

  testthat::local_mocked_bindings(
    .package = 'base',
    readline = function(prompt = '') {
      expect_equal(prompt, 'Enter database username: ')
      'prompted_user'
    },
    path.expand = function(path) {
      if (identical(path, '~')) home_dir else path
    }
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    get_password_secure = function(prompt) {
      expect_equal(prompt, 'Enter database password: ')
      'prompted_pass'
    }
  )

  result <- setup_db_credentials(user = NULL, pass = NULL)
  lines <- readLines(renviron_path)

  expect_true(isTRUE(result))
  expect_true('MYDB_USER=prompted_user' %in% lines)
  expect_true('MYDB_PASS=prompted_pass' %in% lines)
})

test_that("remove_db_credentials removes stored MYDB entries and clears in-memory cache", {
  home_dir <- withr::local_tempdir()
  renviron_path <- file.path(home_dir, '.Renviron')
  writeLines(c('OTHER_VAR=keep', 'MYDB_USER=user1', 'MYDB_PASS=pass1'), renviron_path)
  credentials$user_db <- 'cached_user'
  credentials$password <- 'cached_pass'
  on.exit(rm(list = ls(envir = credentials), envir = credentials), add = TRUE)

  testthat::local_mocked_bindings(
    .package = 'base',
    path.expand = function(path) {
      if (identical(path, '~')) home_dir else path
    }
  )

  result <- remove_db_credentials()
  lines <- readLines(renviron_path)

  expect_true(isTRUE(result))
  expect_equal(lines, 'OTHER_VAR=keep')
  expect_equal(length(ls(envir = credentials)), 0)
})

test_that("remove_db_credentials returns FALSE when no file or no MYDB entries exist", {
  home_dir <- withr::local_tempdir()
  renviron_path <- file.path(home_dir, '.Renviron')

  testthat::local_mocked_bindings(
    .package = 'base',
    path.expand = function(path) {
      if (identical(path, '~')) home_dir else path
    }
  )

  expect_false(isTRUE(remove_db_credentials()))

  writeLines('OTHER_VAR=keep', renviron_path)
  expect_false(isTRUE(remove_db_credentials()))
})

test_that("get_password_secure uses getPass when interactive and available", {
  testthat::local_mocked_bindings(
    .package = 'base',
    interactive = function() TRUE
  )
  testthat::local_mocked_bindings(
    .package = 'base',
    requireNamespace = function(package, quietly = TRUE) {
      expect_equal(package, 'getPass')
      TRUE
    }
  )
  testthat::local_mocked_bindings(
    .package = 'getPass',
    getPass = function(msg) {
      expect_equal(msg, 'Password:')
      'secret_value'
    }
  )

  expect_equal(get_password_secure('Password:'), 'secret_value')
})

test_that("get_password_secure falls back to readline and errors in non-interactive mode", {
  testthat::local_mocked_bindings(
    .package = 'base',
    interactive = function() TRUE,
    requireNamespace = function(package, quietly = TRUE) FALSE,
    readline = function(prompt = '') {
      expect_match(prompt, 'WARNING: will be visible')
      'visible_secret'
    }
  )

  expect_warning(expect_equal(get_password_secure('Password:'), 'visible_secret'), 'No secure password input available')

  testthat::local_mocked_bindings(
    .package = 'base',
    interactive = function() FALSE
  )
  expect_error(get_password_secure('Password:'), 'Cannot prompt for password')
})

test_that("get_username_secure reads interactively and errors otherwise", {
  testthat::local_mocked_bindings(
    .package = 'base',
    interactive = function() TRUE,
    readline = function(prompt = '') {
      expect_equal(prompt, 'Username:')
      'alice'
    }
  )
  expect_equal(get_username_secure('Username:'), 'alice')

  testthat::local_mocked_bindings(
    .package = 'base',
    interactive = function() FALSE
  )
  expect_error(get_username_secure('Username:'), 'Cannot prompt for username')
})

test_that("check_taxa_permissions handles read-only, writable, and broken connections", {
  testthat::local_mocked_bindings(
    .package = 'DBI',
    dbGetQuery = function(con, sql) data.frame(test = 1),
    dbExecute = function(con, sql) stop('write denied')
  )
  expect_no_error(check_taxa_permissions(structure(list(), class = 'mock_con')))

  calls <- character()
  testthat::local_mocked_bindings(
    .package = 'DBI',
    dbGetQuery = function(con, sql) data.frame(test = 1),
    dbExecute = function(con, sql) {
      calls <<- c(calls, sql)
      1L
    }
  )
  expect_no_error(check_taxa_permissions(structure(list(), class = 'mock_con')))
  expect_equal(length(calls), 2)

  testthat::local_mocked_bindings(
    .package = 'DBI',
    dbGetQuery = function(con, sql) stop('cannot connect')
  )
  expect_no_error(check_taxa_permissions(structure(list(), class = 'mock_con')))
})

test_that("print_connection_status handles connected, broken, error, and disconnected states", {
  infos <- list(
    list(
      main = list(status = 'connected', database = 'plots_transects', user = 'alice', connection_valid = TRUE),
      taxa = list(status = 'disconnected')
    ),
    list(
      main = list(status = 'connected', database = 'plots_transects', user = 'alice', connection_valid = FALSE),
      taxa = list(status = 'error', message = 'boom')
    )
  )
  idx <- 0L

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    get_connection_info = function() {
      idx <<- idx + 1L
      infos[[idx]]
    }
  )

  expect_no_error(print_connection_status())
  expect_no_error(print_connection_status())
})


