test_that("register_user creates the registry table and inserts a new user", {
  calls <- list(create_registry = 0L, executed = character())

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE,
    create_user_registry = function(con) {
      calls$create_registry <<- calls$create_registry + 1L
      TRUE
    }
  )
  testthat::local_mocked_bindings(
    .package = "glue",
    glue_sql = function(..., .con = NULL, .sep = "") paste0(..., collapse = .sep)
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbExistsTable = function(con, name) FALSE,
    dbGetQuery = function(con, sql) {
      sql_chr <- as.character(sql)
      if (grepl("pg_roles", sql_chr, fixed = TRUE)) {
        return(data.frame(ok = 1L))
      }
      if (grepl("user_registry", sql_chr, fixed = TRUE)) {
        return(data.frame(username = character()))
      }
      stop(sprintf("Unexpected query: %s", sql_chr))
    },
    dbExecute = function(con, sql) {
      calls$executed <<- c(calls$executed, as.character(sql))
      1L
    }
  )

  result <- register_user(
    con = structure(list(), class = "mock_connection"),
    username = "new_user",
    email = "new_user@example.org",
    institution = "AMAP"
  )

  expect_true(isTRUE(result))
  expect_equal(calls$create_registry, 1L)
  expect_true(any(grepl("INSERT INTO user_registry", calls$executed, fixed = TRUE)))
})

test_that("register_user updates an existing user with provided fields only", {
  executed <- character()

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE
  )
  testthat::local_mocked_bindings(
    .package = "glue",
    glue_sql = function(..., .con = NULL, .sep = "") paste0(..., collapse = .sep)
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbExistsTable = function(con, name) TRUE,
    dbGetQuery = function(con, sql) {
      sql_chr <- as.character(sql)
      if (grepl("pg_roles", sql_chr, fixed = TRUE)) {
        return(data.frame(ok = 1L))
      }
      if (grepl("user_registry", sql_chr, fixed = TRUE)) {
        return(data.frame(username = "existing_user"))
      }
      stop(sprintf("Unexpected query: %s", sql_chr))
    },
    dbExecute = function(con, sql) {
      executed <<- c(executed, as.character(sql))
      1L
    }
  )

  result <- register_user(
    con = structure(list(), class = "mock_connection"),
    username = "existing_user",
    email = "updated@example.org",
    notes = "updated"
  )

  expect_true(isTRUE(result))
  expect_equal(length(executed), 1L)
  expect_true(grepl("UPDATE user_registry SET", executed[[1]], fixed = TRUE))
  expect_true(grepl("email", executed[[1]], fixed = TRUE))
  expect_true(grepl("notes", executed[[1]], fixed = TRUE))
  expect_false(grepl("institution", executed[[1]], fixed = TRUE))
})

test_that("setup_user_permissions applies read-only grants and plot-scoped SELECT policies", {
  executed <- character()
  policy_call <- NULL

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE,
    register_user = function(...) TRUE,
    define_user_policy = function(con, user, ids, table = "data_liste_plots", operations, mode = "replace") {
      policy_call <<- list(user = user, ids = ids, operations = operations, mode = mode, table = table)
      TRUE
    }
  )
  testthat::local_mocked_bindings(
    .package = "glue",
    glue_sql = function(..., .con = NULL, .sep = "") paste0(..., collapse = .sep),
    glue = function(..., .sep = "") paste0(..., collapse = .sep)
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(con, sql) {
      sql_chr <- as.character(sql)
      if (grepl("pg_roles", sql_chr, fixed = TRUE)) {
        return(data.frame(ok = 1L))
      }
      stop(sprintf("Unexpected query: %s", sql_chr))
    },
    dbExecute = function(con, sql) {
      executed <<- c(executed, as.character(sql))
      1L
    }
  )

  result <- setup_user_permissions(
    con_main = structure(list(name = "main"), class = "mock_connection"),
    con_taxa = structure(list(name = "taxa"), class = "mock_connection"),
    username = "reader",
    main_db_access = "read_only",
    taxa_db_access = "read_only",
    plot_ids = c(10L, 11L)
  )

  expect_true(isTRUE(result$registry))
  expect_true(isTRUE(result$main_db))
  expect_true(isTRUE(result$taxa_db))
  expect_true(isTRUE(result$rls_policies))
  expect_equal(length(executed), 4L)
  expect_true(all(grepl("GRANT ", executed, fixed = TRUE)))
  expect_equal(policy_call$user, "reader")
  expect_equal(policy_call$ids, c(10L, 11L))
  expect_equal(policy_call$operations, "SELECT")
})
