# safe_delete_subplot_obs_features(): data_subplot_feat is mocked through DBI

mock_obs_db <- function(env) {
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE,
    choose_prompt = function(...) env$confirm,
    .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(con, sql) {
      env$select <- gsub("[[:space:]]+", " ", sql)
      data.frame(
        id_subplot_feat = c(101L, 102L, 103L),
        id_sub_plots = 10L,
        type = c("additional_people", "additional_people", "team_leader"),
        typevalue = c(5, 6, 7),
        typevalue_char = NA_character_,
        colnam = c("Person A", "Person B", "Person C")
      )
    },
    dbBegin = function(con) invisible(TRUE),
    dbCommit = function(con) invisible(TRUE),
    dbRollback = function(con) invisible(TRUE),
    dbExecute = function(con, sql) {
      env$delete <- sql
      ids <- sub(".*IN [(](.*)[)]", "\\1", sql)
      length(strsplit(ids, ",")[[1]])
    },
    .env = parent.frame()
  )
}

con <- structure(list(), class = "mock_connection")

test_that("safe_delete_subplot_obs_features validates its inputs", {
  expect_error(safe_delete_subplot_obs_features(con = con), "at least one")
  expect_error(
    safe_delete_subplot_obs_features(id_subplot_feat = "1; DROP", con = con),
    "whole numbers"
  )
  expect_error(
    safe_delete_subplot_obs_features(subplot_ids = 1.5, con = con),
    "whole numbers"
  )
})

test_that("dry run reports the records and deletes nothing", {
  env <- new.env()
  mock_obs_db(env)

  res <- safe_delete_subplot_obs_features(
    id_subplot_feat = c(101L, 102L, 103L), con = con, verbose = FALSE
  )

  expect_match(env$select, "sf.id_subplot_feat IN (101,102,103)", fixed = TRUE)
  expect_null(env$delete)
  expect_equal(res$counts$subplot_obs_features, 3L)
  expect_false(res$success)
})

test_that("types filters the records of the given subplots", {
  env <- new.env()
  env$confirm <- TRUE
  mock_obs_db(env)

  res <- safe_delete_subplot_obs_features(
    subplot_ids = 10L, types = "additional_people",
    con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_match(env$select, "sf.id_sub_plots IN (10)", fixed = TRUE)
  expect_equal(
    env$delete,
    "DELETE FROM data_subplot_feat WHERE id_subplot_feat IN (101,102)"
  )
  expect_true(res$success)
  expect_equal(res$deleted$subplot_obs_features, 2L)
})

test_that("a declined confirmation deletes nothing", {
  env <- new.env()
  env$confirm <- FALSE
  mock_obs_db(env)

  res <- safe_delete_subplot_obs_features(
    id_subplot_feat = 101L, con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_null(env$delete)
  expect_false(res$success)
})
