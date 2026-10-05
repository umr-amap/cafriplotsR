# safe_delete_plot_features(): data_liste_sub_plots is mocked through DBI

mock_feat_db <- function(env) {
  env$executed <- character()
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE,
    choose_prompt = function(...) env$confirm,
    .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(con, sql) {
      sql <- gsub("[[:space:]]+", " ", sql)
      if (grepl("FROM data_liste_sub_plots sp", sql, fixed = TRUE)) {
        env$select <- sql
        return(data.frame(
          id_sub_plots = c(10L, 11L, 12L),
          id_table_liste_plots = 1L,
          plot_name = "plot_A",
          type = c("census", "soil_sample", "soil_sample"),
          year = c(2020L, NA, NA),
          typevalue = c(1, 4.5, 5.1),
          typevalue_char = NA_character_,
          colnam = NA_character_
        ))
      }
      table <- sub(".*FROM ([a-z_]+) .*", "\\1", sql)
      rows <- env$linked[[table]]
      if (is.null(rows)) rows <- data.frame(id_sub_plots = integer(), n = integer())
      rows
    },
    dbBegin = function(con) invisible(TRUE),
    dbCommit = function(con) invisible(TRUE),
    dbRollback = function(con) {
      env$rolled_back <- TRUE
      invisible(TRUE)
    },
    dbExecute = function(con, sql) {
      env$executed <- c(env$executed, sql)
      ids <- sub(".*IN [(](.*)[)]", "\\1", sql)
      n <- length(strsplit(ids, ",")[[1]])
      if (grepl("^DELETE FROM data_liste_sub_plots", sql) && !is.null(env$short)) {
        n <- n - env$short
      }
      n
    },
    .env = parent.frame()
  )
}

con <- structure(list(), class = "mock_connection")

test_that("safe_delete_plot_features validates its inputs", {
  expect_error(safe_delete_plot_features(con = con), "at least one")
  expect_error(
    safe_delete_plot_features(subplot_ids = "1; DROP", con = con),
    "whole numbers"
  )
  expect_error(
    safe_delete_plot_features(plot_ids = 1.5, con = con),
    "whole numbers"
  )
  expect_error(
    safe_delete_plot_features(subplot_ids = 1, linked_measurements = "delete",
                              con = con),
    "should be one of"
  )
})

test_that("dry run reports the features and what is attached, deletes nothing", {
  env <- new.env()
  env$linked <- list(
    data_subplot_feat = data.frame(id_sub_plots = 10L, n = 3L)
  )
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    subplot_ids = c(10L, 11L, 12L), con = con, verbose = FALSE
  )

  expect_match(env$select, "sp.id_sub_plots IN (10,11,12)", fixed = TRUE)
  expect_length(env$executed, 0)
  expect_equal(res$counts$plot_features, 3L)
  expect_equal(res$counts$subplot_obs_features, 3L)
  expect_equal(res$feat_info$n_obs_features, c(3L, 0L, 0L))
  expect_false(res$blocked)
  expect_false(res$success)
})

test_that("plot_ids and types select the features of a plot", {
  env <- new.env()
  env$confirm <- TRUE
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    plot_ids = 1L, types = "soil_sample",
    con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_match(env$select, "sp.id_table_liste_plots IN (1)", fixed = TRUE)
  expect_equal(env$executed, c(
    "DELETE FROM data_subplot_feat WHERE id_sub_plots IN (11,12)",
    "DELETE FROM data_liste_sub_plots WHERE id_sub_plots IN (11,12)"
  ))
  expect_true(res$success)
  expect_equal(res$deleted$plot_features, 2L)
})

test_that("linked measurements block the deletion by default", {
  env <- new.env()
  env$confirm <- TRUE
  env$linked <- list(
    data_traits_measures = data.frame(id_sub_plots = 10L, n = 250L)
  )
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    subplot_ids = 10L, con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_true(res$blocked)
  expect_false(res$success)
  expect_length(env$executed, 0)
  expect_equal(res$counts$measurements, 250L)
})

test_that("unlink detaches linked measurements before deleting", {
  env <- new.env()
  env$confirm <- TRUE
  env$linked <- list(
    data_traits_measures = data.frame(id_sub_plots = 10L, n = 250L),
    data_ind_measures_feat = data.frame(id_sub_plots = 10L, n = 40L)
  )
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    subplot_ids = 10L, types = "census", linked_measurements = "unlink",
    con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_equal(env$executed, c(
    "DELETE FROM data_subplot_feat WHERE id_sub_plots IN (10)",
    "UPDATE data_traits_measures SET id_sub_plots = NULL WHERE id_sub_plots IN (10)",
    "UPDATE data_ind_measures_feat SET id_sub_plots = NULL WHERE id_sub_plots IN (10)",
    "DELETE FROM data_liste_sub_plots WHERE id_sub_plots IN (10)"
  ))
  expect_true(res$success)
  expect_false(res$blocked)
})

test_that("declining the confirmation deletes nothing", {
  env <- new.env()
  env$confirm <- FALSE
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    subplot_ids = 11L, con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_length(env$executed, 0)
  expect_false(res$success)
})

test_that("a short delete count rolls back the whole transaction", {
  env <- new.env()
  env$confirm <- TRUE
  env$short <- 1L
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    plot_ids = 1L, types = "soil_sample",
    con = con, dry_run = FALSE, verbose = FALSE
  )

  expect_true(env$rolled_back)
  expect_false(res$success)
  expect_match(res$error, "only 1 of 2")
})

test_that("no matching feature returns without touching the database", {
  env <- new.env()
  mock_feat_db(env)

  res <- safe_delete_plot_features(
    plot_ids = 1L, types = "no_such_type", con = con, verbose = FALSE
  )

  expect_null(res$feat_info)
  expect_length(env$executed, 0)
})
