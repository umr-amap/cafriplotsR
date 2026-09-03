# Tests for the legacy specimen importer and subplot-query compatibility wrapper

test_that("add_specimens standardizes input and returns a dry-run payload", {
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    call.mydb = function() structure(list(), class = "mock_con"),
    call.mydb.taxa = function() structure(list(), class = "mock_con"),
    try_open_postgres_table = function(table, con) {
      expect_equal(table, "table_taxa")
      tibble::tibble(idtax_n = 101L, idtax_good_n = 101L)
    }
  )
  testthat::local_mocked_bindings(
    .package = "dplyr",
    tbl = function(src, from, ...) {
      expect_equal(from, "specimens")
      tibble::tibble(colnbr = character(), id_colnam = integer(), id_specimen = integer())
    }
  )

  result <- suppressWarnings(add_specimens(
    new_data = tibble::tibble(taxon = 101L, number = "A-1", collector = 7L),
    col_names_select = c("taxon", "number", "collector"),
    col_names_corresp = c("idtax_n", "colnbr", "id_colnam"),
    launch_adding_data = FALSE
  ))

  expect_length(result, 1)
  expect_equal(result[[1]]$idtax_n, 101L)
  expect_equal(result[[1]]$colnbr, "A-1")
  expect_equal(result[[1]]$id_colnam, 7L)
  expect_true("suffix" %in% names(result[[1]]))
  expect_true(all(c("data_modif_d", "data_modif_m", "data_modif_y") %in% names(result[[1]])))
})

test_that("add_specimens rejects invalid mappings and taxon identifiers before writing", {
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    call.mydb = function() structure(list(), class = "mock_con"),
    call.mydb.taxa = function() structure(list(), class = "mock_con")
  )

  expect_error(
    add_specimens(tibble::tibble(taxon = 1L), "taxon", c("idtax_n", "extra")),
    "same numbers"
  )
  expect_error(
    add_specimens(tibble::tibble(number = "A-1", collector = 7L), c("number", "collector"), c("colnbr", "id_colnam")),
    "idtax_n column missing"
  )
  expect_error(
    add_specimens(tibble::tibble(taxon = 0L), "taxon", "idtax_n"),
    "idtax_n is NULL"
  )
})

test_that("query_subplots delegates supplied IDs and preserves the legacy result shape", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    query_plot_features = function(plot_ids, subplot_ids, subplot_type, format,
                                   include_subplot_obs_features, con) {
      captured <<- list(plot_ids, subplot_ids, subplot_type, format,
                         include_subplot_obs_features, con)
      list(features_raw = "raw", features_aggregated = "wide", census_info = "census")
    }
  )

  con <- structure(list(), class = "mock_con")

  # The wrapper is superseded, so the call warns; the renaming it exists for
  # must still be exact.
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    result <- query_subplots(
      ids_plots = c(1L, 2L), ids_subplots = 10L, subtype = "census",
      extract_subplots_obs_features = TRUE, verbose = FALSE, con = con
    ),
    class = "lifecycle_warning_deprecated"
  )

  expect_equal(captured, list(c(1L, 2L), 10L, "census", "wide", TRUE, con))
  expect_equal(result, list(all_subplots = "raw", all_subplot_pivot = "wide", census_features = "census"))
})

test_that("query_subplots refuses the plot filters it never applied", {
  # These four were served by an internal helper deleted in 1.9.0; the call
  # site was left behind, so they raised `could not find function` rather than
  # filtering. They are now a deprecation error pointing at query_plots().
  withr::local_options(lifecycle_verbosity = "quiet")
  con <- structure(list(), class = "mock_con")

  for (arg in c("plot_name", "country", "locality_name", "method")) {
    call_args <- list(verbose = FALSE, con = con)
    call_args[[arg]] <- "anything"
    expect_error(
      do.call(query_subplots, call_args),
      "query_plots",
      class = "defunctError"
    )
  }
})

test_that("query_subplots asks for ids rather than fetching every plot", {
  # The removed filter branch used to treat "nothing supplied" as "everything".
  withr::local_options(lifecycle_verbosity = "quiet")
  con <- structure(list(), class = "mock_con")
  expect_error(
    query_subplots(verbose = FALSE, con = con),
    "ids_plots"
  )
})
