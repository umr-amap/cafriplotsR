# Tests for targeted helpers in R/updates_tables_functions.R

test_that("calculate_similarities ranks the best match first", {
  compared <- tibble::tibble(
    id = 1:3,
    name = c("Gabon", "Cameroon", "Congo")
  )

  result <- calculate_similarities("gabon", compared, "name")

  expect_equal(result$name[[1]], "Gabon")
  expect_true(result$similarity[[1]] >= result$similarity[[2]])
  expect_true("similarity" %in% names(result))
})

test_that("find_similar_strings applies the threshold filter", {
  compared <- tibble::tibble(
    id = 1:3,
    name = c("Gabon", "Cameroon", "Congo")
  )

  result <- find_similar_strings("gab", compared, "name", threshold = 0.7)

  expect_true(all(result$similarity >= 0.7))
  expect_true("Gabon" %in% result$name)
})

test_that("handle_grep_search returns NULL for empty pattern and no matches", {
  full_table <- tibble::tibble(id = 1:2, label = c("alpha", "beta"))

  testthat::local_mocked_bindings(
    .package = 'base',
    readline = function(prompt = '') ''
  )
  expect_null(handle_grep_search(full_table, "label", "id", "orig", FALSE, "tbl", NULL))

  calls <- 0L
  testthat::local_mocked_bindings(
    .package = 'base',
    readline = function(prompt = '') {
      calls <<- calls + 1L
      if (calls == 1L) 'zzz' else ''
    }
  )
  expect_null(handle_grep_search(full_table, "label", "id", "orig", FALSE, "tbl", NULL))
})

test_that("handle_grep_search delegates matching rows to interactive_selection_loop", {
  full_table <- tibble::tibble(id = 1:3, label = c("alpha", "beta", "alphabet"))

  testthat::local_mocked_bindings(
    .package = 'base',
    readline = function(prompt = '') 'alp'
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    interactive_selection_loop = function(value_to_search, initial_suggestions, full_table, column_name, id_column, allow_add, table_name, con) {
      expect_equal(value_to_search, 'orig')
      expect_equal(nrow(initial_suggestions), 2)
      expect_true(all(c('alpha', 'alphabet') %in% initial_suggestions$label))
      42L
    }
  )

  result <- handle_grep_search(full_table, "label", "id", "orig", FALSE, "tbl", NULL)
  expect_equal(result, 42L)
})

test_that("detect_direct_changes returns only changed direct values", {
  new_data <- tibble::tibble(
    id = c(1L, 2L, 3L),
    plot_name = c("P1-new", "P2", NA_character_),
    country = c("Gabon", "Cameroon", "Congo")
  )
  current_data <- tibble::tibble(
    id = c(1L, 2L, 3L),
    plot_name = c("P1", "P2", "P3"),
    country = c("Gabon", "Cameroon", "Congo")
  )
  config <- list(id_column = "id", table = "data_liste_plots")

  testthat::local_mocked_bindings(
    .package = 'dplyr',
    tbl = function(con, table) current_data,
    collect = function(x, ...) x
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    .display_changes_grouped = function(changes_df, id_column, max_display = 10) invisible(changes_df)
  )

  result <- detect_direct_changes(new_data, columns = c("plot_name", "country"), config = config, con = structure(list(), class = 'mock_con'))

  expect_s3_class(result, 'tbl_df')
  expect_equal(nrow(result), 2)
  expect_equal(sort(unique(result$id)), c(1L, 3L))
  expect_true(all(result$column == 'plot_name'))
  expect_true(any(is.na(result$new_value)))
})

test_that("detect_direct_changes returns NULL when there are no differences", {
  data <- tibble::tibble(id = c(1L, 2L), country = c("Gabon", "Cameroon"))
  config <- list(id_column = "id", table = "data_liste_plots")

  testthat::local_mocked_bindings(
    .package = 'dplyr',
    tbl = function(con, table) data,
    collect = function(x, ...) x
  )

  expect_null(detect_direct_changes(data, columns = "country", config = config, con = structure(list(), class = 'mock_con')))
})

test_that("detect_feature_changes flags census-specific features immediately", {
  data <- tibble::tibble(id = 1L, height_census_2 = 12)
  config <- list(id_column = 'id')

  result <- detect_feature_changes(data, feature_columns = 'height_census_2', config = config, table_type = 'individuals', con = NULL)

  expect_true('height_census_2' %in% names(result))
  expect_equal(result$height_census_2$error, 'census_specific')
  expect_equal(result$height_census_2$base_feature, 'height')
  expect_equal(result$height_census_2$census, '2')
})
