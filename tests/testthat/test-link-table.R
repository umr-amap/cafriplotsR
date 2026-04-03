# Tests for R/link_table_functions.R .link_table() helper

test_that(".link_table assigns IDs for exact matches and preserves duplicates", {
  lookup <- tibble::tibble(
    id_method = c(1L, 2L),
    method = c("transect", "plot")
  )

  testthat::local_mocked_bindings(
    try_open_postgres_table = function(table, con) lookup,
    .find_cat = function(...) stop(".find_cat should not be called for exact matches"),
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(method_in = c("transect", "plot", "transect"))

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "method_in",
    column_name = "method",
    id_field = "id_method",
    id_table_name = "id_method",
    db_connection = structure(list(), class = "mock_con"),
    table_name = "methodslist"
  )

  expect_equal(result$id_method, c(1L, 2L, 1L))
  expect_false("name" %in% names(result))
})

test_that(".link_table uses .find_cat for non-exact matches", {
  lookup <- tibble::tibble(
    id_country = c(10L, 20L),
    country = c("Cameroon", "Gabon")
  )

  testthat::local_mocked_bindings(
    try_open_postgres_table = function(table, con) lookup,
    .find_cat = function(value_to_search, compared_table, column_name, field_label = NULL) {
      expect_equal(value_to_search, "Cameroun")
      expect_equal(column_name, "country")
      expect_equal(field_label, "country")

      list(
        selected_name = 1L,
        sorted_matches = tibble::tibble(
          id_country = c(10L, 20L),
          country = c("Cameroon", "Gabon")
        )
      )
    },
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(country_raw = c("Cameroun", "Gabon"))

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "country_raw",
    column_name = "country",
    id_field = "id_country",
    id_table_name = "id_country",
    db_connection = structure(list(), class = "mock_con"),
    table_name = "table_countries"
  )

  expect_equal(result$id_country, c(10L, 20L))
})

test_that(".link_table stores 0 when .find_cat skips a value", {
  lookup <- tibble::tibble(
    id_country = 10L,
    country = "Cameroon"
  )

  testthat::local_mocked_bindings(
    try_open_postgres_table = function(table, con) lookup,
    .find_cat = function(...) {
      list(selected_name = 0L, sorted_matches = lookup)
    },
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(country_raw = "Unknownland")

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "country_raw",
    column_name = "country",
    id_field = "id_country",
    id_table_name = "id_country",
    db_connection = structure(list(), class = "mock_con"),
    table_name = "table_countries"
  )

  expect_equal(result$id_country, 0)
})

test_that(".link_table keeps original value when keep_original_value is TRUE", {
  lookup <- tibble::tibble(
    id_method = 1L,
    method = "transect"
  )

  testthat::local_mocked_bindings(
    try_open_postgres_table = function(table, con) lookup,
    .find_cat = function(...) stop(".find_cat should not be called for exact matches"),
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(method_in = "transect")

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "method_in",
    column_name = "method",
    id_field = "id_method",
    id_table_name = "id_method",
    db_connection = structure(list(), class = "mock_con"),
    table_name = "methodslist",
    keep_original_value = TRUE
  )

  expect_true("original_method" %in% names(result))
  expect_equal(result$original_method, "transect")
})

test_that(".link_table joins keep_columns from lookup table", {
  lookup <- tibble::tibble(
    id_country = c(10L, 20L),
    country = c("Cameroon", "Gabon"),
    iso3 = c("CMR", "GAB")
  )

  testthat::local_mocked_bindings(
    try_open_postgres_table = function(table, con) lookup,
    .find_cat = function(...) stop(".find_cat should not be called for exact matches"),
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(country_raw = c("Cameroon", "Gabon"))

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "country_raw",
    column_name = "country",
    id_field = "id_country",
    id_table_name = "id_country",
    db_connection = structure(list(), class = "mock_con"),
    table_name = "table_countries",
    keep_columns = "iso3"
  )

  expect_equal(result$iso3, c("CMR", "GAB"))
})

test_that(".link_table leaves empty strings unmatched without calling .find_cat", {
  lookup <- tibble::tibble(
    id_country = 10L,
    country = "Cameroon"
  )

  testthat::local_mocked_bindings(
    try_open_postgres_table = function(table, con) lookup,
    .find_cat = function(...) stop(".find_cat should not be called for empty strings"),
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(country_raw = c("", "Cameroon"))

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "country_raw",
    column_name = "country",
    id_field = "id_country",
    id_table_name = "id_country",
    db_connection = structure(list(), class = "mock_con"),
    table_name = "table_countries"
  )

  expect_true(is.na(result$id_country[1]))
  expect_equal(result$id_country[2], 10L)
})

test_that(".link_table uses provided db_connection instead of calling call.mydb", {
  lookup <- tibble::tibble(
    id_method = 1L,
    method = "transect"
  )

  mock_con <- structure(list(name = "passed"), class = "mock_con")

  testthat::local_mocked_bindings(
    call.mydb = function() stop("call.mydb should not be called"),
    try_open_postgres_table = function(table, con) {
      expect_identical(con, mock_con)
      lookup
    },
    .find_cat = function(...) stop(".find_cat should not be called for exact matches"),
    .package = "CafriplotsR"
  )

  data <- tibble::tibble(method_in = "transect")

  result <- CafriplotsR:::.link_table(
    data_stand = data,
    column_searched = "method_in",
    column_name = "method",
    id_field = "id_method",
    id_table_name = "id_method",
    db_connection = mock_con,
    table_name = "methodslist"
  )

  expect_equal(result$id_method, 1L)
})
