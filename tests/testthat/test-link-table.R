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

test_that(".find_similar_string ranks the closest matches first", {
  compared <- tibble::tibble(
    comp_value = c("Cameroon", "Gabon", "Congo")
  )

  result <- CafriplotsR:::.find_similar_string("Cameroun", compared, "comp_value")

  expect_equal(result$comp_value[[1]], "Cameroon")
  expect_true(all(diff(result$dist) <= 0))
})

test_that(".find_cat returns an exact match without interactive input", {
  compared <- tibble::tibble(
    id_country = c(10L, 20L),
    country = c("Cameroon", "Gabon")
  )

  testthat::local_mocked_bindings(
    .package = "base",
    readline = function(prompt = "") stop("readline should not be called for exact matches")
  )

  result <- CafriplotsR:::.find_cat(
    value_to_search = "Gabon",
    compared_table = compared,
    column_name = "country"
  )

  expect_equal(result$selected_name, 2L)
  expect_equal(result$sorted_matches$comp_value, c("Cameroon", "Gabon"))
  expect_true(result$sorted_matches$perfect_match[[2]])
})

test_that(".link_table calls call.mydb when no connection is provided", {
  lookup <- tibble::tibble(
    id_method = 1L,
    method = "transect"
  )
  mock_con <- structure(list(name = "created"), class = "mock_con")

  testthat::local_mocked_bindings(
    call.mydb = function() mock_con,
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
    table_name = "methodslist"
  )

  expect_equal(result$id_method, 1L)
})

# ── the table shown by the interactive matching prompt ───────────────────────
#
# When a filter value does not match exactly, .find_cat() prints the near
# misses and asks the user to type a number. The table used to carry two bare
# integer columns -- a row-number column called `ID` and the lookup table's own
# `id_country` / `id_method` -- with nothing saying which was being asked for.

# One page of suggestions, shaped the way .find_cat() builds it.
find_cat_page <- function(rows = 1:10, n = 25) {
  tibble::tibble(
    id_country    = 100L + seq_len(n),
    comp_value    = paste0("Country", seq_len(n)),
    perfect_match = FALSE
  ) %>%
    dplyr::mutate(Choice = dplyr::row_number()) %>%
    dplyr::slice(rows)
}

test_that(".find_cat_display() leads with the column the user must type", {
  d <- CafriplotsR:::.find_cat_display(find_cat_page(), "country")

  expect_equal(names(d)[1], "Choice")
  expect_equal(names(d)[2], "country")
})

test_that(".find_cat_display() gives the searched column its real name back", {
  # `comp_value` is .find_cat()'s internal name for it and means nothing to
  # whoever is being asked to pick a country.
  d <- CafriplotsR:::.find_cat_display(find_cat_page(), "country")

  expect_false("comp_value" %in% names(d))
  expect_true("country" %in% names(d))
  expect_equal(d$country[1], "Country1")
})

test_that(".find_cat_display() hides the internal perfect_match flag", {
  # Always FALSE on this branch: an exact match never reaches the prompt.
  d <- CafriplotsR:::.find_cat_display(find_cat_page(), "country")

  expect_false("perfect_match" %in% names(d))
})

test_that(".find_cat_display() keeps the database id, but not as the choice", {
  # The id is worth seeing; it is just not what the user types.
  d <- CafriplotsR:::.find_cat_display(find_cat_page(), "country")

  expect_true("id_country" %in% names(d))
  expect_false(identical(d$Choice, d$id_country))
})

test_that("the choice numbers continue across pages", {
  # Page two is numbered 11-20, which is why the prompt could not go on
  # saying "type a number (1-10)".
  page2 <- find_cat_page(rows = 11:20)
  d <- CafriplotsR:::.find_cat_display(page2, "country")

  expect_equal(d$Choice, 11:20)
  expect_equal(d$country[1], "Country11")
})

test_that("the choice number indexes the returned match table", {
  # .find_cat() returns sorted_matches and callers slice it by the number the
  # user typed, so Choice must be that position and nothing else.
  all_matches <- find_cat_page(rows = 1:25)
  d <- CafriplotsR:::.find_cat_display(find_cat_page(rows = 11:20), "country")

  typed <- 13L
  expect_equal(d$country[d$Choice == typed], all_matches$comp_value[typed])
})

test_that(".find_cat_display() passes an empty page through untouched", {
  empty <- find_cat_page()[0, ]
  expect_equal(nrow(CafriplotsR:::.find_cat_display(empty, "country")), 0)
})

test_that(".find_cat_display() works for any searched column", {
  page <- tibble::tibble(
    id_trait      = 1:3,
    comp_value    = c("height", "dbh", "wood_density"),
    valuetype     = "numeric",
    perfect_match = FALSE
  ) %>%
    dplyr::mutate(Choice = dplyr::row_number())

  d <- CafriplotsR:::.find_cat_display(page, "trait")

  expect_equal(names(d)[1:2], c("Choice", "trait"))
  expect_true(all(c("id_trait", "valuetype") %in% names(d)))
})
