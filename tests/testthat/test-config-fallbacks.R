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
