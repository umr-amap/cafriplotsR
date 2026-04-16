# Tests for R/import_column_mapping.R
# Pure string matching functions - no database required

# =============================================================================
# .get_column_synonyms()
# =============================================================================

test_that(".get_column_synonyms returns a named list", {
  synonyms <- .get_column_synonyms()
  expect_type(synonyms, "list")
  expect_true(length(synonyms) > 0)
  expect_true(all(nchar(names(synonyms)) > 0))
})

test_that(".get_column_synonyms includes key column names", {
  synonyms <- .get_column_synonyms()

  # Core columns should be present
  expect_true("plot_name" %in% names(synonyms))
  expect_true("method" %in% names(synonyms))
  expect_true("country" %in% names(synonyms))
  expect_true("ddlat" %in% names(synonyms))
  expect_true("ddlon" %in% names(synonyms))
  expect_true("elevation" %in% names(synonyms))
})

test_that(".get_column_synonyms has expected synonyms for key columns", {
  synonyms <- .get_column_synonyms()

  # latitude synonyms should include common variations
  expect_true("latitude" %in% synonyms$ddlat)
  expect_true("lat" %in% synonyms$ddlat)

  # longitude synonyms
  expect_true("longitude" %in% synonyms$ddlon)

  # country synonyms should include French

  expect_true("pays" %in% synonyms$country)
})

# =============================================================================
# .find_synonym_match()
# =============================================================================

test_that(".find_synonym_match finds exact match on target name", {
  synonyms <- .get_column_synonyms()
  result <- .find_synonym_match("plot_name", synonyms)
  expect_equal(result, "plot_name")
})

test_that(".find_synonym_match finds exact match on synonym", {
  synonyms <- .get_column_synonyms()

  # "latitude" is a synonym for ddlat
  result <- .find_synonym_match("latitude", synonyms)
  expect_equal(result, "ddlat")

  # "pays" is a synonym for country
  result <- .find_synonym_match("pays", synonyms)
  expect_equal(result, "country")
})

test_that(".find_synonym_match handles normalized matching", {
  synonyms <- .get_column_synonyms()

  # Should match despite casing/special chars differences
  result <- .find_synonym_match("plot.name", synonyms)
  expect_false(is.null(result))
})

test_that(".find_synonym_match returns NULL for unrecognized column", {
  synonyms <- .get_column_synonyms()
  result <- .find_synonym_match("completely_unknown_column_xyz", synonyms)
  expect_null(result)
})

test_that(".find_synonym_match skips short synonyms in substring matching", {
  # Short synonyms (< 3 chars like "y", "x") should not cause false positives
  synonyms <- list(
    ddlat = c("y", "latitude"),
    ddlon = c("x", "longitude")
  )

  # "country" contains "y" but should NOT match ddlat via substring
  result <- .find_synonym_match("country", synonyms)
  expect_null(result)
})

test_that(".find_synonym_match prefers longest synonym match", {
  synonyms <- list(
    col_a = c("elev"),
    col_b = c("elevation_meters")
  )

  # "elevation_meters" is longer than "elev", should match col_b
  result <- .find_synonym_match("elevation_meters_asl", synonyms)
  expect_equal(result, "col_b")
})

# =============================================================================
# .fuzzy_match_column()
# =============================================================================

test_that(".fuzzy_match_column finds close matches", {
  schema_cols <- c("plot_name", "ddlat", "ddlon", "elevation", "country")

  # "plotname" is similar to "plot_name"
  result <- .fuzzy_match_column("plotname", schema_cols, threshold = 0.6)
  expect_false(is.null(result))
  expect_equal(result$match, "plot_name")
  expect_true(result$similarity >= 0.6)
})

test_that(".fuzzy_match_column returns NULL below threshold", {
  schema_cols <- c("plot_name", "ddlat", "ddlon")

  result <- .fuzzy_match_column("xyz_random_col", schema_cols, threshold = 0.6)
  expect_null(result)
})

test_that(".fuzzy_match_column respects threshold parameter", {
  schema_cols <- c("elevation")

  # "elev" vs "elevation" - some similarity
  result_low <- .fuzzy_match_column("elev", schema_cols, threshold = 0.3)
  result_high <- .fuzzy_match_column("elev", schema_cols, threshold = 0.99)

  expect_false(is.null(result_low))
  expect_null(result_high)
})

test_that(".fuzzy_match_column returns similarity score", {
  schema_cols <- c("plot_name", "country")

  result <- .fuzzy_match_column("plot_name", schema_cols, threshold = 0.5)
  expect_false(is.null(result))
  expect_true(is.numeric(result$similarity))
  expect_true(result$similarity > 0 && result$similarity <= 1)
})

test_that("get_import_column_routing adds import config and subplot validation rules", {
  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    call.mydb = function() structure(list(), class = "mock_con"),
    get_column_routing = function(table_type, con) {
      list(
        direct_columns = c("plot_name", "method", "country"),
        subplot_features = c("plot_area"),
        feature_columns = character(0)
      )
    },
    .get_column_descriptions = function(con, table_type = "plots") {
      list(plot_name = list(description = "Plot name", category = "Identification"))
    },
    subplot_list = function(con) {
      data.frame(
        type = "plot_area",
        valuetype = "numeric",
        minallowedvalue = 0.5,
        maxallowedvalue = 10,
        expectedunit = "ha",
        stringsAsFactors = FALSE
      )
    }
  )

  config <- get_import_column_routing("plots", con = NULL)

  expect_true("import_config" %in% names(config))
  expect_equal(config$import_config$required_columns, c("plot_name", "method", "country"))
  expect_true("plot_area" %in% names(config$import_config$validation_rules))
  expect_equal(config$import_config$validation_rules$plot_area$min, 0.5)
  expect_equal(config$import_config$validation_rules$plot_area$expectedunit, "ha")
})

test_that("map_user_columns maps exact, synonym, fuzzy, and unmapped columns", {
  user_data <- data.frame(
    plot_name = "P1",
    Latitude = 1.2,
    countri = "Gabon",
    mystery = "x",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  config <- list(
    direct_columns = c("plot_name", "country", "ddlat"),
    subplot_features = character(0),
    feature_columns = character(0),
    import_config = list(column_synonyms = .get_column_synonyms())
  )

  result <- map_user_columns(user_data, config, similarity_threshold = 0.6, interactive = FALSE)

  expect_equal(result$mappings[["plot_name"]], "plot_name")
  expect_equal(result$mappings[["Latitude"]], "ddlat")
  expect_equal(result$methods[["Latitude"]], "synonym")
  expect_equal(result$mappings[["countri"]], "country")
  expect_equal(result$methods[["countri"]], "fuzzy")
  expect_true(is.na(result$mappings[["mystery"]]))
  expect_true("mystery" %in% result$unmapped)
})

test_that("map_user_columns deduplicates competing mappings to same target", {
  user_data <- data.frame(
    plot_name = "P1",
    `plot name` = "P1-alt",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  config <- list(
    direct_columns = c("plot_name"),
    subplot_features = character(0),
    feature_columns = character(0),
    import_config = list(column_synonyms = .get_column_synonyms())
  )

  result <- map_user_columns(user_data, config, interactive = FALSE)

  expect_equal(result$mappings[["plot_name"]], "plot_name")
  expect_true(is.na(result$mappings[["plot name"]]))
  expect_equal(result$methods[["plot name"]], "none")
})


