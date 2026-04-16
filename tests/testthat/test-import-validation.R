# Tests for R/import_validation.R
# .validate_required_fields() is pure validation logic, no DB required

# =============================================================================
# .validate_required_fields()
# =============================================================================

test_that(".validate_required_fields returns empty list when all required fields present", {
  data <- data.frame(
    plot_name = c("P1", "P2"),
    method    = c("transect", "plot"),
    country   = c("Gabon", "Cameroon"),
    stringsAsFactors = FALSE
  )

  errors <- .validate_required_fields(data, c("plot_name", "method", "country"))
  expect_equal(length(errors), 0)
})

test_that(".validate_required_fields detects missing column", {
  data <- data.frame(
    plot_name = c("P1", "P2"),
    stringsAsFactors = FALSE
  )

  errors <- .validate_required_fields(data, c("plot_name", "method"))
  expect_true(length(errors) > 0)

  # The error should mention the missing column
  error_messages <- sapply(errors, function(e) e$message)
  expect_true(any(grepl("method", error_messages)))
  expect_true(any(grepl("missing", error_messages, ignore.case = TRUE)))
})

test_that(".validate_required_fields detects NA values in required columns", {
  data <- data.frame(
    plot_name = c("P1", NA, "P3"),
    method    = c("transect", "plot", "transect"),
    stringsAsFactors = FALSE
  )

  errors <- .validate_required_fields(data, c("plot_name", "method"))
  expect_true(length(errors) > 0)

  # Should report the specific row with NA
  na_errors <- Filter(function(e) e$column == "plot_name", errors)
  expect_true(length(na_errors) > 0)
  expect_equal(na_errors[[1]]$row, 2)
})

test_that(".validate_required_fields detects empty strings in required columns", {
  data <- data.frame(
    plot_name = c("P1", "", "P3"),
    method    = c("transect", "plot", "  "),  # whitespace-only
    stringsAsFactors = FALSE
  )

  errors <- .validate_required_fields(data, c("plot_name", "method"))

  # Row 2 plot_name is empty, row 3 method is whitespace-only
  expect_true(length(errors) >= 2)

  # Check that both problematic rows are identified
  error_rows <- sapply(errors, function(e) e$row)
  expect_true(2 %in% error_rows)  # empty plot_name
  expect_true(3 %in% error_rows)  # whitespace method
})

test_that(".validate_required_fields returns correct error structure", {
  data <- data.frame(
    plot_name = c(NA),
    stringsAsFactors = FALSE
  )

  errors <- .validate_required_fields(data, c("plot_name", "missing_col"))

  # Each error should have: column, row, message, value
  for (err in errors) {
    expect_true("column" %in% names(err))
    expect_true("row" %in% names(err))
    expect_true("message" %in% names(err))
    expect_true("value" %in% names(err))
  }
})

test_that(".validate_required_fields handles empty required_columns", {
  data <- data.frame(x = 1:3)
  errors <- .validate_required_fields(data, character(0))
  expect_equal(length(errors), 0)
})

test_that(".validate_required_fields handles multiple missing columns", {
  data <- data.frame(x = 1:3, stringsAsFactors = FALSE)

  errors <- .validate_required_fields(data, c("plot_name", "method", "country"))

  # Should have one error per missing column
  missing_cols <- sapply(errors, function(e) e$column)
  expect_true("plot_name" %in% missing_cols)
  expect_true("method" %in% missing_cols)
  expect_true("country" %in% missing_cols)

  # Missing column errors have row = NA
  for (err in errors) {
    expect_true(is.na(err$row))
  }
})

test_that("validate_plot_metadata renames mapped columns, drops skipped columns, and applies lookup fixes", {
  data <- data.frame(
    Plot = c("P1", "P2"),
    Method = c("transect", "transect"),
    Country = c("Gabon", "Gabon"),
    IgnoreMe = c("x", "y"),
    stringsAsFactors = FALSE
  )

  column_mappings <- c(Plot = "plot_name", Method = "method", Country = "country", IgnoreMe = NA)
  config <- list(required_columns = c("plot_name", "method", "country"))

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    .validate_column_types = function(data, config, con) {
      list(errors = list(), warnings = list())
    },
    .validate_ranges = function(data, config, con) {
      list(errors = list(), warnings = list())
    },
    .validate_lookup_values_interactive = function(data, config, con, interactive, fix_on_fly) {
      cleaned <- data
      cleaned$country[2] <- "Cameroon"
      list(
        errors = list(),
        warnings = list(),
        cleaned_data = cleaned,
        changes_made = data.frame(
          column = "country",
          row = 2L,
          original_value = "Gabon",
          corrected_value = "Cameroon",
          method = "interactive",
          stringsAsFactors = FALSE
        )
      )
    },
    .validate_unique_constraints = function(data, config, con) list(),
    .check_duplicate_plots = function(data, con) {
      list(
        warnings = list(list(column = "plot_name", row = 1L, message = "Possible duplicate", value = "P1")),
        errors = list()
      )
    }
  )

  result <- validate_plot_metadata(
    data = data,
    column_mappings = column_mappings,
    config = config,
    con = structure(list(), class = "mock_con"),
    interactive = FALSE,
    fix_on_fly = FALSE
  )

  expect_s3_class(result, "plot_validation_result")
  expect_true(result$valid)
  expect_false("IgnoreMe" %in% names(result$cleaned_data))
  expect_equal(names(result$cleaned_data), c("plot_name", "method", "country"))
  expect_equal(result$cleaned_data$country[2], "Cameroon")
  expect_equal(nrow(result$changes_made), 1)
  expect_equal(nrow(result$warnings), 1)
  expect_equal(result$summary$mapped_columns, 4)
  expect_equal(result$summary$changes_applied, 1)
})

test_that("validate_plot_metadata strict mode turns warnings into invalid result", {
  data <- data.frame(plot_name = "P1", method = "transect", country = "Gabon", stringsAsFactors = FALSE)
  config <- list(required_columns = c("plot_name", "method", "country"))

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    .validate_column_types = function(data, config, con) {
      list(errors = list(), warnings = list(list(column = "country", row = 1L, message = "warning", value = "Gabon")))
    },
    .validate_ranges = function(data, config, con) {
      list(errors = list(), warnings = list())
    },
    .validate_lookup_values_interactive = function(data, config, con, interactive, fix_on_fly) {
      list(errors = list(), warnings = list(), cleaned_data = data, changes_made = data.frame())
    },
    .validate_unique_constraints = function(data, config, con) list(),
    .check_duplicate_plots = function(data, con) list(warnings = list(), errors = list())
  )

  result <- validate_plot_metadata(
    data = data,
    column_mappings = c(plot_name = "plot_name", method = "method", country = "country"),
    config = config,
    con = structure(list(), class = "mock_con"),
    strict = TRUE,
    interactive = FALSE,
    fix_on_fly = FALSE
  )

  expect_false(result$valid)
  expect_equal(nrow(result$errors), 0)
  expect_equal(nrow(result$warnings), 1)
})

test_that(".validate_column_types flags invalid numeric and coordinate values", {
  data <- data.frame(
    plot_area = c("10.5", "oops"),
    elevation = c(10000, 500),
    ddlat = c(95, 10),
    ddlon = c(20, 200),
    stringsAsFactors = FALSE
  )
  config <- list()

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    subplot_list = function(con) {
      data.frame(
        type = "plot_area",
        valuetype = "numeric",
        stringsAsFactors = FALSE
      )
    }
  )

  result <- .validate_column_types(data, config, con = structure(list(), class = "mock_con"))

  error_messages <- vapply(result$errors, function(x) x$message, character(1))
  warning_messages <- vapply(result$warnings, function(x) x$message, character(1))

  expect_true(any(grepl("Value must be numeric", error_messages)))
  expect_true(any(grepl("Latitude must be between -90 and 90", error_messages)))
  expect_true(any(grepl("Longitude must be between -180 and 180", error_messages)))
  expect_true(any(grepl("Elevation outside typical range", warning_messages)))
})

test_that(".validate_ranges uses import rules and subplot min-max rules", {
  data <- data.frame(
    ddlat = c(2000, -91),
    plot_area = c(0.4, 15),
    stringsAsFactors = FALSE
  )
  config <- list(
    import_config = list(
      validation_rules = list(
        ddlat = list(
          type = "numeric",
          min = -90,
          max = 90,
          severity = "error",
          message = "Latitude must be between -90 and 90",
          utm_hint = TRUE
        )
      )
    )
  )

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    subplot_list = function(con) {
      data.frame(
        type = "plot_area",
        minallowedvalue = 0.5,
        maxallowedvalue = 10,
        expectedunit = "ha",
        stringsAsFactors = FALSE
      )
    }
  )

  result <- .validate_ranges(data, config, con = structure(list(), class = "mock_con"))
  error_messages <- vapply(result$errors, function(x) x$message, character(1))

  expect_true(any(grepl("Possible UTM coordinates detected", error_messages, fixed = TRUE)))
  expect_true(any(grepl("below minimum allowed", error_messages)))
  expect_true(any(grepl("exceeds maximum allowed", error_messages)))
})

test_that(".validate_unique_constraints reports duplicates within uploaded data", {
  data <- data.frame(plot_name = c("P1", "P1", "P2"), stringsAsFactors = FALSE)
  errors <- .validate_unique_constraints(data, config = list(), con = NULL)

  expect_length(errors, 1)
  expect_match(errors[[1]]$message, "Duplicate plot_name 'P1'")
  expect_equal(errors[[1]]$row, "1, 2")
})

