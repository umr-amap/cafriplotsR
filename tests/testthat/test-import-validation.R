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
