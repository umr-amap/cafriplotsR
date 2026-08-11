# Tests for the tag checks in R/import_individuals_validation.R
#
# data_individuals.tag is a PostgreSQL `real`: a single-precision float, not an
# integer and not text. Two consequences drive these tests — a tag that is not
# a number cannot be stored at all, and one above 2^24 is stored as a
# different number. Both used to pass through silently.

# =============================================================================
# .validate_tag_values()
# =============================================================================

test_that("non-numeric tags are reported with the offending values", {
  res <- .validate_tag_values(
    data.frame(tag = c("A12", "101", "12B"), stringsAsFactors = FALSE)
  )
  msg <- paste(unlist(res$errors), collapse = " ")

  expect_length(res$errors, 1)
  expect_match(msg, "rows: 1, 3")
  expect_match(msg, "A12")
})

test_that("a clean numeric column raises nothing", {
  expect_length(.validate_tag_values(data.frame(tag = c(101, 102)))$errors, 0)
})

test_that("fractional tags are accepted", {
  # 22.1, 22.2 … is an established multi-stem convention in this database
  expect_length(.validate_tag_values(data.frame(tag = c(22.1, 22.2)))$errors, 0)
})

test_that("the zero check runs on a text column too", {
  # It used to sit behind is.numeric(data$tag), so a tag column read from a
  # spreadsheet as text skipped it entirely
  res <- .validate_tag_values(data.frame(tag = c("0", "5"),
                                         stringsAsFactors = FALSE))
  expect_match(paste(unlist(res$errors), collapse = " "), "cannot be 0")
})

test_that("the negative check runs on a text column too", {
  res <- .validate_tag_values(data.frame(tag = c("-5", "5"),
                                         stringsAsFactors = FALSE))
  expect_length(res$errors, 0)
  expect_match(paste(unlist(res$warnings), collapse = " "), "negative")
})

test_that("tags beyond the column's exact range are rejected", {
  # 20250001 stored in a real comes back as 20250000
  res <- .validate_tag_values(data.frame(tag = c(20250001, 5)),
                              max_exact = 2^24)
  expect_match(paste(unlist(res$errors), collapse = " "), "stored exactly")
})

test_that("the largest exactly representable tag is allowed", {
  expect_length(.validate_tag_values(data.frame(tag = 2^24),
                                     max_exact = 2^24)$errors, 0)
})

test_that("a widened column accepts the tags a real would have rounded", {
  # After migrate_tag_to_numeric() an eight-digit barcode is storable
  expect_length(.validate_tag_values(data.frame(tag = 20250001))$errors, 0)
})

test_that("the default ceiling is R's own double limit", {
  expect_length(.validate_tag_values(data.frame(tag = 2^53))$errors, 0)
  expect_length(.validate_tag_values(data.frame(tag = 2^53 * 4))$errors, 1)
})

test_that("a nonsense ceiling falls back to the double limit", {
  expect_length(.validate_tag_values(data.frame(tag = 20250001),
                                     max_exact = NA)$errors, 0)
})

# =============================================================================
# .tag_precision_limit()
# =============================================================================

test_that(".tag_precision_limit is permissive without a connection", {
  expect_equal(.tag_precision_limit(NULL), 2^53)
})

# ATTACH gives SQLite a genuine `information_schema` schema, so the query
# under test runs verbatim rather than against a table that merely has a dot
# in its name — which SQLite would read as schema.table and fail on.
make_catalogue_db <- function(data_type) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS information_schema")
  DBI::dbExecute(con, "CREATE TABLE information_schema.columns (
      table_schema TEXT, table_name TEXT, column_name TEXT, data_type TEXT)")
  DBI::dbExecute(con, sprintf("
    INSERT INTO information_schema.columns
    VALUES ('public', 'data_individuals', 'tag', '%s')", data_type))
  con
}

test_that(".tag_precision_limit tightens the ceiling for a real column", {
  skip_if_not_installed("RSQLite")

  con <- make_catalogue_db("real")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(.tag_precision_limit(con), 2^24)
})

test_that(".tag_precision_limit relaxes once the column is numeric", {
  skip_if_not_installed("RSQLite")

  con <- make_catalogue_db("numeric")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(.tag_precision_limit(con), 2^53)
})

test_that(".tag_precision_limit stays permissive when the lookup fails", {
  skip_if_not_installed("RSQLite")

  # No information_schema table at all — a role without catalogue rights
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_message(res <- .tag_precision_limit(con), "tag column type")
  expect_equal(res, 2^53)
})

test_that("blank cells count as missing, not as malformed", {
  res <- .validate_tag_values(data.frame(tag = c("101", "", "  ", NA),
                                         stringsAsFactors = FALSE))
  expect_length(res$errors, 0)
})

test_that("factor tag columns are read by their labels", {
  # as.numeric() on a factor returns level codes, which would silently pass
  res <- .validate_tag_values(data.frame(tag = factor(c("A12", "101"))))
  expect_match(paste(unlist(res$errors), collapse = " "), "A12")
})

test_that("a missing tag column is not an error", {
  expect_length(.validate_tag_values(data.frame(plot_name = "P1"))$errors, 0)
})

# =============================================================================
# .generate_sequential_tags()
# =============================================================================

test_that("generation refuses to coerce a character tag column", {
  # One blank cell used to trigger generation, as.numeric() turned every
  # alphanumeric tag into NA, and the fill then replaced them all with row
  # numbers — reported as though only the blank row had changed
  data <- data.frame(plot_name = rep("P1", 3),
                     tag = c("A12", "B7", NA),
                     stringsAsFactors = FALSE)

  expect_error(.generate_sequential_tags(data), "not numbers")
})

test_that("the refusal names the values it would have destroyed", {
  data <- data.frame(plot_name = "P1", tag = "A12", stringsAsFactors = FALSE)
  expect_error(.generate_sequential_tags(data), "A12")
})

test_that("generation still fills blanks in a numeric column", {
  data <- data.frame(plot_name = c("P1", "P1", "P2"),
                     tag = c(101, NA, NA))
  out <- .generate_sequential_tags(data)

  expect_equal(out$tag[1], 101)
  expect_false(any(is.na(out$tag)))
})

test_that("generation accepts a text column of clean numbers", {
  data <- data.frame(plot_name = rep("P1", 3),
                     tag = c("101", "", "103"),
                     stringsAsFactors = FALSE)
  out <- .generate_sequential_tags(data)

  expect_type(out$tag, "double")
  expect_equal(out$tag[c(1, 3)], c(101, 103))
  expect_false(is.na(out$tag[2]))
})

test_that("generation creates the column when it is absent", {
  out <- .generate_sequential_tags(data.frame(plot_name = c("P1", "P1")))
  expect_equal(out$tag, c(1, 2))
})

test_that("each plot gets its own sequence", {
  out <- .generate_sequential_tags(
    data.frame(plot_name = c("P1", "P1", "P2"), tag = NA_real_)
  )
  expect_equal(out$tag, c(1, 2, 1))
})
