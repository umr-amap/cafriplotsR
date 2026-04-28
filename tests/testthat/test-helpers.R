# Tests for R/helpers.R pure utility functions

# =============================================================================
# .get_debug_header()
# =============================================================================

test_that(".get_debug_header includes package, R version, and timestamp context", {
  header <- CafriplotsR:::.get_debug_header()

  expect_match(header, "^\\[CafriplotsR v")
  expect_match(header, paste0("R ", R.version$major, "\\.", sub("\\..*$", "", R.version$minor)))
  expect_match(header, "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]$")
})

# =============================================================================
# choose_prompt()
# =============================================================================

test_that("choose_prompt defaults to the first choice on empty input", {
  testthat::local_mocked_bindings(
    .package = "base",
    readline = function(prompt = "") ""
  )

  expect_true(choose_prompt(message = "Select an option"))
})

test_that("choose_prompt maps the third choice to NA", {
  testthat::local_mocked_bindings(
    .package = "base",
    readline = function(prompt = "") "3"
  )

  expect_true(is.na(choose_prompt()))
})

test_that("choose_prompt returns NULL for invalid input", {
  testthat::local_mocked_bindings(
    .package = "base",
    readline = function(prompt = "") "9"
  )

  expect_null(choose_prompt())
})

# =============================================================================
# replace_NA()
# =============================================================================

test_that("replace_NA replaces NAs with sentinel values", {
  df <- make_mixed_df()
  result <- replace_NA(df)

  # Numeric NAs become -9999

  expect_equal(result$numeric_col, c(1.5, -9999, 3.0, -9999))
  expect_equal(result$integer_col, c(1L, 2L, -9999L, 4L))


  # Character NAs become "-9999"
  expect_equal(result$char_col, c("a", "-9999", "c", "d"))

  # No NAs should remain
  expect_false(anyNA(result))
})

test_that("replace_NA with inv = TRUE restores NAs from sentinels", {
  df <- make_mixed_df()

  # Round-trip: replace then restore
  replaced <- replace_NA(df)
  restored <- replace_NA(replaced, inv = TRUE)

  expect_equal(restored$numeric_col, df$numeric_col)
  expect_equal(restored$char_col, df$char_col)
})

test_that("replace_NA handles data frame with no NAs", {
  df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"),
                   stringsAsFactors = FALSE)
  result <- replace_NA(df)
  expect_equal(result, df)
})

test_that("replace_NA handles data frame with all NAs", {
  df <- data.frame(x = c(NA_real_, NA_real_),
                   y = c(NA_character_, NA_character_),
                   stringsAsFactors = FALSE)
  result <- replace_NA(df)

  expect_equal(result$x, c(-9999, -9999))
  expect_equal(result$y, c("-9999", "-9999"))
})

# =============================================================================
# .rename_data()
# =============================================================================

test_that(".rename_data renames a single column", {
  df <- data.frame(old_name = 1:3)
  result <- .rename_data(df, "old_name", "new_name")
  expect_true("new_name" %in% names(result))
  expect_false("old_name" %in% names(result))
})

test_that(".rename_data renames multiple columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  result <- .rename_data(df, c("a", "c"), c("alpha", "gamma"))
  expect_equal(names(result), c("alpha", "b", "gamma"))
})

test_that(".rename_data errors when column not found", {
  df <- data.frame(a = 1:3)
  expect_error(
    .rename_data(df, "nonexistent", "new"),
    "Column name provided not found"
  )
})

test_that(".rename_data errors when old and new lengths differ", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(
    .rename_data(df, c("a", "b"), "only_one"),
    "number of new columns names different"
  )
})

# =============================================================================
# .add_modif_field()
# =============================================================================

test_that(".add_modif_field adds date columns", {
  df <- data.frame(x = 1:3)
  result <- .add_modif_field(df)

  expect_true("date_modif_d" %in% names(result))
  expect_true("date_modif_m" %in% names(result))
  expect_true("date_modif_y" %in% names(result))

  # Values should match today's date
  expect_equal(unique(result$date_modif_d), lubridate::day(Sys.Date()))
  expect_equal(unique(result$date_modif_m), lubridate::month(Sys.Date()))
  expect_equal(unique(result$date_modif_y), lubridate::year(Sys.Date()))
})

test_that(".add_modif_field preserves existing columns", {
  df <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  result <- .add_modif_field(df)
  expect_true(all(c("a", "b") %in% names(result)))
  expect_equal(result$a, df$a)
  expect_equal(result$b, df$b)
})

# =============================================================================
# species_plot_matrix()
# =============================================================================

test_that("species_plot_matrix produces correct matrix dimensions", {
  df <- make_species_plot_data()
  result <- species_plot_matrix(df)

  # Should have 2 species (Sp_A, Sp_B) and 2 plots (P1, P2)
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_true(all(c("P1", "P2") %in% colnames(result)))
  expect_true(all(c("Sp_A", "Sp_B") %in% rownames(result)))
})

test_that("species_plot_matrix counts individuals correctly", {
  df <- make_species_plot_data()
  result <- species_plot_matrix(df)

  # Sp_A: 2 in P1, 1 in P2
  expect_equal(result["Sp_A", "P1"], 2)
  expect_equal(result["Sp_A", "P2"], 1)

  # Sp_B: 1 in P1, 1 in P2
  expect_equal(result["Sp_B", "P1"], 1)
  expect_equal(result["Sp_B", "P2"], 1)
})

test_that("species_plot_matrix removes NA taxa", {
  df <- make_species_plot_data()
  result <- species_plot_matrix(df)

  # The NA row in the input should be excluded
  expect_false(any(is.na(rownames(result))))
})

test_that("species_plot_matrix fills zeros for absent species", {
  df <- data.frame(
    tax_sp_level = c("Sp_A", "Sp_B"),
    plot_name    = c("P1", "P2"),
    stringsAsFactors = FALSE
  )
  result <- species_plot_matrix(df)

  # Sp_A absent from P2, Sp_B absent from P1
  expect_equal(result["Sp_A", "P2"], 0)
  expect_equal(result["Sp_B", "P1"], 0)
})

test_that("species_plot_matrix respects custom column names", {
  df <- data.frame(
    species = c("A", "B", "A"),
    site    = c("S1", "S1", "S2"),
    stringsAsFactors = FALSE
  )
  result <- species_plot_matrix(df, tax_col = "species", plot_col = "site")
  expect_equal(nrow(result), 2)
  expect_true(all(c("S1", "S2") %in% colnames(result)))
})

# =============================================================================
# join_help_function()
# =============================================================================

test_that("join_help_function keeps requested lookup columns using remapped keys", {
  df1 <- tibble::tibble(id_country = c(10L, 20L, 30L))
  df2 <- tibble::tibble(
    id = c(10L, 20L),
    iso3 = c("CMR", "GAB"),
    region = c("Central Africa", "Central Africa")
  )

  result <- join_help_function(df1, df2, "id_country", "id", keep_columns = c("iso3", "region"))

  expect_equal(result$iso3, c("CMR", "GAB", NA))
  expect_equal(result$region, c("Central Africa", "Central Africa", NA))
})
