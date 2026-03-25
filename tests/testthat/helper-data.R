# Shared test fixtures for CafriplotsR tests
# This file is automatically sourced by testthat before running tests.

# ---------------------------------------------------------------------------
# Sample data for helpers.R tests
# ---------------------------------------------------------------------------

#' A small data frame with mixed types and NAs for testing replace_NA()
make_mixed_df <- function() {

  data.frame(
    numeric_col = c(1.5, NA, 3.0, NA),
    integer_col = c(1L, 2L, NA, 4L),
    char_col    = c("a", NA, "c", "d"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Sample data for species_plot_matrix() tests
# ---------------------------------------------------------------------------

#' Minimal species-plot data
make_species_plot_data <- function() {
  data.frame(
    tax_sp_level = c("Sp_A", "Sp_A", "Sp_B", "Sp_B", "Sp_A", NA),
    plot_name    = c("P1",   "P1",   "P1",   "P2",   "P2",   "P1"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Sample data for pivot_numeric_traits_generic() tests
# ---------------------------------------------------------------------------

#' Numeric trait data in long format
make_numeric_trait_data <- function() {
  data.frame(
    idtax             = c(1, 1, 1, 2, 2, 2),
    trait             = c("height", "height", "dbh", "height", "dbh", "dbh"),
    traitvalue        = c("10.5", "12.3", "30.0", "8.0", "25.0", "27.0"),
    id_trait_measures = c(101, 102, 103, 104, 105, 106),
    stringsAsFactors  = FALSE
  )
}

# ---------------------------------------------------------------------------
# Sample data for pivot_categorical_traits_generic() tests
# ---------------------------------------------------------------------------

#' Categorical trait data in long format
make_categorical_trait_data <- function() {
  data.frame(
    idtax             = c(1, 1, 1, 2, 2),
    trait             = c("bark", "bark", "leaf", "bark", "leaf"),
    traitvalue_char   = c("smooth", "rough", "simple", "smooth", "compound"),
    id_trait_measures = c(201, 202, 203, 204, 205),
    stringsAsFactors  = FALSE
  )
}
