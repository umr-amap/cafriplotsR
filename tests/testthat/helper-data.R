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

# ---------------------------------------------------------------------------
# Sample data for output_styles_helpers.R tests
# ---------------------------------------------------------------------------

#' Flat plot+individual data as returned by query_plots() before restructuring.
#' Two plots (P1, P2), two individuals each.
make_plot_individual_data <- function() {
  data.frame(
    id_liste_plots          = c(1L, 1L, 2L, 2L),
    plot_name               = c("P1", "P1", "P2", "P2"),
    country                 = c("Gabon", "Gabon", "Cameroon", "Cameroon"),
    ddlat                   = c(-0.5,  -0.5,   3.2,  3.2),
    ddlon                   = c(12.1,  12.1,  11.8, 11.8),
    elevation               = c(300L,  300L,  450L, 450L),
    id_n                    = c(101L,  102L,  201L, 202L),
    tag                     = c("T1",  "T2",  "T3", "T4"),
    tax_fam                 = c("Fabaceae", "Meliaceae", "Fabaceae", "Annonaceae"),
    tax_gen                 = c("Gilbertiodendron", "Entandrophragma",
                                "Brachystegia", "Greenwayodendron"),
    tax_sp_level            = c("dewevrei", "utile", "laurentii", "suaveolens"),
    stem_diameter           = c(350, 420, 280, NA),
    tree_height             = c(28.5, 35.0, 22.0, NA),
    height_of_stem_diameter = c(1.3, 1.3, 1.3, NA),
    date_modif_y            = c(2024L, 2024L, 2024L, 2024L),
    stringsAsFactors        = FALSE
  )
}

#' Census features table as would come from subplot census data.
make_census_features <- function() {
  data.frame(
    plot_name  = c("P1", "P1", "P2"),
    typevalue  = c(1,    2,    1),
    year       = c(2010, 2015, 2012),
    month      = c(3,    7,    11),
    stringsAsFactors = FALSE
  )
}

#' Multi-census individual data: two censuses per individual.
#' Includes both base columns (stem_diameter, tree_height) and census-suffixed
#' columns (stem_diameter_census_1, etc.) to match real query_plots() output.
make_multi_census_data <- function() {
  data.frame(
    id_liste_plots            = c(1L, 1L),
    plot_name                 = c("P1", "P1"),
    id_n                      = c(101L, 102L),
    tag                       = c("T1", "T2"),
    tax_fam                   = c("Fabaceae", "Meliaceae"),
    tax_gen                   = c("Gilbertiodendron", "Entandrophragma"),
    tax_sp_level              = c("dewevrei", "utile"),
    stem_diameter             = c(350, 420),          # base column (most recent)
    tree_height               = c(28.5, 35.0),        # base column (most recent)
    stem_diameter_census_1    = c(340, 410),
    stem_diameter_census_2    = c(350, 420),
    tree_height_census_1      = c(27.0, 34.0),
    tree_height_census_2      = c(28.5, 35.0),
    height_of_stem_diameter_census_1 = c(1.3, 1.3),
    height_of_stem_diameter_census_2 = c(1.3, 1.3),
    stringsAsFactors          = FALSE
  )
}
