# Tests for filter_to_census() and the report of traits it removes
#
# query_plots(show_multiple_census = FALSE) keeps one census per plot, chosen
# across all traits at once. A trait measured only at earlier censuses - tree
# height, typically, measured once and never re-measured - is dropped whole:
# the wide pivot builds columns from trait/census combinations that carry data,
# so the trait returns no column rather than a column of NAs. The filtering is
# deliberate; the silence was not, so the warning is pinned down here.

make_census_measurements <- function() {
  # One plot, three censuses. Diameter at all three, height at census_1/2 only.
  tibble::tribble(
    ~id_data_individuals, ~id_sub_plots, ~id_table_liste_plots, ~trait,          ~traitvalue, ~census_name, ~census_typevalue, ~census_day, ~census_month, ~census_year,
    1L,                   10L,           100L,                  "stem_diameter", 10,          "census_1",   1,                 1L,          6L,            2010L,
    1L,                   11L,           100L,                  "stem_diameter", 12,          "census_2",   2,                 1L,          6L,            2015L,
    1L,                   12L,           100L,                  "stem_diameter", 14,          "census_3",   3,                 1L,          6L,            2020L,
    1L,                   10L,           100L,                  "tree_height",   20,          "census_1",   1,                 1L,          6L,            2010L,
    1L,                   11L,           100L,                  "tree_height",   22,          "census_2",   2,                 1L,          6L,            2015L
  )
}

test_that("filter_to_census keeps only the last census, dropping traits absent from it", {
  result <- suppressMessages(
    filter_to_census(make_census_measurements(), strategy = "last")
  )

  expect_equal(unique(result$census_name), "census_3")
  expect_false("tree_height" %in% result$trait)
  expect_equal(nrow(result), 1L)
})

test_that("filter_to_census keeps only the first census", {
  result <- suppressMessages(
    filter_to_census(make_census_measurements(), strategy = "first")
  )

  expect_equal(unique(result$census_name), "census_1")
  expect_setequal(result$trait, c("stem_diameter", "tree_height"))
})

test_that("filter_to_census names the traits the census selection dropped", {
  expect_message(
    filter_to_census(make_census_measurements(), strategy = "last"),
    "tree_height"
  )
})

test_that("filter_to_census stays quiet when every trait survives the selection", {
  # Height measured at the last census too: nothing is lost, nothing is said
  data <- make_census_measurements()
  data <- dplyr::bind_rows(
    data,
    tibble::tibble(
      id_data_individuals = 1L, id_sub_plots = 12L, id_table_liste_plots = 100L,
      trait = "tree_height", traitvalue = 25, census_name = "census_3",
      census_typevalue = 3, census_day = 1L, census_month = 6L, census_year = 2020L
    )
  )

  messages <- testthat::capture_messages(filter_to_census(data, strategy = "last"))
  expect_false(any(grepl("dropped", messages)))
})

test_that("filter_to_census leaves non-census measurements untouched", {
  # A trait linked to no census - or to a subplot that is not a census - must
  # survive whichever census is selected
  data <- dplyr::bind_rows(
    make_census_measurements(),
    tibble::tibble(
      id_data_individuals = 1L, id_sub_plots = NA_integer_,
      id_table_liste_plots = 100L, trait = "wood_density", traitvalue = 0.6,
      census_name = NA_character_, census_typevalue = NA_real_,
      census_day = NA_integer_, census_month = NA_integer_, census_year = NA_integer_
    )
  )

  result <- suppressMessages(filter_to_census(data, strategy = "last"))

  expect_true("wood_density" %in% result$trait)
  expect_setequal(result$trait, c("stem_diameter", "wood_density"))
})

test_that(".report_traits_dropped_by_census returns the dropped traits invisibly", {
  data <- make_census_measurements()
  dated <- dplyr::mutate(data, sort_key = as.Date(paste0(census_year, "-01-01")))

  dropped <- suppressMessages(
    .report_traits_dropped_by_census(
      dated       = dated,
      kept        = dplyr::filter(dated, census_name == "census_3"),
      passthrough = dated[0, ],
      strategy    = "last"
    )
  )

  expect_equal(dropped, "tree_height")
})

test_that(".report_traits_dropped_by_census says nothing when nothing is dropped", {
  dated <- dplyr::mutate(
    make_census_measurements(),
    sort_key = as.Date(paste0(census_year, "-01-01"))
  )

  expect_silent(
    result <- .report_traits_dropped_by_census(
      dated       = dated,
      kept        = dated,
      passthrough = dated[0, ],
      strategy    = "last"
    )
  )
  expect_length(result, 0)
})
