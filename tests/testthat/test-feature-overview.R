# The feature overview is shared by the update app and the feature wizard, so
# its wording and its columns are worth pinning down once.

.fake_i18n <- function() {
  list(t = function(x) x)
}

test_that(".feature_rule_label() words the single-record case apart from the rule", {
  i18n <- .fake_i18n()

  # The rule is what the extraction does; one record still goes through it.
  expect_equal(CafriplotsR:::.feature_rule_label("mean", 1L, i18n),
               "one record, shown as it is")
  expect_equal(CafriplotsR:::.feature_rule_label("mean", 3L, i18n),
               "mean of 3 records")
  expect_equal(CafriplotsR:::.feature_rule_label("concat", 2L, i18n),
               "2 records joined into one text")
  expect_equal(CafriplotsR:::.feature_rule_label("per_census", 4L, i18n),
               "one value per census, from 4 records")
})

test_that(".feature_rule_label() keeps census and not-extracted whatever the count", {
  i18n <- .fake_i18n()

  # A single census record is still the plot's census list, not a value.
  expect_equal(CafriplotsR:::.feature_rule_label("census", 1L, i18n),
               "not a value: n_census, first_census, last_census, date_census_N")
  expect_equal(CafriplotsR:::.feature_rule_label("not_extracted", 1L, i18n),
               "not carried into extracted tables")
})

test_that(".feature_rule_labels() maps over records", {
  i18n <- .fake_i18n()
  out <- CafriplotsR:::.feature_rule_labels(c("mean", "census"), c(2L, 1L), i18n)
  expect_length(out, 2L)
  expect_equal(out[1], "mean of 2 records")
})

test_that(".feature_overview_dt() shows the plot column only when plots are named", {
  i18n <- .fake_i18n()
  base <- dplyr::tibble(
    feature = "soil_depth", valuetype = "numeric", unit = "cm",
    n_records = 2L, agg_rule = "mean", aggregate_display = "35",
    is_aggregated = TRUE
  )

  one <- CafriplotsR:::.feature_overview_dt(base, i18n)
  expect_false(grepl("Plot", paste(as.character(one$x$container), collapse = "")))
  expect_equal(one$x$data$feature, "soil_depth")
  expect_false("plot_name" %in% names(one$x$data))

  many <- CafriplotsR:::.feature_overview_dt(
    dplyr::bind_cols(dplyr::tibble(plot_name = "BEL-01"), base), i18n
  )
  expect_equal(names(many$x$data)[1], "plot_name")
  expect_true(grepl("Plot", paste(as.character(many$x$container), collapse = "")))
})

test_that(".feature_overview_dt() fills a missing extracted value rather than showing NA", {
  i18n <- .fake_i18n()
  s <- dplyr::tibble(
    feature = "some_ordinal", valuetype = "ordinal", unit = NA_character_,
    n_records = 1L, agg_rule = "not_extracted",
    aggregate_display = NA_character_, is_aggregated = FALSE
  )
  dt <- CafriplotsR:::.feature_overview_dt(s, i18n)

  expect_equal(dt$x$data$aggregate_display, "-")
  expect_equal(dt$x$data$stored_as, "not carried into extracted tables")
})
