# Filtering plots by their features.
#
# A plot feature is a row of `data_liste_sub_plots` typed by `subplotype_list`,
# not a column of `data_liste_plots`, so a feature filter is a subquery rather
# than a WHERE clause on the plots table. These tests run against an in-memory
# SQLite database holding the same shape, which is enough to check both the SQL
# that gets built and the plots it actually selects.

# ── fixture ──────────────────────────────────────────────────────────────────

# Three plots:
#   1 Alpha  provider "IRD",       PI Dauby
#   2 Beta   provider "IRD-CNRS",  PI Sonke
#   3 Gamma  provider "Herbarium", PI Dauby
# Plus a numeric feature (census) that must be refused as a text filter.
feature_filter_db <- function() {

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = 1:3,
    plot_name      = c("Alpha", "Beta", "Gamma"),
    id_country     = c(1L, 1L, 2L),
    id_method      = c(1L, 1L, 1L),
    locality_name  = c("Loc1", "Loc2", "Loc3"),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "subplotype_list", data.frame(
    id_subplotype   = 1:3,
    type            = c("data_provider", "principal_investigator", "census"),
    valuetype       = c("character", "table_colnam", "numeric"),
    typedescription = c("Who provided the data", "Lead scientist", "Census number"),
    category        = c("provenance", "people", "census"),
    expectedunit    = NA_character_,
    minallowedvalue = NA_real_,
    maxallowedvalue = NA_real_,
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = c(10L, 11L),
    colnam          = c("Dauby", "Sonke"),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "data_liste_sub_plots", data.frame(
    id_sub_plots         = 1:6,
    id_table_liste_plots = c(1L, 2L, 3L, 1L, 2L, 3L),
    id_type_sub_plot     = c(1L, 1L, 1L, 2L, 2L, 2L),
    typevalue            = c(NA, NA, NA, 10, 11, 10),
    typevalue_char       = c("IRD", "IRD-CNRS", "Herbarium", NA, NA, NA),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "table_countries", data.frame(
    id_country = 1:2,
    country    = c("Gabon", "Cameroon"),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "methodslist", data.frame(
    id_method = 1L,
    method    = "transect",
    stringsAsFactors = FALSE
  ))

  con
}

# Plots selected by a builder, as a sorted integer vector.
selected_plots <- function(builder, con) {
  res <- DBI::dbGetQuery(con, builder$build())
  sort(as.integer(res$id_liste_plots))
}

# ── which valuetypes can be filtered ─────────────────────────────────────────

test_that(".is_filterable_valuetype() accepts text and lookup features only", {
  expect_true(CafriplotsR:::.is_filterable_valuetype("character"))
  expect_true(CafriplotsR:::.is_filterable_valuetype("table_colnam"))
  expect_false(CafriplotsR:::.is_filterable_valuetype("numeric"))
  expect_false(CafriplotsR:::.is_filterable_valuetype("integer"))
  expect_false(CafriplotsR:::.is_filterable_valuetype(NA_character_))
})

# ── validation ───────────────────────────────────────────────────────────────

test_that("a feature the database does not have is an error, not an empty result", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  expect_error(
    CafriplotsR:::.validate_feature_filters(list(data_providr = "IRD"), con),
    "Unknown plot feature"
  )
})

test_that("a numeric feature cannot be filtered as text", {
  # `census` exists, so a silently empty result would read as "no plot has that
  # census" rather than "that is a measurement, not a label".
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  expect_error(
    CafriplotsR:::.validate_feature_filters(list(census = "1"), con),
    "Only character and lookup features"
  )
})

test_that("feature_filters must be a named list without repeats", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  expect_error(
    CafriplotsR:::.validate_feature_filters("IRD", con),
    "must be a named list"
  )
  expect_error(
    CafriplotsR:::.validate_feature_filters(list("IRD"), con),
    "must be named after a plot feature"
  )
  expect_error(
    CafriplotsR:::.validate_feature_filters(
      list(data_provider = "IRD", data_provider = "CNRS"), con
    ),
    "only once"
  )
})

# ── the SQL that gets built ──────────────────────────────────────────────────

test_that("a character feature is matched on typevalue_char", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  sql <- as.character(
    PlotFilterBuilder$new(con)$filter_features(list(data_provider = "IRD"))$build()
  )

  expect_match(sql, "data_liste_sub_plots", fixed = TRUE)
  expect_match(sql, "typevalue_char", fixed = TRUE)
  expect_match(sql, "LIKE", fixed = TRUE)
})

test_that("a people feature is matched on the resolved id, not on text", {
  # table_colnam features store the id of the person in `typevalue`;
  # `typevalue_char` is never their store.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  sql <- as.character(
    PlotFilterBuilder$new(con)$
      filter_features(list(principal_investigator = "Dauby"))$build()
  )

  expect_match(sql, "sp.typevalue IN", fixed = TRUE)
  expect_match(sql, "10", fixed = TRUE)
  expect_false(grepl("typevalue_char", sql, fixed = TRUE))
})

test_that("exact_match switches substring matching off", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  loose <- as.character(
    PlotFilterBuilder$new(con)$
      filter_features(list(data_provider = "IRD"))$build()
  )
  exact <- as.character(
    PlotFilterBuilder$new(con)$
      filter_features(list(data_provider = "IRD"), exact_match = TRUE)$build()
  )

  expect_match(loose, "LIKE", fixed = TRUE)
  expect_false(grepl("LIKE", exact, fixed = TRUE))
})

# ── the plots actually selected ──────────────────────────────────────────────

test_that("a substring match selects every plot containing the value", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- PlotFilterBuilder$new(con)$filter_features(list(data_provider = "IRD"))
  expect_equal(selected_plots(b, con), c(1L, 2L))
})

test_that("exact_match selects only the plot holding that exact value", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- PlotFilterBuilder$new(con)$
    filter_features(list(data_provider = "IRD"), exact_match = TRUE)
  expect_equal(selected_plots(b, con), 1L)
})

test_that("several values of one feature are combined with OR", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- PlotFilterBuilder$new(con)$
    filter_features(list(data_provider = c("Herbarium", "IRD-CNRS")))
  expect_equal(selected_plots(b, con), c(2L, 3L))
})

test_that("different features are combined with AND", {
  # Plot 1 is the only one that is both IRD-provided and Dauby-led. The two
  # values live on different data_liste_sub_plots rows, which is exactly why
  # each feature needs its own subquery.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- PlotFilterBuilder$new(con)$filter_features(list(
    data_provider          = "IRD",
    principal_investigator = "Dauby"
  ))
  expect_equal(selected_plots(b, con), 1L)
})

test_that("a people feature resolves the name through table_colnam", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- PlotFilterBuilder$new(con)$
    filter_features(list(principal_investigator = "Dauby"))
  expect_equal(selected_plots(b, con), c(1L, 3L))
})

# ── an unmatched value returns nothing, and says so ──────────────────────────

test_that("an unmatched feature value selects no plot", {
  # The dangerous failure is the opposite one: dropping the condition would
  # return every plot the user can see and look like a successful query.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- PlotFilterBuilder$new(con)
  expect_message(
    b$filter_features(list(principal_investigator = "Nobody")),
    "no plot can match"
  )
  expect_equal(selected_plots(b, con), integer(0))
})

test_that("an unmatched country selects no plot", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- suppressMessages(
    PlotFilterBuilder$new(con)$filter_country("Atlantis")
  )
  expect_match(as.character(b$build()), "FALSE", fixed = TRUE)
  expect_equal(selected_plots(b, con), integer(0))
})

test_that("an unmatched method selects no plot", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- suppressMessages(
    PlotFilterBuilder$new(con)$filter_method("not-a-method")
  )
  expect_match(as.character(b$build()), "FALSE", fixed = TRUE)
  expect_equal(selected_plots(b, con), integer(0))
})

test_that("a matched filter still selects its plots", {
  # Guards against the impossible condition leaking into the happy path.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  b <- suppressMessages(PlotFilterBuilder$new(con)$filter_country("Gabon"))
  expect_false(grepl("FALSE", as.character(b$build()), fixed = TRUE))
  expect_equal(selected_plots(b, con), c(1L, 2L))
})

# ── discovery helpers ────────────────────────────────────────────────────────

test_that("plot_feature_filters() lists text features and omits measurements", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  res <- plot_feature_filters(con = con)

  expect_true(all(c("data_provider", "principal_investigator") %in% res$feature))
  expect_false("census" %in% res$feature)
  expect_true(all(c("feature", "valuetype", "category", "description") %in% names(res)))
})

test_that("plot_feature_values() refuses a feature that holds measurements", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  expect_error(plot_feature_values("census", con = con),
               "Only character and lookup features")
  expect_error(plot_feature_values(c("a", "b"), con = con),
               "single feature name")
})

# ── plot ids matching a filter ───────────────────────────────────────────────

test_that(".plot_ids_matching_features() returns the ids the filter selects", {
  # This is the path query_plots() takes when it was given explicit id_plot and
  # so never built a filter query.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  ids <- CafriplotsR:::.plot_ids_matching_features(
    list(data_provider = "IRD"), con = con
  )
  expect_equal(sort(ids), c(1L, 2L))
})
