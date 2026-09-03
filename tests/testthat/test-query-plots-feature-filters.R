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

# The conditions a set of feature filters produces.
feature_conditions <- function(..., con, exact_match = FALSE) {
  CafriplotsR:::.feature_conditions(list(...), con, exact_match = exact_match)
}

# The query a set of conditions builds, as a string.
built_sql <- function(conditions, con) {
  as.character(CafriplotsR:::.assemble_plot_query(conditions, con))
}

# Plots selected by a set of conditions, as a sorted integer vector.
selected_plots <- function(conditions, con) {
  res <- DBI::dbGetQuery(con, CafriplotsR:::.assemble_plot_query(conditions, con))
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

  sql <- built_sql(feature_conditions(data_provider = "IRD", con = con), con)

  expect_match(sql, "data_liste_sub_plots", fixed = TRUE)
  expect_match(sql, "typevalue_char", fixed = TRUE)
  expect_match(sql, "LIKE", fixed = TRUE)
})

test_that("a people feature is matched on the resolved id, not on text", {
  # table_colnam features store the id of the person in `typevalue`;
  # `typevalue_char` is never their store.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  sql <- built_sql(feature_conditions(principal_investigator = "Dauby", con = con), con)

  expect_match(sql, "sp.typevalue IN", fixed = TRUE)
  expect_match(sql, "10", fixed = TRUE)
  expect_false(grepl("typevalue_char", sql, fixed = TRUE))
})

test_that("exact_match switches substring matching off", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  loose <- built_sql(feature_conditions(data_provider = "IRD", con = con), con)
  exact <- built_sql(
    feature_conditions(data_provider = "IRD", con = con, exact_match = TRUE), con
  )

  expect_match(loose, "LIKE", fixed = TRUE)
  expect_false(grepl("LIKE", exact, fixed = TRUE))
})

# ── the plots actually selected ──────────────────────────────────────────────

test_that("a substring match selects every plot containing the value", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- feature_conditions(data_provider = "IRD", con = con)
  expect_equal(selected_plots(cond, con), c(1L, 2L))
})

test_that("exact_match selects only the plot holding that exact value", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- feature_conditions(data_provider = "IRD", con = con, exact_match = TRUE)
  expect_equal(selected_plots(cond, con), 1L)
})

test_that("several values of one feature are combined with OR", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- feature_conditions(data_provider = c("Herbarium", "IRD-CNRS"), con = con)
  expect_equal(selected_plots(cond, con), c(2L, 3L))
})

test_that("different features are combined with AND", {
  # Plot 1 is the only one that is both IRD-provided and Dauby-led. The two
  # values live on different data_liste_sub_plots rows, which is exactly why
  # each feature needs its own subquery.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- feature_conditions(
    data_provider          = "IRD",
    principal_investigator = "Dauby",
    con                    = con
  )
  expect_length(cond, 2)
  expect_equal(selected_plots(cond, con), 1L)
})

test_that("a people feature resolves the name through table_colnam", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- feature_conditions(principal_investigator = "Dauby", con = con)
  expect_equal(selected_plots(cond, con), c(1L, 3L))
})

# ── an unmatched value returns nothing, and says so ──────────────────────────

test_that("an unmatched feature value selects no plot", {
  # The dangerous failure is the opposite one: dropping the condition would
  # return every plot the user can see and look like a successful query.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  expect_message(
    cond <- feature_conditions(principal_investigator = "Nobody", con = con),
    "no plot can match"
  )
  expect_equal(selected_plots(cond, con), integer(0))
})

test_that("an unmatched country selects no plot", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- suppressMessages(
    CafriplotsR:::.plot_condition_country("Atlantis", con)
  )
  expect_match(built_sql(cond, con), "FALSE", fixed = TRUE)
  expect_equal(selected_plots(cond, con), integer(0))
})

test_that("an unmatched method selects no plot", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- suppressMessages(
    CafriplotsR:::.plot_condition_method("not-a-method", con)
  )
  expect_match(built_sql(cond, con), "FALSE", fixed = TRUE)
  expect_equal(selected_plots(cond, con), integer(0))
})

test_that("a matched filter still selects its plots", {
  # Guards against the impossible condition leaking into the happy path.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- suppressMessages(CafriplotsR:::.plot_condition_country("Gabon", con))
  expect_false(grepl("FALSE", built_sql(cond, con), fixed = TRUE))
  expect_equal(selected_plots(cond, con), c(1L, 2L))
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

test_that("plot_feature_values() returns the stored values of a character feature", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  res <- plot_feature_values("data_provider", con = con)

  expect_equal(sort(res$value), c("Herbarium", "IRD", "IRD-CNRS"))
  expect_true(all(res$n_plots == 1))
})

test_that("plot_feature_values() resolves a people feature to names", {
  # The stored value is an id_table_colnam in `typevalue`; what the user needs
  # to filter on is the name.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  res <- plot_feature_values("principal_investigator", con = con)

  expect_equal(sort(res$value), c("Dauby", "Sonke"))
  # Dauby leads two of the three plots, so the list is led by them.
  expect_equal(res$value[1], "Dauby")
  expect_equal(res$n_plots[res$value == "Dauby"], 2)
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

# ── assembling the whole query ───────────────────────────────────────────────
#
# query_plots() no longer holds a builder object: each filter argument is
# turned into conditions by its own function and the set is assembled once.
# What follows checks that assembly, argument by argument.

test_that("an argument that was not given adds no condition", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  expect_length(CafriplotsR:::.plot_condition_country(NULL, con), 0)
  expect_length(CafriplotsR:::.plot_condition_plot_name(NULL, con), 0)
  expect_length(CafriplotsR:::.plot_condition_method(NULL, con), 0)
  expect_length(CafriplotsR:::.plot_condition_locality(NULL, con), 0)
  expect_length(CafriplotsR:::.feature_conditions(NULL, con), 0)
})

test_that("no condition at all selects every plot", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  sql <- built_sql(character(0), con)
  expect_false(grepl("WHERE", sql, fixed = TRUE))
  expect_equal(selected_plots(character(0), con), c(1L, 2L, 3L))
})

test_that("a single plot name is a substring match, several are exact", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  loose <- CafriplotsR:::.plot_condition_plot_name("lph", con)
  expect_equal(selected_plots(loose, con), 1L)

  several <- CafriplotsR:::.plot_condition_plot_name(c("Alpha", "Gamma"), con)
  expect_equal(selected_plots(several, con), c(1L, 3L))

  exact <- CafriplotsR:::.plot_condition_plot_name("lph", con, exact_match = TRUE)
  expect_equal(selected_plots(exact, con), integer(0))
})

test_that("localities are matched as substrings and OR'ed together", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  one <- CafriplotsR:::.plot_condition_locality("Loc2", con)
  expect_equal(selected_plots(one, con), 2L)

  several <- CafriplotsR:::.plot_condition_locality(c("Loc1", "Loc3"), con)
  expect_equal(selected_plots(several, con), c(1L, 3L))
})

test_that(".plot_filter_query() combines the filters with AND", {
  # Gabon holds plots 1 and 2; the Herbarium provided plot 3 only. Asking for
  # both must return nothing, not the union.
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  query <- suppressMessages(CafriplotsR:::.plot_filter_query(
    con = con, country = "Gabon",
    feature_filters = list(data_provider = "Herbarium")
  ))
  expect_equal(nrow(DBI::dbGetQuery(con, query)), 0)

  query <- suppressMessages(CafriplotsR:::.plot_filter_query(
    con = con, country = "Gabon",
    feature_filters = list(data_provider = "IRD")
  ))
  res <- DBI::dbGetQuery(con, query)
  expect_equal(sort(as.integer(res$id_liste_plots)), c(1L, 2L))
})

test_that(".plot_filter_query() can join its conditions with OR", {
  con <- feature_filter_db()
  on.exit(DBI::dbDisconnect(con))

  query <- suppressMessages(CafriplotsR:::.plot_filter_query(
    con = con, country = "Gabon",
    feature_filters = list(data_provider = "Herbarium"),
    operator = "OR"
  ))
  res <- DBI::dbGetQuery(con, query)
  expect_equal(sort(as.integer(res$id_liste_plots)), c(1L, 2L, 3L))
})
