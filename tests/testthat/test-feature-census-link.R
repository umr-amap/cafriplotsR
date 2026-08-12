# Tests for R/feature_census_link.R
#
# Whether a feature carries id_sub_plots decides how query_plots() reports it
# when show_multiple_census is TRUE, so the policy is worth pinning down.

test_that("position features are never attached to a census", {
  policy <- .feature_census_link(
    c("quadrat", "position_x", "position_y", "position_transect",
      "transect_part", "transect_section"))

  expect_true(all(policy == "never"))
})

test_that("what the plant is, and what a lab measured, are not census facts", {
  policy <- .feature_census_link(
    c("strate_cat", "abundance_coeff", "reproductive_state",
      "growth_form_level_1", "life_history_level_2",
      "plant_height", "spinescence", "twig_fresh_volume",
      "leaf_C_percentage", "leaf_delta_carbon_13", "colnam"))

  expect_true(all(policy == "never"))
})

test_that("measurements are attached by default", {
  policy <- .feature_census_link(
    c("stem_diameter", "tree_height", "stem_status", "flag1_rainfor",
      "height_of_stem_diameter", "crown_width", "observations"))

  expect_true(all(policy == "always"))
})

test_that("an unknown feature is attached rather than silently dropped", {
  # A census import exists to record a campaign; not linking has to be the
  # deliberate choice, never the accident
  expect_equal(unname(.feature_census_link("a_brand_new_trait")), "always")
})

test_that(".feature_census_link keeps the order and names it was given", {
  features <- c("tree_height", "quadrat", "stem_diameter", "position_x")
  policy <- .feature_census_link(features)

  expect_equal(names(policy), features)
  expect_equal(unname(policy), c("always", "never", "always", "never"))
})

test_that(".feature_census_link handles an empty request", {
  expect_length(.feature_census_link(character(0)), 0)
})

test_that("the database policy overrides the built-in default", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS information_schema")
  DBI::dbExecute(con, "CREATE TABLE information_schema.columns (
                         table_schema TEXT, table_name TEXT, column_name TEXT)")
  DBI::dbExecute(con, "INSERT INTO information_schema.columns
                       VALUES ('public', 'traitlist', 'census_link')")
  DBI::dbWriteTable(con, "traitlist", data.frame(
    stringsAsFactors = FALSE,
    trait       = c("quadrat", "tree_height", "leaf_area"),
    census_link = c("always", "never", NA_character_)
  ))

  policy <- .feature_census_link(c("quadrat", "tree_height", "leaf_area"), con)

  # the column wins in both directions
  expect_equal(unname(policy["quadrat"]), "always")
  expect_equal(unname(policy["tree_height"]), "never")
  # and NULL there leaves the default in place
  expect_equal(unname(policy["leaf_area"]), "always")
})

test_that("a database without the column falls back to the default", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS information_schema")
  DBI::dbExecute(con, "CREATE TABLE information_schema.columns (
                         table_schema TEXT, table_name TEXT, column_name TEXT)")
  DBI::dbExecute(con, "INSERT INTO information_schema.columns
                       VALUES ('public', 'traitlist', 'trait')")

  policy <- .feature_census_link(c("quadrat", "stem_diameter"), con)

  expect_equal(unname(policy), c("never", "always"))
})

test_that("an unreadable database does not break the policy", {
  broken <- structure(list(), class = "not_a_connection")

  expect_message(
    policy <- .feature_census_link(c("quadrat", "stem_diameter"), broken),
    "census link policy"
  )
  expect_equal(unname(policy), c("never", "always"))
})

test_that("an invalid declared value is ignored rather than written through", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "ATTACH DATABASE ':memory:' AS information_schema")
  DBI::dbExecute(con, "CREATE TABLE information_schema.columns (
                         table_schema TEXT, table_name TEXT, column_name TEXT)")
  DBI::dbExecute(con, "INSERT INTO information_schema.columns
                       VALUES ('public', 'traitlist', 'census_link')")
  DBI::dbWriteTable(con, "traitlist", data.frame(
    stringsAsFactors = FALSE,
    trait = "quadrat", census_link = "sometimes"
  ))

  expect_equal(unname(.feature_census_link("quadrat", con)), "never")
})
