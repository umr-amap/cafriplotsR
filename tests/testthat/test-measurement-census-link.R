# Tests for the census link in the feature wizard's measurement step
#
# The "Link to Census" box pre-fills a census, and a measurement carrying
# id_sub_plots is reported per census by query_plots(show_multiple_census =
# TRUE). Neither should happen for a feature that describes the tree rather
# than the campaign, so both the offer and the write are pinned down here.

fake_i18n <- list(t = function(x) x)

test_that(".never_linked_features names only the features the policy refuses", {
  policy <- .feature_census_link(c("stem_diameter", "quadrat", "position_x"))

  expect_equal(
    sort(.never_linked_features(c("stem_diameter", "quadrat", "position_x"), policy)),
    c("position_x", "quadrat")
  )
})

test_that(".never_linked_features names nothing when the policy is not loaded", {
  # An empty policy means "not read yet", which must leave the caller's
  # default pre-selection alone rather than suppress it
  expect_length(.never_linked_features(c("quadrat", "position_x"), character(0)), 0)
  expect_length(.never_linked_features(character(0), c(quadrat = "never")), 0)
})

test_that(".latest_census_per_plot offers the most recent census of each plot", {
  censuses <- data.frame(
    stringsAsFactors = FALSE,
    id_sub_plots         = c(11L, 12L, 21L),
    id_table_liste_plots = c(1L, 1L, 2L),
    census_num           = c(1, 2, 1)
  )

  expect_equal(.latest_census_per_plot(censuses), c("12", "21"))
})

test_that(".latest_census_per_plot offers nothing when no census is numbered", {
  censuses <- data.frame(
    id_sub_plots = 11L, id_table_liste_plots = 1L, census_num = NA_real_
  )

  expect_length(.latest_census_per_plot(censuses), 0)
  expect_length(.latest_census_per_plot(censuses[0, ]), 0)
  expect_length(.latest_census_per_plot(NULL), 0)
})

test_that(".unlink_never_features clears the census only where the policy says never", {
  data <- data.frame(
    stringsAsFactors = FALSE,
    trait_name   = c("stem_diameter", "quadrat", "stem_diameter"),
    id_sub_plots = c(10L, 10L, 11L)
  )
  policy <- c(stem_diameter = "always", quadrat = "never")

  out <- suppressMessages(.unlink_never_features(data, policy))

  expect_equal(out$id_sub_plots, c(10L, NA_integer_, 11L))
})

test_that(".unlink_never_features leaves untouched what it has no policy for", {
  data <- data.frame(
    trait_name = c("stem_diameter", "quadrat"), id_sub_plots = c(10L, 10L)
  )

  expect_equal(.unlink_never_features(data, character(0))$id_sub_plots, c(10L, 10L))
  expect_equal(
    .unlink_never_features(data, c(stem_diameter = "always"))$id_sub_plots,
    c(10L, 10L)
  )
})

test_that("a wide mapping attaches the census to the diameter but not the quadrat", {
  df <- data.frame(
    stringsAsFactors = FALSE,
    plot_name      = c("PLOT1", "PLOT1"),
    tag            = c(1, 2),
    id_liste_plots = c(1L, 1L),
    dbh            = c(12.5, 30.1),
    quadrat        = c("A1", "B2")
  )
  traits <- data.frame(
    stringsAsFactors = FALSE,
    id_trait  = c(3L, 4L),
    trait     = c("stem_diameter", "quadrat"),
    valuetype = c("numeric", "character")
  )
  census_map <- data.frame(id_sub_plots = 10L, id_table_liste_plots = 1L)
  input <- list(
    trait_map_dbh = "stem_diameter", trait_map_quadrat = "quadrat",
    col_role_dbh = "trait", col_role_quadrat = "trait"
  )

  out <- suppressMessages(.apply_wide_mapping(
    df, traits, census_map, input, ns = identity, i18n = fake_i18n,
    link_policy = c(stem_diameter = "always", quadrat = "never")
  ))

  expect_equal(
    out$data$id_sub_plots[out$data$trait_name == "stem_diameter"],
    c(10L, 10L)
  )
  expect_true(all(is.na(out$data$id_sub_plots[out$data$trait_name == "quadrat"])))
})

test_that("a long mapping attaches the census to the diameter but not the quadrat", {
  df <- data.frame(
    stringsAsFactors = FALSE,
    plot_name      = c("PLOT1", "PLOT1"),
    tag            = c(1, 1),
    id_liste_plots = c(1L, 1L),
    trait_type     = c("dbh", "quadrat"),
    value_num      = c(12.5, NA),
    value_char     = c(NA, "A1")
  )
  traits <- data.frame(
    stringsAsFactors = FALSE,
    id_trait  = c(3L, 4L),
    trait     = c("stem_diameter", "quadrat"),
    valuetype = c("numeric", "character")
  )
  census_map <- data.frame(id_sub_plots = 10L, id_table_liste_plots = 1L)
  input <- list(
    map_trait_type = "trait_type",
    map_value_num  = "value_num",
    map_value_char = "value_char",
    trait_map_dbh = "stem_diameter", trait_map_quadrat = "quadrat"
  )

  out <- suppressMessages(.apply_long_mapping(
    df, traits, census_map, input, ns = identity, i18n = fake_i18n,
    link_policy = c(stem_diameter = "always", quadrat = "never")
  ))

  expect_equal(out$data$id_sub_plots[out$data$trait_name == "stem_diameter"], 10L)
  expect_true(is.na(out$data$id_sub_plots[out$data$trait_name == "quadrat"]))
})

test_that("without a policy a wide mapping links everything, as before", {
  df <- data.frame(
    stringsAsFactors = FALSE,
    plot_name = "PLOT1", tag = 1, id_liste_plots = 1L, dbh = 12.5
  )
  traits <- data.frame(
    stringsAsFactors = FALSE,
    id_trait = 3L, trait = "stem_diameter", valuetype = "numeric"
  )

  out <- .apply_wide_mapping(
    df, traits, data.frame(id_sub_plots = 10L, id_table_liste_plots = 1L),
    list(trait_map_dbh = "stem_diameter"), ns = identity, i18n = fake_i18n
  )

  expect_equal(out$data$id_sub_plots, 10L)
})

test_that("the census box follows the mapping, not the calendar", {
  skip_if_not_installed("shiny")

  censuses <- data.frame(
    stringsAsFactors = FALSE,
    id_sub_plots         = c(11L, 12L),
    id_table_liste_plots = c(1L, 1L),
    census_num           = c(1, 2),
    year                 = c(2020, 2024),
    plot_name            = c("PLOT1", "PLOT1")
  )

  shiny::testServer(
    mod_feat_step3_measurements_server,
    args = list(
      selected_plots = shiny::reactive(NULL),
      operation_mode = shiny::reactive("add_measurements"),
      con            = shiny::reactive(NULL),
      i18n           = shiny::reactive(list(t = function(x) x))
    ),
    {
      uploaded_raw(data.frame(
        stringsAsFactors = FALSE,
        plot = "PLOT1", tag = 1, dbh = 12.5, quadrat = "A1"
      ))
      census_choices(censuses)
      link_policy(c(stem_diameter = "always", quadrat = "never"))

      session$setInputs(
        data_format = "wide", map_plot_name = "plot", map_tag = "tag",
        trait_map_dbh = "stem_diameter", trait_map_quadrat = "quadrat"
      )

      # a diameter belongs to a campaign, so the latest census is offered
      expect_setequal(mapped_traits(), c("stem_diameter", "quadrat"))
      expect_false(nothing_to_link())

      # drop the diameter and only the quadrat is left: nothing to link to,
      # and the census the box had filled in for itself is taken back out
      session$setInputs(trait_map_dbh = "")
      expect_equal(mapped_traits(), "quadrat")
      expect_true(nothing_to_link())

      # the observer that empties the box acted on the transition
      # (updateSelectInput itself is a noop under a mock session)
      expect_true(last_link_state())
    }
  )
})

test_that("the box explains what a census link does to the data", {
  html <- as.character(.census_link_explainer(fake_i18n))

  # the three consequences, each traceable to the code that causes it:
  # the per-census pivot, the census date, and growth computation
  expect_match(html, "stem_diameter_census_1", fixed = TRUE)
  expect_match(html, "dated by the census", fixed = TRUE)
  expect_match(html, "growth computation cannot see it", fixed = TRUE)
  expect_match(html, "averaged into one column", fixed = TRUE)
})
