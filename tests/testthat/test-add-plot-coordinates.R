# Tests for the reshaping done by add_plot_coordinates() and its helpers.
# Nothing here touches the database: launch_add_data stays FALSE and con is NULL.

quiet_coordinates <- function(...) {
  res <- NULL
  suppressMessages(
    utils::capture.output(res <- add_plot_coordinates(...))
  )
  res
}

jalons_fixture <- function() {
  tibble::tibble(
    plot_name = c("p1", "p1", "p1", "p1", "p2", "p2"),
    X_theo = c(0, 0, 20, 100, 0, 40),
    Y_theo = c(0, 0, 0, 100, 0, 0),
    Latitude = c(1, 3, 5, 7, 10, 20),
    Longitude = c(2, 4, 6, 8, 11, 21)
  )
}

test_that(".match_column finds a column, tolerates case, and errors otherwise", {
  data <- tibble::tibble(plot_name = "p1", x_theo = 0, Latitude = 1)

  expect_equal(.match_column(data, "plot_name", "plot_name_field"), "plot_name")
  expect_message(
    expect_equal(.match_column(data, "X_theo", "X_theo"), "x_theo"),
    "x_theo"
  )
  expect_error(.match_column(data, "Y_theo", "Y_theo"), "not found in dataset")
  expect_error(.match_column(data, c("a", "b"), "X_theo"), "single column name")
})

test_that(".format_quadrat_component writes whole numbers without decimals", {
  expect_equal(.format_quadrat_component(c(0, 20, 100)), c("0", "20", "100"))
  expect_equal(.format_quadrat_component(c(2.5, NA)), c("2.5", NA))
  expect_equal(.format_quadrat_component(c(" 20 ", "")), c("20", NA))
})

test_that(".first_non_na returns the first available value", {
  expect_equal(.first_non_na(c(NA, 2, 3)), 2)
  expect_equal(.first_non_na(c("a", "b")), "a")
  expect_true(is.na(.first_non_na(c(NA_character_, NA_character_))))
})

test_that("add_plot_coordinates pivots one row per plot and one column per quadrat", {
  res <- quiet_coordinates(jalons_fixture())

  expect_named(res, c("ddlat", "ddlon"))
  expect_equal(nrow(res$ddlat), 2L)
  expect_equal(res$ddlat$plot_name, c("p1", "p2"))

  # quadrats sorted by X then Y, not by order of appearance
  expect_equal(
    names(res$ddlat),
    c("plot_name", "ddlat_plot_X_Y_0_0", "ddlat_plot_X_Y_20_0",
      "ddlat_plot_X_Y_40_0", "ddlat_plot_X_Y_100_100")
  )
  expect_equal(
    names(res$ddlon),
    c("plot_name", "ddlon_plot_X_Y_0_0", "ddlon_plot_X_Y_20_0",
      "ddlon_plot_X_Y_40_0", "ddlon_plot_X_Y_100_100")
  )
})

test_that("add_plot_coordinates averages repeated measurements of a quadrat", {
  res <- quiet_coordinates(jalons_fixture())

  expect_equal(res$ddlat$ddlat_plot_X_Y_0_0[res$ddlat$plot_name == "p1"], 2)
  expect_equal(res$ddlon$ddlon_plot_X_Y_0_0[res$ddlon$plot_name == "p1"], 3)
})

test_that("quadrats missing for a plot are NA, never NaN", {
  res <- quiet_coordinates(jalons_fixture())

  expect_true(is.na(res$ddlat$ddlat_plot_X_Y_40_0[res$ddlat$plot_name == "p1"]))
  expect_false(any(vapply(res$ddlat[-1], function(x) any(is.nan(x)), logical(1))))
  expect_false(any(vapply(res$ddlon[-1], function(x) any(is.nan(x)), logical(1))))
})

test_that("a missing quadrat column is an error, not a single bogus quadrat", {
  data <- jalons_fixture() %>% dplyr::rename(x_position = X_theo)

  # regression: tidy evaluation used to fall back on the X_theo argument itself
  # and build one quadrat literally named "X_theo_Y_theo"
  expect_error(quiet_coordinates(data), "not found in dataset")
})

test_that("column names are matched case-insensitively", {
  data <- jalons_fixture() %>%
    dplyr::rename(x_theo = X_theo, y_theo = Y_theo)

  res <- quiet_coordinates(data)

  expect_true("ddlat_plot_X_Y_0_0" %in% names(res$ddlat))
})

test_that("additional columns are carried, one value per plot", {
  data <- jalons_fixture() %>%
    dplyr::mutate(coly = 2026, colm = 6, colnam = "Someone")

  res <- quiet_coordinates(data, add_cols = c("coly", "colm"),
                           cor_cols = c("year", "month"),
                           collector_field = "colnam")

  expect_true(all(c("coly", "colm", "colnam") %in% names(res$ddlat)))
  expect_equal(res$ddlat$coly, c(2026, 2026))
  expect_equal(res$ddlat$colnam, c("Someone", "Someone"))
})

test_that("add_cols and cor_cols must have the same length", {
  expect_error(
    quiet_coordinates(jalons_fixture(), add_cols = c("a", "b"), cor_cols = "year"),
    "same length"
  )
})

test_that("rows without a plot name or without a quadrat are dropped", {
  data <- tibble::tibble(
    plot_name = c("p1", "p1", NA),
    X_theo = c(0, NA, 40),
    Y_theo = c(0, 0, 0),
    Latitude = c(1, 2, 3),
    Longitude = c(4, 5, 6)
  )

  res <- quiet_coordinates(data)

  expect_equal(nrow(res$ddlat), 1L)
  expect_equal(names(res$ddlat), c("plot_name", "ddlat_plot_X_Y_0_0"))
})

test_that("a dataset without any usable row is an error", {
  data <- tibble::tibble(
    plot_name = NA_character_, X_theo = 0, Y_theo = 0,
    Latitude = 1, Longitude = 2
  )

  expect_error(quiet_coordinates(data), "no row with both a plot name")
})
