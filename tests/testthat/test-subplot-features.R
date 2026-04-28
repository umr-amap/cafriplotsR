# Tests for pure helpers in R/subsplots_features_function.R

test_that('aggregate_numeric_plot_features computes per-plot means and returns NULL for empty input', {
  empty <- tibble::tibble(id_table_liste_plots = integer(), type = character(), typevalue = character())
  expect_null(aggregate_numeric_plot_features(empty))

  data <- tibble::tibble(
    id_table_liste_plots = c(1L, 1L, 1L, 2L, 2L),
    type = c('plot_area', 'plot_area', 'slope', 'plot_area', 'slope'),
    typevalue = c('1.0', '2.0', '10', '4.0', '20')
  )

  result <- aggregate_numeric_plot_features(data)

  expect_s3_class(result, 'tbl_df')
  expect_equal(result$plot_area[result$id_table_liste_plots == 1], 1.5)
  expect_equal(result$slope[result$id_table_liste_plots == 1], 10)
  expect_equal(result$plot_area[result$id_table_liste_plots == 2], 4)
  expect_equal(result$slope[result$id_table_liste_plots == 2], 20)
})

test_that('aggregate_character_plot_features trims and collapses unique values', {
  empty <- tibble::tibble(id_table_liste_plots = integer(), type = character(), typevalue_char = character())
  expect_null(aggregate_character_plot_features(empty))

  data <- tibble::tibble(
    id_table_liste_plots = c(1L, 1L, 1L, 2L, 2L),
    type = c('forest_type', 'forest_type', 'forest_type', 'disturbance', 'disturbance'),
    typevalue_char = c(' Evergreen ', 'Evergreen', ' Semi-deciduous ', 'Logged', NA_character_)
  )

  result <- aggregate_character_plot_features(data)

  expect_s3_class(result, 'tbl_df')
  forest_1 <- result$forest_type[result$id_table_liste_plots == 1]
  expect_match(forest_1, 'Evergreen')
  expect_match(forest_1, 'Semi-deciduous')
  expect_equal(result$disturbance[result$id_table_liste_plots == 2], 'Logged')
})

test_that('extract_census_info summarizes the maximum census number by plot', {
  data <- tibble::tibble(
    id_table_liste_plots = c(1L, 1L, 1L, 2L, 2L),
    type = c('census', 'census', 'plot_area', 'census', 'census'),
    typevalue = c('1', '3', 'ignored', '1', '2')
  )

  result <- extract_census_info(data)

  expect_s3_class(result, 'tbl_df')
  expect_equal(result$number_of_census[result$id_table_liste_plots == 1], 3)
  expect_equal(result$number_of_census[result$id_table_liste_plots == 2], 2)
})

test_that('extract_census_dates returns widened dates and julian dates, dropping invalid rows', {
  expect_null(extract_census_dates(tibble::tibble()))

  census_data <- tibble::tibble(
    id_table_liste_plots = c(1L, 1L, 2L, 2L),
    typevalue = c('1', '2', '1', '2'),
    day = c(1L, NA_integer_, 15L, 31L),
    month = c(1L, 2L, 6L, 2L),
    year = c(2020L, 2021L, 2019L, NA_integer_)
  )

  result <- extract_census_dates(census_data)

  expect_s3_class(result, 'tbl_df')
  expect_true(all(c('date_census_1', 'date_census_2', 'date_census_julian_1', 'date_census_julian_2') %in% names(result)))
  expect_equal(result$date_census_1[result$id_table_liste_plots == 1], as.Date('2020-01-01'))
  expect_equal(result$date_census_2[result$id_table_liste_plots == 1], as.Date('2021-02-01'))
  expect_equal(result$date_census_1[result$id_table_liste_plots == 2], as.Date('2019-06-15'))
  expect_true(is.na(result$date_census_2[result$id_table_liste_plots == 2]))
  expect_true(is.numeric(result$date_census_julian_1))
})

test_that('aggregate_plot_features combines numeric, character, and census-derived outputs', {
  data <- tibble::tibble(
    id_table_liste_plots = c(1L, 1L, 1L, 1L, 2L, 2L, 2L),
    valuetype = c('numeric', 'numeric', 'character', 'character', 'numeric', 'character', 'character'),
    type = c('plot_area', 'census', 'forest_type', 'census', 'plot_area', 'forest_type', 'census'),
    typevalue = c('1.5', '1', NA_character_, '2', '3.0', NA_character_, '1'),
    typevalue_char = c(NA_character_, NA_character_, ' Evergreen ', NA_character_, NA_character_, 'Dry forest', NA_character_),
    day = c(NA_integer_, 1L, NA_integer_, 1L, NA_integer_, NA_integer_, 15L),
    month = c(NA_integer_, 1L, NA_integer_, 2L, NA_integer_, NA_integer_, 6L),
    year = c(NA_integer_, 2020L, NA_integer_, 2021L, NA_integer_, NA_integer_, 2019L)
  )

  result <- aggregate_plot_features(data, con = NULL)

  expect_s3_class(result, 'tbl_df')
  expect_true(all(c('id_table_liste_plots', 'plot_area', 'forest_type', 'date_census_1') %in% names(result)))
  expect_equal(result$plot_area[result$id_table_liste_plots == 1], 1.5)
  expect_equal(result$plot_area[result$id_table_liste_plots == 2], 3.0)
  expect_equal(result$forest_type[result$id_table_liste_plots == 1], 'Evergreen')
  expect_equal(result$forest_type[result$id_table_liste_plots == 2], 'Dry forest')
  expect_equal(result$date_census_1[result$id_table_liste_plots == 1], as.Date('2020-01-01'))
})
