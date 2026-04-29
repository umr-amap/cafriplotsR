# Tests for non-interactive helpers in R/add_plot_features_user_friendly.R

test_that('.identify_plot_id_column validates explicit columns and auto-detects common cases', {
  data <- tibble::tibble(plot_name = c('P1', 'P2'), id_liste_plots = c(1L, 2L), other = 1:2)

  expect_equal(CafriplotsR:::.identify_plot_id_column(data, 'plot_name', interactive = FALSE, verbose = FALSE),
               list(column = 'plot_name', type = 'name'))
  expect_equal(CafriplotsR:::.identify_plot_id_column(data, 'id_liste_plots', interactive = FALSE, verbose = FALSE),
               list(column = 'id_liste_plots', type = 'id'))
  expect_equal(CafriplotsR:::.identify_plot_id_column(data, NULL, interactive = FALSE, verbose = FALSE),
               list(column = 'plot_name', type = 'name'))

  fuzzy_name <- tibble::tibble(my_plot_name = c('P1', 'P2'))
  fuzzy_id <- tibble::tibble(plot_identifier = c(1L, 2L))
  expect_equal(CafriplotsR:::.identify_plot_id_column(fuzzy_name, NULL, interactive = FALSE, verbose = FALSE),
               list(column = 'my_plot_name', type = 'name'))
  expect_equal(CafriplotsR:::.identify_plot_id_column(fuzzy_id, NULL, interactive = FALSE, verbose = FALSE),
               list(column = 'plot_identifier', type = 'id'))
})

test_that('.identify_plot_id_column errors cleanly when detection fails or column is missing', {
  expect_error(
    CafriplotsR:::.identify_plot_id_column(tibble::tibble(site = c('A', 'B')), 'missing_col', interactive = FALSE, verbose = FALSE),
    'Specified plot_id_column'
  )

  expect_error(
    CafriplotsR:::.identify_plot_id_column(tibble::tibble(site = c('A', 'B')), NULL, interactive = FALSE, verbose = FALSE),
    'Could not identify plot ID column'
  )
})

test_that('.get_subplot_feature_synonyms returns expected feature aliases', {
  synonyms <- CafriplotsR:::.get_subplot_feature_synonyms()

  expect_type(synonyms, 'list')
  expect_true('plot_area' %in% names(synonyms))
  expect_true('census_date' %in% names(synonyms))
  expect_true('area' %in% synonyms$plot_area)
  expect_true('date' %in% synonyms$census_date)
})

test_that('.find_fuzzy_match returns best match above threshold and NULL otherwise', {
  valid <- c('plot_area', 'vegetation_type', 'locality_name')

  expect_equal(CafriplotsR:::.find_fuzzy_match('plotarea', valid, threshold = 0.6), 'plot_area')
  expect_null(CafriplotsR:::.find_fuzzy_match('zzz', valid, threshold = 0.95))
})

test_that('.map_subplot_feature_columns handles exact, synonym, fuzzy, skip, and explicit mapping validation', {
  data <- tibble::tibble(
    plot_name = c('P1', 'P2'),
    plot_area = c('1.5', '2.0'),
    area = c('1.5', '2.0'),
    vegtype = c('Forest', 'Savanna'),
    mystery = c('x', 'y'),
    year = c(2020L, 2021L)
  )

  subplot_info <- tibble::tibble(type = c('plot_area', 'vegetation_type', 'census_date'))

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    subplot_list = function(con = NULL) subplot_info,
    .find_synonym_match = function(col, synonyms) {
      if (identical(col, 'area')) 'plot_area' else NULL
    },
    .find_fuzzy_match = function(col, valid_values, threshold = 0.6) {
      if (identical(col, 'vegtype')) 'vegetation_type' else NULL
    }
  )

  result <- CafriplotsR:::.map_subplot_feature_columns(
    data,
    plot_id_column = 'plot_name',
    column_mapping = NULL,
    con = structure(list(), class = 'mock_con'),
    interactive = FALSE,
    similarity_threshold = 0.6,
    verbose = FALSE
  )

  expect_equal(result$mappings$plot_area, 'plot_area')
  expect_equal(result$mappings$area, 'plot_area')
  expect_equal(result$mappings$vegtype, 'vegetation_type')
  expect_false('year' %in% names(result$mappings))
  expect_false('mystery' %in% names(result$mappings))

  expect_error(
    CafriplotsR:::.map_subplot_feature_columns(
      data,
      plot_id_column = 'plot_name',
      column_mapping = list(area = 'not_a_feature'),
      con = structure(list(), class = 'mock_con'),
      interactive = FALSE,
      similarity_threshold = 0.6,
      verbose = FALSE
    ),
    'Invalid feature types'
  )

  expect_error(
    CafriplotsR:::.map_subplot_feature_columns(
      data,
      plot_id_column = 'plot_name',
      column_mapping = list(missing_col = 'plot_area'),
      con = structure(list(), class = 'mock_con'),
      interactive = FALSE,
      similarity_threshold = 0.6,
      verbose = FALSE
    ),
    'Columns in column_mapping not found'
  )
})

test_that('.validate_plot_features_data reports missing plots, invalid features, and empty-value warnings', {
  data <- tibble::tibble(
    plot_name = c('P1', 'P_missing'),
    area = c('1.5', ''),
    lead = c('Alice', NA_character_)
  )

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    query_plots = function(plot_name, con, exact_match = TRUE) tibble::tibble(plot_name = 'P1'),
    subplot_list = function(con = NULL) tibble::tibble(type = c('plot_area', 'team_leader'))
  )

  result <- CafriplotsR:::.validate_plot_features_data(
    data,
    plot_id_column = 'plot_name',
    plot_id_type = 'name',
    column_mappings = list(area = 'plot_area', lead = 'not_real'),
    con = structure(list(), class = 'mock_con'),
    verbose = FALSE
  )

  expect_false(result$valid)
  expect_true(any(grepl('Plots not found', result$errors)))
  expect_true(any(grepl('Invalid subplot feature types', result$errors)))
  expect_true(any(grepl("Column 'area' has 1 empty/NA values", result$warnings)))
  expect_true(any(grepl("Column 'lead' has 1 empty/NA values", result$warnings)))
})

test_that('.prepare_subplot_features links plot names, renames feature columns, and supports dry-run people features', {
  data <- tibble::tibble(
    plot_name = c('P1', 'P1', 'P2'),
    area = c('1.5', '', '2.0'),
    team = c('Alice, Bob', 'Alice', 'Charlie')
  )

  subplot_info <- tibble::tibble(
    type = c('plot_area', 'team_leader'),
    valuetype = c('numeric', 'table_colnam')
  )

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    try_open_postgres_table = function(table, con) tibble::tibble(plot_name = c('P1', 'P2'), id_liste_plots = c(10L, 20L)),
    subplot_list = function(con = NULL) subplot_info
  )

  result <- CafriplotsR:::.prepare_subplot_features(
    data,
    plot_id_column = 'plot_name',
    plot_id_type = 'name',
    column_mappings = list(area = 'plot_area', team = 'team_leader'),
    con = structure(list(), class = 'mock_con'),
    interactive = FALSE,
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_true(all(c('plot_area', 'team_leader') %in% names(result$features)))

  area_data <- result$features$plot_area
  expect_equal(names(area_data), c('id_liste_plots', 'plot_area'))
  expect_equal(area_data$id_liste_plots, c(10L, 20L))
  expect_equal(area_data$plot_area, c('1.5', '2.0'))

  people_data <- result$features$team_leader
  expect_true('team_leader_id' %in% names(people_data))
  expect_equal(people_data$id_liste_plots, c(10L, 10L, 10L, 20L))
  expect_equal(people_data$team_leader, c('Alice', 'Bob', 'Alice', 'Charlie'))
  expect_true(all(people_data$team_leader_id == 999))
})

test_that('.prepare_subplot_features handles id-based plot columns directly', {
  data <- tibble::tibble(id_liste_plots = c(1L, 2L), area = c('1.0', '2.0'))
  subplot_info <- tibble::tibble(type = 'plot_area', valuetype = 'numeric')

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    subplot_list = function(con = NULL) subplot_info
  )

  result <- CafriplotsR:::.prepare_subplot_features(
    data,
    plot_id_column = 'id_liste_plots',
    plot_id_type = 'id',
    column_mappings = list(area = 'plot_area'),
    con = structure(list(), class = 'mock_con'),
    interactive = FALSE,
    dry_run = FALSE,
    verbose = FALSE
  )

  expect_equal(result$features$plot_area$id_liste_plots, c(1L, 2L))
  expect_equal(result$features$plot_area$plot_area, c('1.0', '2.0'))
})
