# Tests for R/column_documentation.R

test_that('.invert_style_renames returns renamed columns plus plot_id mapping', {
  result <- CafriplotsR:::.invert_style_renames('standard', 'individuals')

  expect_equal(result[['family']], 'tax_fam')
  expect_equal(result[['genus']], 'tax_gen')
  expect_equal(result[['species']], 'tax_sp_level')
  expect_equal(result[['plot_id']], 'id_liste_plots')
  expect_equal(attr(result, 'style'), 'standard')
})

test_that('.invert_census_renames inverts configured census prefixes', {
  result <- CafriplotsR:::.invert_census_renames('permanent_plot_multi_census')

  expect_equal(result[['dbh']], 'stem_diameter')
  expect_equal(result[['height']], 'tree_height')
})

test_that('.parse_pivot_pattern handles supported suffix patterns', {
  expect_equal(CafriplotsR:::.parse_pivot_pattern('issue_agg_height')$suffix_type, 'issue')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('issue_agg_height_2')$suffix_type, 'issue_census')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('ids_agg_height')$suffix_type, 'ids')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('ids_agg_height_2')$suffix_type, 'ids_census')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('char_bark')$suffix_type, 'char')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('char_bark_2')$suffix_type, 'char_census')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('stem_diameter_census_2')$suffix_type, 'census')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('height_mean')$suffix_type, 'mean')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('height_sd')$suffix_type, 'sd')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('height_n')$suffix_type, 'n')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('dbh_0')$suffix_type, 'pair_0')
  expect_equal(CafriplotsR:::.parse_pivot_pattern('dbh_1')$suffix_type, 'pair_1')
  expect_null(CafriplotsR:::.parse_pivot_pattern('plain_column'))
})

test_that('.document_single_column adds rename and pivot notes and falls back cleanly', {
  inverted <- CafriplotsR:::.invert_style_renames('permanent_plot_multi_census', 'individuals')
  descriptions <- list(
    stem_diameter = list(description = 'Stem diameter', category = 'Measurements', expectedunit = 'cm'),
    bark = list(description = 'Bark texture', category = 'Traits', expectedunit = '')
  )

  renamed <- CafriplotsR:::.document_single_column('family', inverted, descriptions)
  expect_equal(renamed$original_name, 'tax_fam')
  expect_match(renamed$notes, "Renamed from 'tax_fam'")

  census <- CafriplotsR:::.document_single_column('dbh_census_2', inverted, descriptions)
  expect_equal(census$original_name, 'stem_diameter_census_2')
  expect_equal(census$description, 'Stem diameter')
  expect_equal(census$unit, 'cm')
  expect_match(census$notes, 'Census: 2')

  pivoted <- CafriplotsR:::.document_single_column('char_bark_2', inverted, descriptions)
  expect_equal(pivoted$description, 'Bark texture')
  expect_match(pivoted$notes, 'Character/categorical value')

  fallback <- CafriplotsR:::.document_single_column('mystery_feature', inverted, descriptions)
  expect_equal(fallback$description, 'Mystery feature')
  expect_equal(fallback$category, 'Other')
})

test_that('describe_columns handles plain data frames and plot_query_list objects', {
  descriptions <- list(
    ddlat = list(description = 'Latitude', category = 'Coordinates', expectedunit = 'degrees'),
    tax_fam = list(description = 'Family', category = 'Taxonomy', expectedunit = ''),
    stem_diameter = list(description = 'Stem diameter', category = 'Measurements', expectedunit = 'cm')
  )

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    call.mydb = function() structure(list(), class = 'mock_con'),
    .get_column_descriptions = function(con, table_type = 'plots') descriptions
  )

  plain <- structure(data.frame(family = 'Fabaceae', dbh_census_1 = 10, check.names = FALSE), style = 'permanent_plot_multi_census')
  plain_doc <- describe_columns(plain)
  expect_s3_class(plain_doc, 'column_documentation_table')
  expect_true(all(c('column_name', 'description', 'notes') %in% names(plain_doc)))
  expect_true(any(plain_doc$column_name == 'family'))
  expect_true(any(grepl('Census: 1', plain_doc$notes)))

  result_list <- list(
    metadata = data.frame(plot_id = 1L, latitude = 1.2),
    individuals = data.frame(family = 'Fabaceae')
  )
  class(result_list) <- c('plot_query_list', 'list')
  attr(result_list, 'style') <- 'standard'

  list_doc <- describe_columns(result_list)
  expect_s3_class(list_doc, 'column_documentation')
  expect_true(all(c('metadata', 'individuals') %in% names(list_doc)))
  expect_true(is.data.frame(list_doc$metadata))
  expect_true(is.data.frame(list_doc$individuals))
})
