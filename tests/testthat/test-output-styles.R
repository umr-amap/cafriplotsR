# Tests for R/output_styles_helpers.R and R/output_styles_config.R
# All functions are pure data transformations - no database required.

# =============================================================================
# .detect_style_from_method()   (output_styles_config.R)
# =============================================================================

test_that(".detect_style_from_method returns 'standard' when no method column", {
  df <- data.frame(plot_name = "P1", country = "Gabon", stringsAsFactors = FALSE)
  expect_equal(.detect_style_from_method(df), "standard")
})

test_that(".detect_style_from_method returns 'standard' when method is all NA", {
  df <- data.frame(method = NA_character_, stringsAsFactors = FALSE)
  expect_equal(.detect_style_from_method(df), "standard")
})

test_that(".detect_style_from_method returns 'standard' for mixed methods", {
  df <- data.frame(
    method = c("1 ha plot", "Long Transect"),
    stringsAsFactors = FALSE
  )
  expect_equal(.detect_style_from_method(df), "standard")
})

test_that(".detect_style_from_method maps known permanent-plot method", {
  df <- data.frame(method = rep("1 ha plot", 3), stringsAsFactors = FALSE)
  expect_equal(.detect_style_from_method(df), "permanent_plot")
})

test_that(".detect_style_from_method maps known transect method", {
  df <- data.frame(method = rep("Long Transect", 2), stringsAsFactors = FALSE)
  expect_equal(.detect_style_from_method(df), "transect")
})

test_that(".detect_style_from_method returns 'standard' for unknown method", {
  df <- data.frame(method = rep("Unknown survey type", 2), stringsAsFactors = FALSE)
  expect_equal(.detect_style_from_method(df), "standard")
})

# =============================================================================
# .extract_census_table()
# =============================================================================

test_that(".extract_census_table returns NULL for NULL input", {
  main <- data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  expect_null(.extract_census_table(NULL, main))
})

test_that(".extract_census_table returns NULL for non-data.frame input", {
  main <- data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  expect_null(.extract_census_table(list(a = 1), main))
})

test_that(".extract_census_table renames typevalue to census_number", {
  feats <- make_census_features()
  main  <- data.frame(plot_name = c("P1", "P2"), stringsAsFactors = FALSE)
  result <- .extract_census_table(feats, main)

  expect_true("census_number" %in% names(result))
  expect_false("typevalue" %in% names(result))
})

test_that(".extract_census_table creates census_date from valid year and month", {
  feats <- make_census_features()
  main  <- data.frame(plot_name = c("P1", "P2"), stringsAsFactors = FALSE)
  result <- .extract_census_table(feats, main)

  expect_true("census_date" %in% names(result))
  # P1 census 1: 2010-03
  row1 <- result[result$plot_name == "P1" & result$census_number == 1, ]
  expect_equal(row1$census_date, "2010-03")
  # P1 census 2: 2015-07
  row2 <- result[result$plot_name == "P1" & result$census_number == 2, ]
  expect_equal(row2$census_date, "2015-07")
})

test_that(".extract_census_table filters to plots present in main_data", {
  feats <- make_census_features()  # has P1 and P2
  main  <- data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  result <- .extract_census_table(feats, main)

  expect_true(all(result$plot_name == "P1"))
  expect_false("P2" %in% result$plot_name)
})

test_that(".extract_census_table sets census_date to NA for invalid dates", {
  feats <- data.frame(
    plot_name = c("P1", "P1"),
    typevalue = c(1, 2),
    year      = c(1800, 2010),   # 1800 < 1900 -> invalid
    month     = c(3, 13),        # month 13 -> invalid
    stringsAsFactors = FALSE
  )
  main  <- data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  result <- .extract_census_table(feats, main)

  expect_true("census_date" %in% names(result))
  expect_true(all(is.na(result$census_date)))
})

test_that(".extract_census_table does not error when all dates are invalid", {
  feats <- data.frame(
    plot_name = "P1",
    typevalue = 1,
    year      = NA_real_,
    month     = NA_real_,
    stringsAsFactors = FALSE
  )
  main <- data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  expect_no_error(.extract_census_table(feats, main))
  result <- .extract_census_table(feats, main)
  expect_true(is.na(result$census_date))
})

test_that(".extract_census_table orders result by plot_name then census_number", {
  feats <- make_census_features()  # P1 c1, P1 c2, P2 c1
  main  <- data.frame(plot_name = c("P1", "P2"), stringsAsFactors = FALSE)
  result <- .extract_census_table(feats, main)

  expect_equal(result$plot_name,     c("P1", "P1", "P2"))
  expect_equal(result$census_number, c(1, 2, 1))
})

# =============================================================================
# .extract_height_diameter_pairs()  - single census
# =============================================================================

test_that(".extract_height_diameter_pairs returns NULL when no height column", {
  data <- data.frame(id_n = 1L, plot_name = "P1", tag = "T1",
                     stem_diameter = 300, stringsAsFactors = FALSE)
  expect_null(.extract_height_diameter_pairs(data, show_multiple_census = FALSE))
})

test_that(".extract_height_diameter_pairs returns NULL when no diameter column", {
  data <- data.frame(id_n = 1L, plot_name = "P1", tag = "T1",
                     tree_height = 25.0, stringsAsFactors = FALSE)
  expect_null(.extract_height_diameter_pairs(data, show_multiple_census = FALSE))
})

test_that(".extract_height_diameter_pairs extracts D and H from canonical column names", {
  data <- make_plot_individual_data()
  result <- .extract_height_diameter_pairs(data, show_multiple_census = FALSE)

  expect_true(is.data.frame(result))
  expect_true("D" %in% names(result))
  expect_true("H" %in% names(result))
  expect_true("id_n" %in% names(result))
  expect_true("plot_name" %in% names(result))
  expect_true("tag" %in% names(result))
})

test_that(".extract_height_diameter_pairs filters rows where D or H is NA", {
  data <- make_plot_individual_data()  # row 4 has NA stem_diameter and tree_height
  result <- .extract_height_diameter_pairs(data, show_multiple_census = FALSE)

  expect_false(any(is.na(result$D)))
  expect_false(any(is.na(result$H)))
  # Only 3 of 4 rows should survive
  expect_equal(nrow(result), 3)
})

test_that(".extract_height_diameter_pairs returns NULL when all rows have NA D or H", {
  data <- data.frame(
    id_n = 1L, plot_name = "P1", tag = "T1",
    stem_diameter = NA_real_, tree_height = NA_real_,
    stringsAsFactors = FALSE
  )
  expect_null(.extract_height_diameter_pairs(data, show_multiple_census = FALSE))
})

test_that(".extract_height_diameter_pairs includes POM column when available", {
  data <- make_plot_individual_data()  # has height_of_stem_diameter
  result <- .extract_height_diameter_pairs(data, show_multiple_census = FALSE)

  expect_true("POM" %in% names(result))
})

test_that(".extract_height_diameter_pairs handles 'dbh' alias for diameter", {
  data <- data.frame(
    id_n = 1L, plot_name = "P1", tag = "T1",
    dbh = 300, tree_height = 25.0,
    stringsAsFactors = FALSE
  )
  result <- .extract_height_diameter_pairs(data, show_multiple_census = FALSE)

  expect_false(is.null(result))
  expect_true("D" %in% names(result))
  expect_equal(result$D, 300)
})

test_that(".extract_height_diameter_pairs handles 'height' alias for tree height", {
  data <- data.frame(
    id_n = 1L, plot_name = "P1", tag = "T1",
    stem_diameter = 300, height = 25.0,
    stringsAsFactors = FALSE
  )
  result <- .extract_height_diameter_pairs(data, show_multiple_census = FALSE)

  expect_false(is.null(result))
  expect_true("H" %in% names(result))
  expect_equal(result$H, 25.0)
})

test_that(".extract_height_diameter_pairs handles 'pom' alias for POM", {
  data <- data.frame(
    id_n = 1L, plot_name = "P1", tag = "T1",
    stem_diameter = 300, tree_height = 25.0, pom = 1.3,
    stringsAsFactors = FALSE
  )
  result <- .extract_height_diameter_pairs(data, show_multiple_census = FALSE)

  expect_true("POM" %in% names(result))
  expect_equal(result$POM, 1.3)
})

# =============================================================================
# .extract_height_diameter_pairs()  - multi census
# =============================================================================

test_that(".extract_height_diameter_pairs pivots multi-census columns to long format", {
  data   <- make_multi_census_data()
  result <- .extract_height_diameter_pairs(data, show_multiple_census = TRUE)

  expect_false(is.null(result))
  # 2 individuals × 2 censuses = 4 rows (all have height data)
  expect_equal(nrow(result), 4)
  expect_true("D" %in% names(result))
  expect_true("H" %in% names(result))
})

test_that(".extract_height_diameter_pairs multi-census filters rows with NA tree_height", {
  data <- make_multi_census_data()
  # Remove census 2 height for individual 102
  data$tree_height_census_2[data$id_n == 102L] <- NA
  result <- .extract_height_diameter_pairs(data, show_multiple_census = TRUE)

  # 4 total pairs but 1 removed -> 3 rows
  expect_equal(nrow(result), 3)
})

test_that(".extract_height_diameter_pairs multi-census includes POM when available", {
  data   <- make_multi_census_data()  # has height_of_stem_diameter_census_N
  result <- .extract_height_diameter_pairs(data, show_multiple_census = TRUE)

  expect_true("POM" %in% names(result))
})

# =============================================================================
# .extract_metadata_table()
# =============================================================================

test_that(".extract_metadata_table always renames id_liste_plots to plot_id", {
  data   <- make_plot_individual_data()
  config <- .plot_output_styles$minimal
  result <- .extract_metadata_table(data, meta_data = NULL, config,
                                    extract_individuals = TRUE)

  expect_true("plot_id" %in% names(result))
  expect_false("id_liste_plots" %in% names(result))
})

test_that(".extract_metadata_table returns one row per plot", {
  data   <- make_plot_individual_data()  # 4 rows, 2 plots
  config <- .plot_output_styles$minimal
  result <- .extract_metadata_table(data, meta_data = NULL, config,
                                    extract_individuals = TRUE)

  expect_equal(nrow(result), 2)
  expect_true(all(c("P1", "P2") %in% result$plot_name))
})

test_that(".extract_metadata_table applies rename_columns (ddlat -> latitude)", {
  data   <- make_plot_individual_data()
  config <- .plot_output_styles$minimal  # renames ddlat/ddlon
  result <- .extract_metadata_table(data, meta_data = NULL, config,
                                    extract_individuals = TRUE)

  expect_true("latitude" %in% names(result))
  expect_true("longitude" %in% names(result))
  expect_false("ddlat" %in% names(result))
  expect_false("ddlon" %in% names(result))
})

test_that(".extract_metadata_table uses meta_data when provided instead of data", {
  data <- make_plot_individual_data()
  # meta_data has an extra column not in data
  meta <- data.frame(
    id_liste_plots = c(1L, 2L),
    plot_name      = c("P1", "P2"),
    country        = c("Gabon", "Cameroon"),
    ddlat          = c(-0.5, 3.2),
    ddlon          = c(12.1, 11.8),
    extra_col      = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  config <- .plot_output_styles$minimal
  result <- .extract_metadata_table(data, meta_data = meta, config,
                                    extract_individuals = TRUE)

  # Should come from meta: extra_col exists only there
  expect_equal(nrow(result), 2)
})

test_that(".extract_metadata_table remove_patterns excludes matching columns", {
  data <- make_plot_individual_data()  # has date_modif_y
  config <- .plot_output_styles$minimal  # remove_patterns includes "^date_modif"
  result <- .extract_metadata_table(data, meta_data = NULL, config,
                                    extract_individuals = TRUE)

  # date_modif_y should be removed
  expect_false("date_modif_y" %in% names(result))
})

test_that(".extract_metadata_table 'all' with extract_individuals=FALSE returns all distinct rows", {
  data   <- make_plot_individual_data()
  config <- .plot_output_styles$full  # metadata_columns = "all"
  result <- .extract_metadata_table(data, meta_data = NULL, config,
                                    extract_individuals = FALSE)

  expect_equal(nrow(result), 2)  # distinct by plot_name
  expect_true(all(c("stem_diameter", "tree_height") %in% names(result)))
})

test_that(".extract_metadata_table keep_common_features includes feat_ cols above 10% threshold", {
  data <- make_plot_individual_data()
  # Add a feature column present in >10% of rows
  data$feat_soil_type <- c("clay", "clay", "sand", "sand")
  # Add a feature column present in 0% of rows
  data$feat_rare_col  <- NA_character_

  config <- .plot_output_styles$standard  # has keep_common_features = TRUE
  result <- .extract_metadata_table(data, meta_data = NULL, config,
                                    extract_individuals = TRUE)

  expect_true("feat_soil_type" %in% names(result))
  expect_false("feat_rare_col" %in% names(result))
})

# =============================================================================
# .extract_individuals_table()
# =============================================================================

test_that(".extract_individuals_table returns data unchanged when individuals_columns is 'all'", {
  data   <- make_plot_individual_data()
  config <- .plot_output_styles$full  # individuals_columns = "all"
  result <- .extract_individuals_table(data, config, show_multiple_census = FALSE)

  expect_identical(result, data)
})

test_that(".extract_individuals_table always keeps essential columns", {
  data <- make_plot_individual_data()
  # Use minimal config which specifies only a subset
  config <- .plot_output_styles$minimal
  result <- .extract_individuals_table(data, config, show_multiple_census = FALSE)

  # Essential: id_n, plot_name, tag, tax_fam, tax_gen, tax_sp_level, stem_diameter
  expect_true("id_n" %in% names(result))
  expect_true("plot_name" %in% names(result))
  expect_true("tag" %in% names(result))
})

test_that(".extract_individuals_table applies rename_columns for individuals", {
  data   <- make_plot_individual_data()
  config <- .plot_output_styles$minimal  # renames tax_fam->family, tax_gen->genus, etc.
  result <- .extract_individuals_table(data, config, show_multiple_census = FALSE)

  expect_true("family" %in% names(result))
  expect_true("genus" %in% names(result))
  expect_true("species" %in% names(result))
  expect_false("tax_fam" %in% names(result))
  expect_false("tax_gen" %in% names(result))
})

test_that(".extract_individuals_table with multi-census keeps _census_N columns", {
  data   <- make_multi_census_data()
  config <- .plot_output_styles$permanent_plot_multi_census  # keep_census_columns = TRUE
  result <- .extract_individuals_table(data, config, show_multiple_census = TRUE)

  census_cols <- grep("_census_\\d+$", names(result), value = TRUE)
  expect_true(length(census_cols) > 0)
})

test_that(".extract_individuals_table census_column_renames renames stem_diameter_census_N to dbh_census_N", {
  data   <- make_multi_census_data()
  config <- .plot_output_styles$permanent_plot_multi_census
  result <- .extract_individuals_table(data, config, show_multiple_census = TRUE)

  expect_true("dbh_census_1" %in% names(result))
  expect_true("dbh_census_2" %in% names(result))
  expect_false("stem_diameter_census_1" %in% names(result))
})

test_that(".extract_individuals_table census_column_renames renames tree_height_census_N to height_census_N", {
  data   <- make_multi_census_data()
  config <- .plot_output_styles$permanent_plot_multi_census
  result <- .extract_individuals_table(data, config, show_multiple_census = TRUE)

  expect_true("height_census_1" %in% names(result))
  expect_true("height_census_2" %in% names(result))
  expect_false("tree_height_census_1" %in% names(result))
})

# =============================================================================
# .apply_output_style()
# =============================================================================

test_that(".apply_output_style returns a plot_query_list object", {
  data   <- make_plot_individual_data()
  result <- .apply_output_style(data, style = "minimal",
                                extract_individuals = TRUE)

  expect_s3_class(result, "plot_query_list")
})

test_that(".apply_output_style sets style attribute", {
  data   <- make_plot_individual_data()
  result <- .apply_output_style(data, style = "minimal",
                                extract_individuals = TRUE)

  expect_equal(attr(result, "style"), "minimal")
})

test_that(".apply_output_style always has a metadata element", {
  data   <- make_plot_individual_data()
  result <- .apply_output_style(data, style = "standard",
                                extract_individuals = FALSE)

  expect_true("metadata" %in% names(result))
  expect_true(is.data.frame(result$metadata))
})

test_that(".apply_output_style includes individuals element when extract_individuals = TRUE", {
  data   <- make_plot_individual_data()
  result <- .apply_output_style(data, style = "minimal",
                                extract_individuals = TRUE)

  expect_true("individuals" %in% names(result))
})

test_that(".apply_output_style omits individuals element when extract_individuals = FALSE", {
  data   <- make_plot_individual_data()
  result <- .apply_output_style(data, style = "minimal",
                                extract_individuals = FALSE)

  expect_false("individuals" %in% names(result))
})

test_that(".apply_output_style falls back to 'standard' for unknown style", {
  data   <- make_plot_individual_data()
  # Should warn but still produce a valid result
  result <- suppressMessages(
    .apply_output_style(data, style = "nonexistent_style_xyz",
                        extract_individuals = FALSE)
  )

  expect_s3_class(result, "plot_query_list")
  expect_true("metadata" %in% names(result))
})

test_that(".apply_output_style upgrades permanent_plot to permanent_plot_multi_census when show_multiple_census = TRUE", {
  data   <- make_multi_census_data()
  result <- suppressMessages(
    .apply_output_style(data, style = "permanent_plot",
                        extract_individuals = TRUE,
                        show_multiple_census = TRUE)
  )

  expect_equal(attr(result, "style"), "permanent_plot_multi_census")
})

test_that(".apply_output_style accepts list input (res_list structure)", {
  data <- make_plot_individual_data()
  res_list <- list(
    extract      = data,
    meta_data    = data[!duplicated(data$plot_name), ],
    census_features = NULL,
    coordinates  = NULL,
    coordinates_sf = NULL
  )
  result <- .apply_output_style(res_list, style = "minimal",
                                extract_individuals = TRUE)

  expect_s3_class(result, "plot_query_list")
  expect_true("metadata" %in% names(result))
})

# =============================================================================
# print.plot_query_list()
# =============================================================================

test_that("print.plot_query_list returns x invisibly", {
  data <- make_plot_individual_data()
  obj  <- .apply_output_style(data, style = "minimal",
                              extract_individuals = TRUE)

  ret <- withVisible(print(obj))
  expect_false(ret$visible)
  expect_identical(ret$value, obj)
})

test_that("print.plot_query_list does not error on minimal valid object", {
  obj <- structure(
    list(metadata = data.frame(plot_name = "P1")),
    class = c("plot_query_list", "list"),
    style = "minimal",
    style_description = "test"
  )
  expect_no_error(suppressMessages(print(obj)))
})
