# Tests for R/helpers_traits_common.R
# pivot_numeric_traits_generic() and pivot_categorical_traits_generic()

# =============================================================================
# pivot_numeric_traits_generic()
# =============================================================================

test_that("pivot_numeric_traits_generic returns NULL for empty data", {
  empty_df <- data.frame(
    idtax = integer(0), trait = character(0),
    traitvalue = character(0), stringsAsFactors = FALSE
  )
  expect_null(pivot_numeric_traits_generic(empty_df, "idtax"))
})

test_that("pivot_numeric_traits_generic with stats produces mean/sd/n columns", {
  df <- make_numeric_trait_data()
  result <- pivot_numeric_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    include_stats = TRUE,
    include_id_measures = FALSE
  )

  expect_s3_class(result, "tbl_df")

  # Should have idtax + mean/sd/n for each trait (height, dbh)
  expect_true("mean_height" %in% names(result))
  expect_true("sd_height" %in% names(result))
  expect_true("n_height" %in% names(result))
  expect_true("mean_dbh" %in% names(result))
  expect_true("sd_dbh" %in% names(result))
  expect_true("n_dbh" %in% names(result))

  # 2 unique idtax values

  expect_equal(nrow(result), 2)
})

test_that("pivot_numeric_traits_generic computes correct statistics", {
  df <- make_numeric_trait_data()
  result <- pivot_numeric_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    include_stats = TRUE,
    include_id_measures = FALSE
  )

  # idtax 1 height: 10.5, 12.3 -> mean = 11.4
  row1 <- result[result$idtax == 1, ]
  expect_equal(row1$mean_height, round(mean(c(10.5, 12.3)), 2))
  expect_equal(row1$n_height, 2)

  # idtax 1 dbh: 30.0 -> mean = 30.0, n = 1
  expect_equal(row1$mean_dbh, 30.0)
  expect_equal(row1$n_dbh, 1)

  # idtax 2 dbh: 25.0, 27.0 -> mean = 26.0
  row2 <- result[result$idtax == 2, ]
  expect_equal(row2$mean_dbh, 26.0)
  expect_equal(row2$n_dbh, 2)
})

test_that("pivot_numeric_traits_generic without stats uses mean aggregation", {
  df <- make_numeric_trait_data()
  result <- pivot_numeric_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    include_stats = FALSE,
    include_id_measures = FALSE
  )

  # Should have trait names as columns, not mean_/sd_/n_ prefixed
  expect_true("height" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_false("mean_height" %in% names(result))
})

test_that("pivot_numeric_traits_generic respects name_prefix", {
  df <- make_numeric_trait_data()
  result <- pivot_numeric_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    include_stats = TRUE,
    include_id_measures = FALSE,
    name_prefix = "taxa_"
  )

  expect_true("taxa_mean_height" %in% names(result))
  expect_true("taxa_sd_dbh" %in% names(result))
  expect_true("taxa_n_dbh" %in% names(result))
})

test_that("pivot_numeric_traits_generic includes id_trait_measures when requested", {
  df <- make_numeric_trait_data()
  result <- pivot_numeric_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    include_stats = TRUE,
    include_id_measures = TRUE
  )

  expect_true("id_trait_measures_height" %in% names(result))
  expect_true("id_trait_measures_dbh" %in% names(result))

  # Check idtax 1 height measures are "101, 102"
  row1 <- result[result$idtax == 1, ]
  ids <- unlist(strsplit(row1$id_trait_measures_height, ", "))
  expect_true(all(c("101", "102") %in% ids))
})

# =============================================================================
# pivot_categorical_traits_generic()
# =============================================================================

test_that("pivot_categorical_traits_generic returns NULL for empty data", {
  empty_df <- data.frame(
    idtax = integer(0), trait = character(0),
    traitvalue_char = character(0), stringsAsFactors = FALSE
  )
  expect_null(pivot_categorical_traits_generic(empty_df, "idtax"))
})

test_that("pivot_categorical_traits_generic mode picks most frequent value", {
  # bark for idtax 1: "smooth" (1x), "rough" (1x) -> tie, first in order
  # We need a clear majority to test mode reliably
  df <- data.frame(
    idtax           = c(1, 1, 1, 2),
    trait           = c("bark", "bark", "bark", "bark"),
    traitvalue_char = c("smooth", "smooth", "rough", "rough"),
    stringsAsFactors = FALSE
  )

  result <- pivot_categorical_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    aggregation_mode = "mode",
    include_id_measures = FALSE
  )

  expect_s3_class(result, "tbl_df")

  # idtax 1: "smooth" appears 2x vs "rough" 1x
  expect_equal(result$bark[result$idtax == 1], "smooth")
  # idtax 2: only "rough"
  expect_equal(result$bark[result$idtax == 2], "rough")
})

test_that("pivot_categorical_traits_generic concat joins unique values", {
  df <- make_categorical_trait_data()
  result <- pivot_categorical_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    aggregation_mode = "concat",
    include_id_measures = FALSE
  )

  # idtax 1 bark: "smooth" and "rough" concatenated
  bark_1 <- result$bark[result$idtax == 1]
  expect_true(grepl("smooth", bark_1))
  expect_true(grepl("rough", bark_1))
})

test_that("pivot_categorical_traits_generic respects name_prefix", {
  df <- make_categorical_trait_data()
  result <- pivot_categorical_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    aggregation_mode = "mode",
    include_id_measures = FALSE,
    name_prefix = "cat_"
  )

  expect_true("cat_bark" %in% names(result))
  expect_true("cat_leaf" %in% names(result))
})

test_that("pivot_categorical_traits_generic includes id_trait_measures", {
  df <- make_categorical_trait_data()
  result <- pivot_categorical_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    aggregation_mode = "mode",
    include_id_measures = TRUE
  )

  expect_true("id_trait_measures_bark" %in% names(result))
  expect_true("id_trait_measures_leaf" %in% names(result))
})

test_that("pivot_categorical_traits_generic handles NA traitvalue_char", {
  df <- data.frame(
    idtax           = c(1, 1),
    trait           = c("bark", "bark"),
    traitvalue_char = c("smooth", NA),
    stringsAsFactors = FALSE
  )

  result <- pivot_categorical_traits_generic(
    data.table::copy(data.table::as.data.table(df)),
    id_col = "idtax",
    aggregation_mode = "mode",
    include_id_measures = FALSE
  )

  # Should still return a result with the non-NA value
  expect_equal(result$bark[result$idtax == 1], "smooth")
})
