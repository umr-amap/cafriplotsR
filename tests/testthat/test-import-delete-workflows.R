test_that("import_individual_data rejects invalid validation results before any DB work", {
  expect_error(
    import_individual_data(
      individuals_data = tibble::tibble(plot_name = "P1"),
      validation = list(valid = FALSE)
    ),
    "Data validation failed"
  )
})

test_that("import_individual_data returns a cancelled result when confirmation is declined", {
  testthat::local_mocked_bindings(
    .package = "base",
    readline = function(prompt = "") "no"
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(con, sql) data.frame(current_user = "tester")
  )

  result <- import_individual_data(
    individuals_data = tibble::tibble(plot_name = "P1", idtax_n = 1L),
    con = structure(list(), class = "mock_connection"),
    ask_confirmation = TRUE,
    progress = FALSE
  )

  expect_false(result$success)
  expect_equal(result$message, "Import cancelled by user")
  expect_false(result$dry_run)
})

test_that("safe_delete_plot validates inputs and connection state before querying", {
  expect_error(
    safe_delete_plot(plot_ids = integer(0), con = structure(list(), class = "mock_connection")),
    "plot_ids must be non-empty integer vector"
  )

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) FALSE
  )

  expect_error(
    safe_delete_plot(plot_ids = 1L, con = structure(list(), class = "mock_connection")),
    "Invalid database connection"
  )
})

test_that("safe_delete_plot returns a dry-run summary with related counts", {
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(con, sql) {
      sql_chr <- gsub("\\s+", " ", as.character(sql))
      if (grepl("FROM data_liste_plots", sql_chr, fixed = TRUE)) {
        return(data.frame(id_liste_plots = 1L, plot_name = "P1"))
      }
      if (grepl("COUNT(*) as n FROM data_individuals", sql_chr, fixed = TRUE)) {
        return(data.frame(n = 2L))
      }
      if (grepl("COUNT(*) as n FROM data_traits_measures", sql_chr, fixed = TRUE)) {
        return(data.frame(n = 3L))
      }
      if (grepl("COUNT(*) as n FROM data_ind_measures_feat", sql_chr, fixed = TRUE)) {
        return(data.frame(n = 4L))
      }
      if (grepl("COUNT(*) as n FROM data_liste_sub_plots", sql_chr, fixed = TRUE)) {
        return(data.frame(n = 5L))
      }
      stop(sprintf("Unexpected query: %s", sql_chr))
    }
  )

  summary <- safe_delete_plot(
    plot_ids = 1L,
    con = structure(list(), class = "mock_connection"),
    dry_run = TRUE,
    verbose = FALSE
  )

  expect_true(summary$dry_run)
  expect_equal(summary$plot_ids, 1L)
  expect_equal(summary$counts$individuals, 2L)
  expect_equal(summary$counts$trait_measurements, 3L)
  expect_equal(summary$counts$measurement_features, 4L)
  expect_equal(summary$counts$subplots, 5L)
  expect_equal(summary$deleted$plots, 0L)
})
