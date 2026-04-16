# Tests for R/growth_census_functions.R
# .trim.growth_internal() is pure; compute_growth/compute_mortality input validation

# =============================================================================
# .trim.growth_internal() - pure function, no DB
# =============================================================================

test_that(".trim.growth_internal accepts normal growth", {
  data <- data.frame(
    dbh_mm_1  = c(200, 300),
    dbh_mm_2  = c(210, 315),
    time_diff = c(1, 1)
  )
  result <- .trim.growth_internal(data)

  expect_true("accepted_growth" %in% names(result))
  expect_true(all(result$accepted_growth))
})

test_that(".trim.growth_internal rejects excessive negative growth", {

  # Negative growth beyond err.limit * stdev threshold
  data <- data.frame(
    dbh_mm_1  = c(200),
    dbh_mm_2  = c(100),   # Lost 100mm - very negative
    time_diff = c(1)
  )
  result <- .trim.growth_internal(data, err.limit = 4)
  expect_false(result$accepted_growth[1])
})

test_that(".trim.growth_internal rejects excessive positive growth", {
  data <- data.frame(
    dbh_mm_1  = c(200),
    dbh_mm_2  = c(500),   # 300mm in 1 year = 300mm/yr > maxgrow
    time_diff = c(1)
  )
  result <- .trim.growth_internal(data, maxgrow = 75)
  expect_false(result$accepted_growth[1])
})

test_that(".trim.growth_internal rejects below minimum DBH", {
  data <- data.frame(
    dbh_mm_1  = c(50),    # Below default mindbh of 100
    dbh_mm_2  = c(55),
    time_diff = c(1)
  )
  result <- .trim.growth_internal(data, mindbh = 100)
  expect_false(result$accepted_growth[1])
})

test_that(".trim.growth_internal rejects NA measurements", {
  data <- data.frame(
    dbh_mm_1  = c(NA, 200, 200),
    dbh_mm_2  = c(210, NA, 0),
    time_diff = c(1, 1, 1)
  )
  result <- .trim.growth_internal(data)

  # NA dbh_mm_1 -> rejected (line 546)
  expect_false(result$accepted_growth[1])
  # NA dbh_mm_2 -> rejected (line 546)
  expect_false(result$accepted_growth[2])
  # dbh_mm_2 <= 0 -> rejected (line 546)
  expect_false(result$accepted_growth[3])
})

test_that(".trim.growth_internal respects custom parameters", {
  data <- data.frame(
    dbh_mm_1  = c(200),
    dbh_mm_2  = c(350),   # 150mm/yr growth
    time_diff = c(1)
  )

  # Default maxgrow=75 -> rejected

  result_strict <- .trim.growth_internal(data, maxgrow = 75)
  expect_false(result_strict$accepted_growth[1])

  # Relaxed maxgrow=200 -> accepted
  result_relaxed <- .trim.growth_internal(data, maxgrow = 200)
  expect_true(result_relaxed$accepted_growth[1])
})

test_that(".trim.growth_internal handles multi-row data correctly", {
  data <- data.frame(
    dbh_mm_1  = c(200, 300, 50, 200),
    dbh_mm_2  = c(210, 600, 55, 205),  # normal, too fast, too small, normal
    time_diff = c(1, 1, 1, 1)
  )
  result <- .trim.growth_internal(data, maxgrow = 75, mindbh = 100)

  expect_equal(result$accepted_growth, c(TRUE, FALSE, FALSE, TRUE))
})

# =============================================================================
# compute_growth() - input validation (no DB call needed for these checks)
# =============================================================================

test_that("compute_growth errors when both plot_ids and plot_names are NULL", {
  expect_error(
    compute_growth(plot_ids = NULL, plot_names = NULL),
    "Either plot_ids or plot_names must be provided"
  )
})

# =============================================================================
# compute_mortality() - input validation
# =============================================================================

test_that("compute_mortality errors when both plot_ids and plot_names are NULL", {
  expect_error(
    compute_mortality(plot_ids = NULL, plot_names = NULL),
    "Either plot_ids or plot_names must be provided"
  )
})

test_that("compute_growth returns summary and individual data from mocked query results", {
  query_result <- list(
    extract = make_growth_query_extract(),
    census_features = data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  )

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    query_plots = function(...) query_result
  )

  result <- compute_growth(plot_ids = 1, con = structure(list(), class = "mock_con"))

  expect_type(result, "list")
  expect_true(all(c("summary", "individuals") %in% names(result)))
  expect_equal(nrow(result$summary), 1)
  expect_equal(result$summary$plot_name[[1]], "P1")
  expect_equal(result$summary$n_individuals[[1]], 1)
  expect_equal(result$summary$n_valid[[1]], 1)
  expect_equal(result$summary$n_excluded[[1]], 0)
  expect_equal(result$summary$mean_growth_mm_yr[[1]], 10)
  expect_equal(nrow(result$individuals), 1)
  expect_equal(result$individuals$growthrate[[1]], 10)
  expect_true(all(c("tax_fam", "tax_gen", "tax_sp_level", "idtax_n") %in% names(result$individuals)))
})

test_that("compute_growth supports exponential method and summary-only output", {
  query_result <- list(
    extract = make_growth_query_extract(),
    census_features = data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  )

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    query_plots = function(...) query_result
  )

  result <- compute_growth(plot_ids = 1, con = structure(list(), class = "mock_con"), method = "E", return_individual = FALSE)

  expect_true("summary" %in% names(result))
  expect_false("individuals" %in% names(result))
  expect_equal(result$summary$mean_growth_mm_yr[[1]], log(210 / 200))
})

test_that("compute_growth errors when query results do not include multiple censuses", {
  query_result <- list(
    extract = data.frame(id_table_liste_plots_n = 1L, plot_name = "P1", stringsAsFactors = FALSE),
    census_features = data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  )

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    query_plots = function(...) query_result
  )

  expect_error(
    compute_growth(plot_ids = 1, con = structure(list(), class = "mock_con")),
    "Need at least 2 censuses"
  )
})

test_that("compute_mortality returns mortality and recruitment summaries from mocked data", {
  query_result <- list(
    extract = make_growth_query_extract(),
    census_features = data.frame(plot_name = "P1", stringsAsFactors = FALSE)
  )

  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    query_plots = function(...) query_result
  )

  result <- compute_mortality(plot_ids = 1, con = structure(list(), class = "mock_con"))

  expect_type(result, "list")
  expect_true(all(c("summary", "dead_individuals", "recruits") %in% names(result)))
  expect_equal(nrow(result$summary), 1)
  expect_equal(result$summary$N_outset[[1]], 2)
  expect_equal(result$summary$N_dead[[1]], 1)
  expect_equal(result$summary$N_survivor[[1]], 1)
  expect_equal(result$summary$N_recruits[[1]], 1)
  expect_equal(nrow(result$dead_individuals), 1)
  expect_equal(nrow(result$recruits), 1)
  expect_equal(result$dead_individuals$dbh_at_death_cm[[1]], 18)
  expect_equal(result$recruits$dbh_at_recruitment_cm[[1]], 12)
})

test_that("compute_mortality errors when query result lacks census_features", {
  testthat::local_mocked_bindings(.package = "CafriplotsR", 
    query_plots = function(...) list(extract = make_growth_query_extract())
  )

  expect_error(
    compute_mortality(plot_ids = 1, con = structure(list(), class = "mock_con")),
    "No census data found"
  )
})

