# Tests for R/aggregate_individual_traits.R
#
# Only the pure aggregation kernel `.compute_aggregate()` is exercised here —
# the DB-touching pieces (`.fetch_individual_values`, `.aggregate_one_rule`,
# `rebuild_aggregated_taxa_traits`) require a live PostgreSQL connection and
# are intended to be covered by integration tests in a separate harness.

# =============================================================================
# .compute_aggregate() — numeric methods
# =============================================================================

test_that(".compute_aggregate returns NA value and n=0 on empty input", {
  res <- .compute_aggregate(numeric(0), method = "mean")
  expect_identical(res$n, 0L)
  expect_true(is.na(res$value_num))
  expect_true(is.na(res$value_char))
})

test_that(".compute_aggregate strips NA before aggregating", {
  res <- .compute_aggregate(c(1, 2, NA, 3), method = "mean")
  expect_identical(res$n, 3L)
  expect_equal(res$value_num, 2)
})

test_that(".compute_aggregate computes mean / median / min / max / sum", {
  v <- c(2, 4, 4, 4, 5, 5, 7, 9)
  expect_equal(.compute_aggregate(v, "mean")$value_num,   5)
  expect_equal(.compute_aggregate(v, "median")$value_num, 4.5)
  expect_equal(.compute_aggregate(v, "min")$value_num,    2)
  expect_equal(.compute_aggregate(v, "max")$value_num,    9)
  expect_equal(.compute_aggregate(v, "sum")$value_num,    sum(v))
})

test_that(".compute_aggregate sd needs at least two values", {
  expect_true(is.na(.compute_aggregate(5, "sd")$value_num))
  expect_equal(.compute_aggregate(c(1, 3), "sd")$value_num, stats::sd(c(1, 3)))
})

test_that(".compute_aggregate percentile uses method_param", {
  v <- 1:100
  res95 <- .compute_aggregate(v, "percentile", method_param = 95)
  res50 <- .compute_aggregate(v, "percentile", method_param = 50)
  expect_equal(res95$value_num,
               unname(stats::quantile(v, 0.95, names = FALSE)))
  expect_equal(res50$value_num,
               unname(stats::quantile(v, 0.50, names = FALSE)))
})

test_that(".compute_aggregate percentile validates parameter", {
  expect_error(.compute_aggregate(1:10, "percentile"), "method_param required")
  expect_error(.compute_aggregate(1:10, "percentile", method_param = -1),
               "in \\[0, 100\\]")
  expect_error(.compute_aggregate(1:10, "percentile", method_param = 101),
               "in \\[0, 100\\]")
})

test_that(".compute_aggregate count returns the non-NA length", {
  expect_equal(.compute_aggregate(c(1, NA, 2, NA, 3), "count")$value_num, 3)
  expect_equal(.compute_aggregate(numeric(0),         "count")$value_num, NA_real_)
})

# =============================================================================
# .compute_aggregate() — character methods
# =============================================================================

test_that(".compute_aggregate mode returns the most frequent value", {
  res <- .compute_aggregate(c("a", "b", "a", "c"), method = "mode")
  expect_equal(res$value_char, "a")
  expect_identical(res$n, 4L)
})

test_that(".compute_aggregate concat returns unique values joined by ;", {
  res <- .compute_aggregate(c("a", "b", "a"), method = "concat")
  # uniqueness preserved, separator is "; "
  expect_equal(sort(strsplit(res$value_char, "; ")[[1]]), c("a", "b"))
})

test_that(".compute_aggregate trims whitespace-only strings out", {
  res <- .compute_aggregate(c("a", "  ", "", "b"), method = "concat")
  expect_equal(sort(strsplit(res$value_char, "; ")[[1]]), c("a", "b"))
  expect_identical(res$n, 2L)
})

# =============================================================================
# .compute_aggregate() — error handling
# =============================================================================

test_that(".compute_aggregate rejects unknown methods", {
  expect_error(.compute_aggregate(1:5, "geometric_mean"),
               "Unknown aggregation method")
})

test_that(".compute_aggregate coerces character input for numeric methods", {
  res <- .compute_aggregate(c("1", "2", "abc"), "mean")
  # "abc" -> NA_real_ via suppressWarnings; result is mean of (1, 2)
  expect_equal(res$value_num, 1.5)
  expect_identical(res$n, 2L)
})

# =============================================================================
# .derived_trait_suffix() — derived trait naming
# =============================================================================

test_that(".derived_trait_suffix encodes percentile with integer param", {
  expect_identical(.derived_trait_suffix("percentile", 95),   "p95")
  expect_identical(.derived_trait_suffix("percentile", 50.0), "p50")
})

test_that(".derived_trait_suffix uses method name for plain methods", {
  expect_identical(.derived_trait_suffix("mean"),   "mean")
  expect_identical(.derived_trait_suffix("max"),    "max")
  expect_identical(.derived_trait_suffix("median"), "median")
  expect_identical(.derived_trait_suffix("sd"),     "sd")
  expect_identical(.derived_trait_suffix("mode"),   "mode")
  expect_identical(.derived_trait_suffix("concat"), "concat")
})

test_that(".derived_trait_suffix requires param for percentile", {
  expect_error(.derived_trait_suffix("percentile"), "method_param required")
  expect_error(.derived_trait_suffix("percentile", NA_real_), "method_param required")
})
