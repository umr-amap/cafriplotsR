# Tests for R/verbosity.R
#
# A full query narrates every internal step with cli. The filter mutes whole
# severity classes by dismissing the cli_message condition, so what has to be
# pinned down is which classes survive - and that the two things which must
# never be muted (warnings, and anything printed while asking the user a
# question) always come through.

test_that(".resolve_verbosity accepts level names, logicals and NULL", {
  expect_equal(.resolve_verbosity("quiet"), "quiet")
  expect_equal(.resolve_verbosity("debug"), "debug")
  expect_equal(.resolve_verbosity(FALSE), "quiet")
  expect_equal(.resolve_verbosity(TRUE), "debug")

  withr::with_options(list(CafriplotsR.verbose = NULL), {
    expect_equal(.resolve_verbosity(NULL), "normal")
  })
  withr::with_options(list(CafriplotsR.verbose = "quiet"), {
    expect_equal(.resolve_verbosity(NULL), "quiet")
  })
})

test_that(".resolve_verbosity rejects anything else", {
  expect_error(.resolve_verbosity("loud"))
  expect_error(.resolve_verbosity(c("quiet", "debug")))
  expect_error(.resolve_verbosity(3))
})

test_that("normal verbosity mutes progress chatter but never warnings", {
  messages <- testthat::capture_messages(
    .with_cli_verbosity("normal", {
      cli::cli_h2("a section header")
      cli::cli_alert_info("a step starting")
      cli::cli_alert_success("a step finished")
      cli::cli_alert_warning("data was dropped")
      cli::cli_alert_danger("a step failed")
    })
  )

  expect_true(any(grepl("data was dropped", messages)))
  expect_true(any(grepl("a step failed", messages)))
  expect_false(any(grepl("a step starting", messages)))
  expect_false(any(grepl("a step finished", messages)))
  expect_false(any(grepl("a section header", messages)))
})

test_that("debug verbosity keeps every message", {
  messages <- testthat::capture_messages(
    .with_cli_verbosity("debug", {
      cli::cli_alert_info("a step starting")
      cli::cli_alert_warning("data was dropped")
    })
  )

  expect_true(any(grepl("a step starting", messages)))
  expect_true(any(grepl("data was dropped", messages)))
})

test_that("plain messages and warnings are left alone", {
  # Only cli output is filtered: base R conditions belong to the caller
  expect_message(
    .with_cli_verbosity("quiet", message("a plain message")),
    "a plain message"
  )
  expect_warning(
    .with_cli_verbosity("quiet", warning("a real warning")),
    "a real warning"
  )
})

test_that(".with_cli_verbosity returns the value of its expression", {
  expect_equal(.with_cli_verbosity("quiet", 1 + 1), 2)
  expect_equal(.with_cli_verbosity("debug", 1 + 1), 2)
})

test_that(".with_cli_verbosity restores the previous level, even on error", {
  before <- .verbosity()

  expect_error(.with_cli_verbosity("quiet", stop("boom")), "boom")
  expect_equal(.verbosity(), before)
})

test_that(".verbose_output lifts the filter for interactive prompts", {
  messages <- testthat::capture_messages(
    .with_cli_verbosity("quiet", {
      cli::cli_alert_info("muted")
      .verbose_output(cli::cli_alert_info("a question the user must see"))
      cli::cli_alert_info("muted again")
    })
  )

  expect_true(any(grepl("a question the user must see", messages)))
  expect_false(any(grepl("muted", messages)))
})

test_that(".verbosity_at_least orders the levels", {
  .with_cli_verbosity("quiet", {
    expect_true(.verbosity_at_least("quiet"))
    expect_false(.verbosity_at_least("normal"))
    expect_false(.verbosity_at_least("debug"))
  })
  .with_cli_verbosity("debug", {
    expect_true(.verbosity_at_least("quiet"))
    expect_true(.verbosity_at_least("normal"))
    expect_true(.verbosity_at_least("debug"))
  })
})

test_that("the tally accumulates counts and names, and resets", {
  .tally_reset()
  expect_null(.tally_get("dead_individuals"))

  .tally_add("dead_individuals", 10)
  .tally_add("dead_individuals", 5)
  expect_equal(.tally_get("dead_individuals"), 15)

  .tally_add("dropped_traits", c("tree_height", "crown_width"))
  .tally_add("dropped_traits", "tree_height")
  expect_equal(.tally_get("dropped_traits"), c("tree_height", "crown_width"))

  .tally_add("dead_individuals", integer(0))
  expect_equal(.tally_get("dead_individuals"), 15)

  .tally_reset()
  expect_null(.tally_get("dropped_traits"))
})

test_that("the query summary states counts, exclusions and tables", {
  .tally_reset()
  .tally_add("dead_individuals", 784)
  .tally_add("flagged_measurements", 1090)

  messages <- testthat::capture_messages(
    .report_query_summary(
      list(
        individuals     = data.frame(a = 1:7085),
        metadata        = data.frame(a = 1:16),
        height_diameter = data.frame(a = 1:3)
      ),
      opts = list(census_strategy = "last", issues = "remove")
    )
  )

  txt <- paste(messages, collapse = " ")

  expect_match(txt, "16 plots")
  expect_match(txt, "7085 individuals")
  expect_match(txt, "784 dead individuals")
  expect_match(txt, "1090 measurements")
  expect_match(txt, "height_diameter")
})

test_that("every exclusion names the argument that controls it", {
  # A count of removed rows is only actionable with the knob that puts them back
  .tally_reset()
  .tally_add("dead_individuals", 130)
  .tally_add("flagged_measurements", 656)

  txt <- paste(
    testthat::capture_messages(
      .report_query_summary(
        list(individuals = data.frame(a = 1), metadata = data.frame(a = 1)),
        opts = list(census_strategy = "first", issues = "remove")
      )
    ),
    collapse = " "
  )

  expect_match(txt, "first census")
  expect_match(txt, "census_strategy")
  expect_match(txt, "show_multiple_census")
  expect_match(txt, "issues")
  expect_match(txt, "include")
})

test_that("the query summary explains genus-level aggregation when it happened", {
  .tally_reset()
  tables <- list(individuals = data.frame(a = 1), metadata = data.frame(a = 1))

  on_txt <- paste(testthat::capture_messages(
    .report_query_summary(tables, opts = list(traits_to_genera = TRUE))
  ), collapse = " ")

  off_txt <- paste(testthat::capture_messages(
    .report_query_summary(tables, opts = list(traits_to_genera = FALSE))
  ), collapse = " ")

  expect_match(on_txt, "genus-level")
  expect_match(on_txt, "traits_to_genera")
  expect_match(on_txt, "source_", fixed = TRUE)
  expect_false(grepl("traits_to_genera", off_txt))
})

test_that("the query summary explains the family-level wood density fallback", {
  .tally_reset()
  tables <- list(individuals = data.frame(a = 1), metadata = data.frame(a = 1))

  txt <- paste(testthat::capture_messages(
    .report_query_summary(tables, opts = list(wd_fam_level = TRUE))
  ), collapse = " ")

  expect_match(txt, "wd_fam_level")
  expect_match(txt, "family")
})

test_that("the query summary handles a bare data frame and an empty tally", {
  .tally_reset()

  messages <- testthat::capture_messages(
    .report_query_summary(data.frame(a = 1:16), opts = list())
  )

  expect_true(any(grepl("16 plots", messages)))
  expect_false(any(grepl("Excluded", messages)))
})

test_that("the query summary never lets a malformed result break the query", {
  .tally_reset()
  expect_silent(res <- .report_query_summary(NULL))
  expect_null(res)
})

test_that("query_plots forwards every argument to its implementation", {
  # The wrapper repeats the signature by hand; a formal added to one and not
  # the other would silently stop being honoured
  wrapper <- setdiff(names(formals(query_plots)), "verbose")
  impl <- names(formals(CafriplotsR:::.query_plots_impl))

  expect_equal(wrapper, impl)

  body_txt <- paste(deparse(body(query_plots)), collapse = " ")
  for (arg in impl) {
    expect_match(body_txt, paste0(arg, " = ", arg), fixed = TRUE)
  }
})

test_that("query_plots mutes the pipeline log but reports its summary", {
  testthat::local_mocked_bindings(
    .query_plots_impl = function(...) {
      cli::cli_h2("Processing individuals")
      cli::cli_alert_info("Fetching individuals")
      cli::cli_alert_success("Query completed")
      cli::cli_alert_warning("Dropped 1 trait with no measurement")
      .tally_add("dead_individuals", 784)
      list(individuals = data.frame(a = 1:100), metadata = data.frame(a = 1:16))
    }
  )

  messages <- testthat::capture_messages(query_plots())

  expect_false(any(grepl("Fetching individuals", messages)))
  expect_false(any(grepl("Query completed", messages)))
  expect_true(any(grepl("Dropped 1 trait", messages)))
  expect_true(any(grepl("16 plots", messages)))
  expect_true(any(grepl("784 dead individuals", messages)))
})

test_that("query_plots(verbose = 'debug') restores the full log", {
  testthat::local_mocked_bindings(
    .query_plots_impl = function(...) {
      cli::cli_alert_info("Fetching individuals")
      data.frame(a = 1:16)
    }
  )

  messages <- testthat::capture_messages(query_plots(verbose = "debug"))
  expect_true(any(grepl("Fetching individuals", messages)))
})

test_that("query_plots(verbose = 'quiet') reports nothing but warnings", {
  testthat::local_mocked_bindings(
    .query_plots_impl = function(...) {
      cli::cli_alert_info("Fetching individuals")
      cli::cli_alert_warning("Dropped 1 trait with no measurement")
      list(individuals = data.frame(a = 1:100), metadata = data.frame(a = 1:16))
    }
  )

  messages <- testthat::capture_messages(query_plots(verbose = "quiet"))

  expect_true(any(grepl("Dropped 1 trait", messages)))
  expect_false(any(grepl("Fetching individuals", messages)))
  expect_false(any(grepl("16 plots", messages)))
})

test_that("query_plots clears the tally between calls", {
  testthat::local_mocked_bindings(
    .query_plots_impl = function(...) data.frame(a = 1:16)
  )

  .tally_add("dead_individuals", 999)
  messages <- testthat::capture_messages(query_plots())

  expect_false(any(grepl("999", messages)))
})

test_that("query_plots passes the arguments it was called with to the summary", {
  testthat::local_mocked_bindings(
    .query_plots_impl = function(...) {
      .tally_add("dead_individuals", 130)
      .tally_add("flagged_measurements", 656)
      list(individuals = data.frame(a = 1:10), metadata = data.frame(a = 1:2))
    }
  )

  txt <- paste(testthat::capture_messages(
    query_plots(census_strategy = "first", traits_to_genera = TRUE)
  ), collapse = " ")

  expect_match(txt, "first census")
  expect_match(txt, "genus-level")
})

test_that("query_plots reports the default census strategy, not the raw default vector", {
  # census_strategy is match.arg()ed inside the implementation, so what the
  # wrapper holds is still c("last", "first", "mean") unless the caller chose
  testthat::local_mocked_bindings(
    .query_plots_impl = function(...) {
      .tally_add("dead_individuals", 5)
      list(individuals = data.frame(a = 1), metadata = data.frame(a = 1))
    }
  )

  txt <- paste(testthat::capture_messages(query_plots()), collapse = " ")

  expect_match(txt, "last census")
  expect_false(grepl("c(\"last\"", txt, fixed = TRUE))
})
