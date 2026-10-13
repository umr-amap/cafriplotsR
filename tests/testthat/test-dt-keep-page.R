# .dt_keep_page_options(): a redrawn table reopens where the user was

test_that("a fresh table starts on page 1 with the default length", {
  opts <- .dt_keep_page_options(NULL, page_length = 12)
  expect_true(opts$stateSave)
  expect_equal(opts$pageLength, 12)
  expect_null(opts$displayStart)
  expect_null(opts$search)
})

test_that("the last state carries page, length and search over", {
  state <- list(start = 50, length = 25, search = list(search = "22.1"))
  opts <- .dt_keep_page_options(state, page_length = 12)
  expect_equal(opts$displayStart, 50L)
  expect_equal(opts$pageLength, 25L)
  expect_equal(opts$search, list(search = "22.1"))
})

test_that("an empty or malformed state falls back to the defaults", {
  opts <- .dt_keep_page_options(
    list(start = 0, length = NULL, search = list(search = "")), 25)
  expect_null(opts$displayStart)
  expect_equal(opts$pageLength, 25)
  expect_null(opts$search)

  # "Show all" is -1: kept, it is a real choice
  expect_equal(.dt_keep_page_options(list(length = -1), 25)$pageLength, -1L)
})

test_that("the options build a datatable", {
  dt <- DT::datatable(data.frame(a = 1:60),
                      options = .dt_keep_page_options(list(start = 25, length = 25)))
  expect_equal(dt$x$options$displayStart, 25L)
})
