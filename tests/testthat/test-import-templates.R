tmpl <- function(type, ...) {
  testthat::with_mocked_bindings(
    .get_column_descriptions_from_db = function(con = NULL) list(),
    get_plot_metadata_template(type, ...)
  )
}

test_that("every template type starts with the three required lookup columns", {
  for (type in c("minimal", "permanent_plot", "transect", "nested_plot", "full")) {
    expect_equal(names(tmpl(type))[1:3], c("plot_name", "method", "country"))
  }
})

test_that("deprecated forest_type is no longer offered", {
  expect_false("forest_type" %in% names(tmpl("full")))
})

test_that("hierarchy columns are in nested_plot and full templates only", {
  expect_true(all(c("parent_plot", "parent_relation") %in% names(tmpl("nested_plot"))))
  expect_true(all(c("parent_plot", "parent_relation") %in% names(tmpl("full"))))
  expect_false("parent_plot" %in% names(tmpl("permanent_plot")))
})

test_that("nested_plot examples respect the paired parent_plot/parent_relation rule", {
  x <- tmpl("nested_plot")
  expect_equal(is.na(x$parent_plot), is.na(x$parent_relation))
  expect_true(all(x$parent_relation[!is.na(x$parent_relation)] %in%
                    c("nested_subsample", "block_member")))
})

test_that("n_examples is honoured and templates work without examples", {
  expect_equal(nrow(tmpl("permanent_plot", n_examples = 1)), 1)
  expect_equal(nrow(tmpl("full", with_examples = FALSE)), 0)
})

test_that("example method/country come from the database context when given", {
  ex <- .get_template_examples("minimal", 2,
                               list(methods = c("m1", "m2"), countries = "c1"))
  expect_equal(ex$method, c("m1", "m2"))
  expect_equal(ex$country, c("c1", "c1"))
})
