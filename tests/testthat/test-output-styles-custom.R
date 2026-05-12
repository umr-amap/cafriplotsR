# Tests for Phase 2 of output style customisation:
#   - output_style() constructor + validate_output_style()
#   - .resolve_output_style() / .validate_query_plots_output_style()
#   - end-to-end pass via .apply_output_style() with a custom object

# =============================================================================
# Constructor: argument validation
# =============================================================================

test_that("output_style() requires metadata_columns when based_on is missing", {
  expect_error(
    output_style(individuals_columns = c("id_n", "tag")),
    "metadata_columns"
  )
})

test_that("output_style() requires individuals_columns when based_on is missing", {
  expect_error(
    output_style(metadata_columns = c("plot_name")),
    "individuals_columns"
  )
})

test_that("output_style() returns a plot_output_style object with required fields", {
  s <- output_style(
    description         = "tiny",
    metadata_columns    = c("plot_name", "country"),
    individuals_columns = c("id_n", "tag")
  )
  expect_s3_class(s, "plot_output_style")
  expect_equal(s$metadata_columns, c("plot_name", "country"))
  expect_equal(s$individuals_columns, c("id_n", "tag"))
  expect_equal(s$description, "tiny")
  expect_identical(attr(s, "style_name"), "<custom>")
  expect_null(attr(s, "based_on"))
})

test_that("output_style() rejects non-list rename_columns", {
  expect_error(
    output_style(
      metadata_columns    = "plot_name",
      individuals_columns = "id_n",
      rename_columns      = c("ddlat" = "latitude")
    ),
    "rename_columns.*list"
  )
})

test_that("output_style() rejects unnamed character vector in rename_columns", {
  expect_error(
    output_style(
      metadata_columns    = "plot_name",
      individuals_columns = "id_n",
      rename_columns      = list(metadata = c("latitude", "longitude"))
    ),
    "named character vector"
  )
})

test_that("output_style() rejects non-logical flag", {
  expect_error(
    output_style(
      metadata_columns     = "plot_name",
      individuals_columns  = "id_n",
      keep_common_features = "yes"
    ),
    "TRUE or FALSE"
  )
})

test_that("output_style() warns on unknown additional_tables values", {
  expect_warning(
    output_style(
      metadata_columns    = "plot_name",
      individuals_columns = "id_n",
      additional_tables   = "not_a_real_table"
    ),
    "Unknown"
  )
})

# =============================================================================
# Constructor: based_on inheritance (replace semantics)
# =============================================================================

test_that("output_style(based_on = name) inherits all unspecified fields", {
  parent <- get_output_style("permanent_plot")
  child  <- output_style(based_on = "permanent_plot")

  # Strip identity attributes before comparing config bodies
  attr_drop <- function(x) {
    attributes(x)[c("style_name", "based_on")] <- NULL
    unclass(x)
  }
  expect_equal(attr_drop(child), attr_drop(parent))
  expect_identical(attr(child, "based_on"), "permanent_plot")
  expect_identical(attr(child, "style_name"), "<custom>")
})

test_that("output_style(based_on = ..., remove_patterns = X) replaces, not appends", {
  child <- output_style(
    based_on        = "permanent_plot",
    remove_patterns = c("^trait_")
  )
  expect_equal(child$remove_patterns, "^trait_")
  # Other parent fields are preserved
  expect_equal(
    child$metadata_columns,
    get_output_style("permanent_plot")$metadata_columns
  )
})

test_that("output_style(based_on = ..., remove_patterns = character()) clears the field", {
  child <- output_style(
    based_on        = "permanent_plot",
    remove_patterns = character()
  )
  expect_equal(child$remove_patterns, character())
})

test_that("output_style() accepts a plot_output_style object as based_on", {
  base  <- output_style(
    description         = "base",
    metadata_columns    = c("plot_name", "country"),
    individuals_columns = c("id_n", "tag")
  )
  child <- output_style(based_on = base, description = "child")
  expect_equal(child$description, "child")
  expect_equal(child$metadata_columns, c("plot_name", "country"))
})

test_that("output_style() rejects bad based_on argument", {
  expect_error(
    output_style(based_on = 1L),
    "based_on"
  )
  expect_error(
    output_style(based_on = c("a", "b")),
    "based_on"
  )
})

# =============================================================================
# validate_output_style()
# =============================================================================

test_that("validate_output_style() accepts an empty list", {
  expect_silent(validate_output_style(list()))
})

test_that("validate_output_style() rejects non-character metadata_columns", {
  expect_error(
    validate_output_style(list(metadata_columns = 1:3)),
    "metadata_columns"
  )
})

# =============================================================================
# .resolve_output_style()
# =============================================================================

test_that(".resolve_output_style() resolves a built-in name", {
  res <- .resolve_output_style("minimal")
  expect_equal(res$name, "minimal")
  expect_identical(res$config, .plot_output_styles$minimal)
})

test_that(".resolve_output_style() resolves a custom plot_output_style object", {
  obj <- output_style(
    metadata_columns    = "plot_name",
    individuals_columns = "id_n"
  )
  res <- .resolve_output_style(obj)
  expect_equal(res$name, "<custom>")
  expect_equal(res$config$metadata_columns, "plot_name")
})

test_that(".resolve_output_style() errors on unknown name", {
  expect_error(
    .resolve_output_style("nope"),
    "Unknown output style"
  )
})

test_that(".resolve_output_style() errors on non-character non-object input", {
  expect_error(.resolve_output_style(1L), "single style name")
  expect_error(.resolve_output_style(NULL), "single style name")
})

# =============================================================================
# .validate_query_plots_output_style()  (the validator query_plots uses)
# =============================================================================

test_that(".validate_query_plots_output_style() accepts 'auto'", {
  expect_identical(.validate_query_plots_output_style("auto"), "auto")
})

test_that(".validate_query_plots_output_style() accepts built-in names", {
  expect_identical(.validate_query_plots_output_style("minimal"), "minimal")
})

test_that(".validate_query_plots_output_style() errors on bogus string", {
  expect_error(
    .validate_query_plots_output_style("garbage"),
    "Got 'garbage'"
  )
})

test_that(".validate_query_plots_output_style() passes through plot_output_style objects", {
  obj <- output_style(
    metadata_columns    = "plot_name",
    individuals_columns = "id_n"
  )
  expect_identical(.validate_query_plots_output_style(obj), obj)
})

test_that(".validate_query_plots_output_style() promotes a raw list", {
  raw <- list(
    metadata_columns    = "plot_name",
    individuals_columns = "id_n"
  )
  out <- .validate_query_plots_output_style(raw)
  expect_s3_class(out, "plot_output_style")
  expect_equal(attr(out, "style_name"), "<custom>")
})

# =============================================================================
# End-to-end: pass a custom style to .apply_output_style()
# =============================================================================

test_that(".apply_output_style() accepts a plot_output_style object", {
  data <- make_plot_individual_data()
  my   <- output_style(
    description         = "id+species only",
    metadata_columns    = c("plot_name", "country", "id_liste_plots"),
    individuals_columns = c("id_n", "tag", "tax_fam", "tax_gen", "tax_sp_level")
  )
  result <- .apply_output_style(
    data                 = data,
    style                = my,
    extract_individuals  = TRUE,
    show_multiple_census = FALSE
  )
  expect_s3_class(result, "plot_query_list")
  expect_identical(attr(result, "style"), "<custom>")
  # Custom selection applied
  expect_setequal(
    names(result$individuals),
    c("id_n", "tag", "tax_fam", "tax_gen", "tax_sp_level", "plot_name", "stem_diameter")
  )
})

test_that(".apply_output_style() applied to a based_on style respects overrides", {
  data <- make_plot_individual_data()
  my   <- output_style(
    based_on        = "minimal",
    remove_patterns = character()      # clear inherited remove_patterns
  )
  result <- .apply_output_style(
    data                = data,
    style               = my,
    extract_individuals = TRUE
  )
  expect_s3_class(result, "plot_query_list")
})

test_that(".apply_output_style() does NOT auto-upgrade custom styles to multi-census variant", {
  data <- make_plot_individual_data()
  # Inherit from permanent_plot but drop additional_tables to avoid the
  # height_diameter pivot path (which expects census-suffixed columns when
  # show_multiple_census = TRUE -- not the case in this test fixture).
  my <- output_style(
    based_on          = "permanent_plot",
    additional_tables = character()
  )

  result <- .apply_output_style(
    data                 = data,
    style                = my,
    extract_individuals  = TRUE,
    show_multiple_census = TRUE
  )
  # The auto-upgrade from "permanent_plot" -> "permanent_plot_multi_census"
  # is a built-in name convenience and must not apply to custom objects.
  expect_identical(attr(result, "style"), "<custom>")
})
