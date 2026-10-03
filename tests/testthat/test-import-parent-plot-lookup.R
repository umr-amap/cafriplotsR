local_plots_db <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  DBI::dbExecute(con, "CREATE TABLE table_colnam (id_table_colnam INTEGER, colnam TEXT)")
  DBI::dbExecute(con, "INSERT INTO table_colnam VALUES (7, 'mbalmayo001')")
  DBI::dbExecute(con, "CREATE TABLE data_liste_plots (id_liste_plots INTEGER, plot_name TEXT)")
  DBI::dbExecute(con, "
    INSERT INTO data_liste_plots VALUES
      (12, 'mbalmayo001'), (13, 'mbalmayo002'), (40, 'somalomo010')")
  con
}

parent_config <- list(metadata_mappings = list(
  parent_plot = list(id_col = "id_parent_plot", lookup_table = "data_liste_plots",
                     lookup_key = "id_liste_plots", lookup_value = "plot_name")
))

parent_lookup <- data.frame(id_liste_plots = c(12L, 13L, 40L),
                            plot_name = c("mbalmayo001", "mbalmayo002", "somalomo010"))


# =============================================================================
# .resolve_parent_plot_ids()
# =============================================================================

test_that("names and ids are both resolved, in the same column", {
  expect_equal(
    .resolve_parent_plot_ids(c("mbalmayo001", "13", " Somalomo010 ", 40), parent_lookup),
    c(12L, 13L, 40L, 40L)
  )
})

test_that("blank, unknown and non-id numbers resolve to NA", {
  expect_equal(
    .resolve_parent_plot_ids(c(NA, "", "nowhere", "999", "12.5"), parent_lookup),
    rep(NA_integer_, 5)
  )
})

test_that("nothing to look up in resolves to NA", {
  expect_equal(.resolve_parent_plot_ids("mbalmayo001", parent_lookup[0, ]), NA_integer_)
  expect_equal(.resolve_parent_plot_ids(character(0), parent_lookup), integer(0))
})


# =============================================================================
# Step 4: lookup matching
# =============================================================================

test_that("parent_plot is looked up among the plots, never created", {
  con <- local_plots_db()
  info <- suppressMessages(.get_lookup_info("parent_plot", con))

  expect_equal(info$table, "data_liste_plots")
  expect_false(info$allow_add_new)
  expect_setequal(info$data$plot_name, parent_lookup$plot_name)
})

test_that("an existing parent name is an exact match, a typo needs matching", {
  con <- local_plots_db()
  data <- data.frame(plot_name = c("a_smalltree", "b_smalltree"),
                     parent_plot_name = c("Mbalmayo001", "mbalmayo02"))

  res <- suppressMessages(.analyze_lookup_columns(
    data, list(parent_plot_name = "parent_plot"), con, parent_config
  ))

  expect_equal(res$exact_matches$parent_plot, "Mbalmayo001")
  expect_equal(res$non_exact_matches$parent_plot, "mbalmayo02")
})

test_that("a misspelled parent is offered the closest plot first", {
  con <- local_plots_db()
  info <- suppressMessages(.get_lookup_info("parent_plot", con))

  sugg <- suppressMessages(.get_fuzzy_matches("mbalmayo02", info))
  expect_true(sugg$value[1] %in% c("mbalmayo002", "mbalmayo001"))
  expect_true(all(sugg$id %in% parent_lookup$id_liste_plots))
})

test_that("exact parent names become ids, unmatched ones are left as typed", {
  con <- local_plots_db()
  data <- data.frame(plot_name = c("a_smalltree", "b_smalltree"),
                     parent_plot_name = c("Mbalmayo001", "mbalmayo02"))

  out <- suppressMessages(.convert_and_clean_lookup_values(
    data, list(parent_plot = "Mbalmayo001"),
    list(parent_plot_name = "parent_plot"), con
  ))

  # Matched on data_liste_plots (12), not on the people table (7)
  expect_equal(out$parent_plot_name, c("12", "mbalmayo02"))
})


# =============================================================================
# Validation and import after step 4
# =============================================================================

test_that("validation accepts ids and names mixed, and names an unknown parent", {
  con <- local_plots_db()
  data <- data.frame(
    plot_name = c("a_smalltree", "b_smalltree", "c_smalltree"),
    parent_plot = c("12", "mbalmayo002", "nowhere"),
    parent_relation = "nested_subsample"
  )

  res <- suppressMessages(.validate_lookup_values(data, list(), con))
  parent_errors <- Filter(function(e) e$column == "parent_plot", res$errors)

  expect_length(parent_errors, 1)
  expect_equal(parent_errors[[1]]$row, 3)
  expect_match(parent_errors[[1]]$message, "nowhere")
})

test_that("the import links ids and names mixed in one column", {
  con <- local_plots_db()
  testthat::local_mocked_bindings(.has_plot_hierarchy = function(con) TRUE)
  data <- data.frame(
    plot_name = c("a_smalltree", "b_smalltree", "c"),
    parent_plot = c("12", "mbalmayo002", NA),
    parent_relation = c("nested_subsample", "nested_subsample", NA)
  )

  out <- .link_parent_plot_for_import(data, con, interactive = FALSE,
                                      dry_run = TRUE, progress = FALSE)
  expect_equal(out$id_parent_plot, c(12L, 13L, NA))
})
