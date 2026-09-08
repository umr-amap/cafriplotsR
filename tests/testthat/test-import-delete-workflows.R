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


# --- Child plots -------------------------------------------------------------
# safe_delete_plot() must not let ON DELETE SET NULL orphan a child plot: the
# child would be left holding a parent_relation with no parent, which
# chk_plot_parent_relation_paired rejects, so the delete aborts on a constraint
# name rather than on a sentence anyone can read.

# Stands in for the main database during a child-plot test. `children` is what
# the step 2b query returns; `hierarchy` says whether the migration has run.
mock_delete_con <- function(children = data.frame(), hierarchy = TRUE,
                            env = parent.frame()) {
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    test_connection = function(con) TRUE,
    .env = env
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbListFields = function(conn, name, ...) {
      if (hierarchy) c("id_liste_plots", "plot_name", "id_parent_plot", "parent_relation")
      else c("id_liste_plots", "plot_name")
    },
    dbGetQuery = function(con, sql) {
      sql_chr <- gsub("\\s+", " ", as.character(sql))
      # Order matters: the child and descendant queries also mention
      # data_liste_plots, so they have to be matched before the plain lookup.
      if (grepl("WITH RECURSIVE descendants", sql_chr, fixed = TRUE)) {
        return(children)
      }
      if (grepl("JOIN data_liste_plots p", sql_chr, fixed = TRUE)) {
        return(children)
      }
      if (grepl("COUNT(*) as n FROM", sql_chr, fixed = TRUE)) {
        return(data.frame(n = 0L))
      }
      if (grepl("FROM data_liste_plots", sql_chr, fixed = TRUE)) {
        # Echo back whatever IDs were asked for, so a deletion set widened by
        # child_plots = "delete" resolves to real plots.
        ids <- as.integer(strsplit(
          sub(".*IN \\(([0-9, ]+)\\).*", "\\1", sql_chr), ","
        )[[1]])
        return(data.frame(id_liste_plots = ids, plot_name = paste0("P", ids)))
      }
      stop(sprintf("Unexpected query: %s", sql_chr))
    },
    .env = env
  )
  structure(list(), class = "mock_connection")
}

one_child <- data.frame(
  id_liste_plots   = 7L,
  plot_name        = "P1_regen",
  parent_relation  = "nested_subsample",
  id_parent_plot   = 1L,
  parent_plot_name = "P1",
  depth            = 1L,
  stringsAsFactors = FALSE
)

test_that("safe_delete_plot refuses to orphan a child plot by default", {
  con <- mock_delete_con(children = one_child)

  expect_error(
    safe_delete_plot(plot_ids = 1L, con = con, dry_run = TRUE, verbose = FALSE),
    "child plot"
  )
})

test_that("safe_delete_plot names the child it refuses to orphan", {
  con <- mock_delete_con(children = one_child)

  expect_error(
    safe_delete_plot(plot_ids = 1L, con = con, dry_run = TRUE, verbose = FALSE),
    "P1_regen"
  )
})

test_that("safe_delete_plot proceeds when told to detach the child", {
  con <- mock_delete_con(children = one_child)

  summary <- safe_delete_plot(
    plot_ids = 1L, con = con, dry_run = TRUE,
    child_plots = "detach", verbose = FALSE
  )

  expect_true(summary$dry_run)
  expect_equal(summary$counts$child_plots_outside, 1L)
  expect_equal(summary$child_plots, "detach")
})

test_that("safe_delete_plot pulls descendants into the set when told to delete them", {
  con <- mock_delete_con(children = one_child)

  summary <- safe_delete_plot(
    plot_ids = 1L, con = con, dry_run = TRUE,
    child_plots = "delete", verbose = FALSE
  )

  expect_setequal(summary$plot_ids, c(1L, 7L))
  # Once the child is in the deletion set it is no longer an outside child,
  # so nothing needs orphaning.
  expect_equal(summary$counts$child_plots_outside, 0L)
})

test_that("safe_delete_plot ignores child plots on an unmigrated database", {
  con <- mock_delete_con(children = one_child, hierarchy = FALSE)

  summary <- safe_delete_plot(plot_ids = 1L, con = con, dry_run = TRUE, verbose = FALSE)

  expect_true(summary$dry_run)
  expect_null(summary$counts$child_plots)
})

test_that("safe_delete_plot leaves child plots alone when the plot record survives", {
  con <- mock_delete_con(children = one_child)

  summary <- safe_delete_plot(
    plot_ids = 1L, con = con, dry_run = TRUE,
    delete_plot = FALSE, verbose = FALSE
  )

  expect_true(summary$dry_run)
  expect_null(summary$counts$child_plots)
})
