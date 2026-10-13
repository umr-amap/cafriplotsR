# update_records(): columns it may write beyond the import columns
#
# The Feature Wizard's multi-stem step writes stem_grouping through
# update_records(table_type = "individuals"). stem_grouping is not an import
# column, so it is routed as an update-only column; when it was missing from
# both lists the update aborted with "No updatable columns found in data".

routing <- function() {
  list(
    table = "data_individuals",
    id_column = "id_n",
    direct_columns = c("plot_name", "tag", "multi_tiges_id"),
    update_columns = "stem_grouping",
    feature_columns = character(0)
  )
}

test_that("update_records() accepts stem_grouping for individuals", {
  seen <- NULL
  local_mocked_bindings(
    get_column_routing = function(table_type, con) routing(),
    detect_direct_changes = function(data, columns, config, con, ...) {
      seen <<- columns
      data.frame()
    }
  )

  expect_no_error(suppressMessages(update_records(
    data.frame(id_n = 514641L, stem_grouping = 514640L),
    table_type = "individuals", execute = FALSE,
    con = structure(list(), class = "fake"), interactive = FALSE
  )))
  expect_equal(seen, "stem_grouping")
})

test_that("an update-only column is not offered to the import wizard", {
  # get_import_column_routing() builds on direct_columns, never update_columns
  expect_false("stem_grouping" %in% routing()$direct_columns)
  expect_false("stem_grouping" %in% get_table_columns("data_individuals", NULL))
})
