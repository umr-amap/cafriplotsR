# check_plot_hierarchy_consistency() runs a fixed sequence of queries against
# data_liste_plots. These tests stand a mock in for the database and dispatch on
# a distinctive fragment of each query, so the reporting, the classification of
# issues into fixable and not, and the return contract are all exercised without
# a connection.

# Every result the function asks for, in the order it asks. Anything not named
# comes back empty.
mock_hierarchy_con <- function(...,
                               hierarchy = TRUE,
                               n_linked = 0L,
                               env = parent.frame()) {
  results <- list(...)

  pick <- function(name) {
    if (!is.null(results[[name]])) results[[name]] else data.frame()
  }

  testthat::local_mocked_bindings(
    .package = "DBI",
    dbListFields = function(conn, name, ...) {
      if (hierarchy) {
        c("id_liste_plots", "plot_name", "id_parent_plot", "parent_relation")
      } else {
        c("id_liste_plots", "plot_name")
      }
    },
    dbGetQuery = function(con, sql, ...) {
      sql_chr <- gsub("\\s+", " ", as.character(sql))
      if (grepl("WITH RECURSIVE walk", sql_chr, fixed = TRUE))          return(pick("cycles"))
      if (grepl("WITH RECURSIVE anc", sql_chr, fixed = TRUE))           return(data.frame(max_depth = 1L))
      if (grepl("GROUP BY parent_relation", sql_chr, fixed = TRUE)) {
        return(data.frame(parent_relation = "nested_subsample", n = n_linked))
      }
      if (grepl("id_parent_plot = id_liste_plots", sql_chr, fixed = TRUE)) return(pick("self_parent"))
      if (grepl("p.id_liste_plots IS NULL", sql_chr, fixed = TRUE))       return(pick("dangling_parent"))
      if (grepl("parent_relation NOT IN", sql_chr, fixed = TRUE))         return(pick("unknown_relation"))
      if (grepl("c.parent_relation IS NULL", sql_chr, fixed = TRUE))      return(pick("parent_without_relation"))
      if (grepl("parent_relation IS NOT NULL", sql_chr, fixed = TRUE) &&
          grepl("id_parent_plot IS NULL", sql_chr, fixed = TRUE))         return(pick("relation_without_parent"))
      if (grepl("COUNT(*) AS n", sql_chr, fixed = TRUE))                  return(data.frame(n = n_linked))
      stop(sprintf("Unexpected query: %s", sql_chr))
    },
    .env = env
  )

  structure(list(), class = "mock_connection")
}

a_plot <- function(relation = "nested_subsample") {
  data.frame(
    id_liste_plots  = 7L,
    plot_name       = "P1_regen",
    id_parent_plot  = 1L,
    parent_relation = relation,
    stringsAsFactors = FALSE
  )
}


test_that("check_plot_hierarchy_consistency says nothing to check on an unmigrated database", {
  con <- mock_hierarchy_con(hierarchy = FALSE)

  expect_null(check_plot_hierarchy_consistency(con = con))
})

test_that("check_plot_hierarchy_consistency returns NULL on a clean hierarchy", {
  con <- mock_hierarchy_con(n_linked = 3L)

  expect_null(check_plot_hierarchy_consistency(con = con))
})

test_that("check_plot_hierarchy_consistency returns NULL on an empty hierarchy", {
  con <- mock_hierarchy_con(n_linked = 0L)

  expect_null(check_plot_hierarchy_consistency(con = con))
})

test_that("check_plot_hierarchy_consistency reports a cycle", {
  cycle <- data.frame(
    id_liste_plots = 7L,
    plot_name      = "P1_regen",
    depth          = 2L,
    cycle_path     = "P1_regen -> P1 -> P1_regen",
    stringsAsFactors = FALSE
  )
  con <- mock_hierarchy_con(n_linked = 2L, cycles = cycle)

  issues <- check_plot_hierarchy_consistency(con = con)

  expect_named(issues, "cycles")
  expect_equal(nrow(issues$cycles), 1L)
})

test_that("a parent without a relation is reported but never auto-fixed", {
  con <- mock_hierarchy_con(n_linked = 1L, parent_without_relation = a_plot(NA_character_))

  issues <- check_plot_hierarchy_consistency(con = con)

  expect_true("parent_without_relation" %in% names(issues))
  expect_false("parent_without_relation" %in% .plot_hierarchy_fixable_types())
})

test_that("a relation without a parent is reported as fixable", {
  con <- mock_hierarchy_con(n_linked = 0L, relation_without_parent = a_plot())

  issues <- check_plot_hierarchy_consistency(con = con)

  expect_true("relation_without_parent" %in% names(issues))
  expect_true("relation_without_parent" %in% .plot_hierarchy_fixable_types())
})

test_that("an unknown relation is reported but never auto-fixed", {
  con <- mock_hierarchy_con(n_linked = 1L, unknown_relation = a_plot("sub_placette"))

  issues <- check_plot_hierarchy_consistency(con = con)

  expect_true("unknown_relation" %in% names(issues))
  expect_false("unknown_relation" %in% .plot_hierarchy_fixable_types())
})

test_that("fix = TRUE writes nothing when the only issues need a human", {
  con <- mock_hierarchy_con(n_linked = 1L, parent_without_relation = a_plot(NA_character_))

  # dbBegin/dbExecute are deliberately not mocked: reaching them would error.
  issues <- check_plot_hierarchy_consistency(con = con, fix = TRUE, force = TRUE)

  expect_true("parent_without_relation" %in% names(issues))
})

test_that("repairing a dangling parent clears the relation too", {
  # Clearing id_parent_plot alone would leave the row holding a relation with
  # no parent, which is exactly what chk_plot_parent_relation_paired rejects.
  executed <- character()

  con <- mock_hierarchy_con(n_linked = 1L, dangling_parent = a_plot())
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbBegin   = function(conn, ...) TRUE,
    dbCommit  = function(conn, ...) TRUE,
    dbExecute = function(conn, statement, ...) {
      executed <<- c(executed, gsub("\\s+", " ", statement))
      1L
    }
  )

  check_plot_hierarchy_consistency(con = con, fix = TRUE, force = TRUE)

  expect_length(executed, 1L)
  expect_match(executed, "id_parent_plot = NULL")
  expect_match(executed, "parent_relation = NULL")
})

test_that("the relation vocabulary matches the CHECK constraint", {
  expect_setequal(.plot_parent_relations(), c("nested_subsample", "block_member"))
})
