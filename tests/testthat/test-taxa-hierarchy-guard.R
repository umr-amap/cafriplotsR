# `table_taxa.id_parent` exists only on the taxa database (rainbio). The main
# database carries its own `table_taxa` without it, so a call.mydb() connection
# reaches a table of the right name and the wrong shape. These tests cover the
# guard that says so, instead of letting a raw PostgreSQL column error surface
# several queries into the check.

mock_taxa_con <- function(columns, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbListFields = function(conn, name, ...) {
      if (length(columns) == 0) stop("relation \"table_taxa\" does not exist")
      columns
    },
    .env = env
  )
  structure(list(), class = "mock_connection")
}


test_that("a connection without table_taxa is named as the wrong database", {
  con <- mock_taxa_con(character(0))

  expect_error(
    CafriplotsR:::.require_taxa_hierarchy(con, "check_hierarchy_consistency"),
    "call.mydb.taxa"
  )
})

test_that("the main database's table_taxa is diagnosed, not queried", {
  # Right name, no id_parent: this is what call.mydb() reaches.
  con <- mock_taxa_con(c("idtax_n", "tax_gen", "tax_fam", "tax_level"))

  err <- expect_error(
    CafriplotsR:::.require_taxa_hierarchy(con, "check_hierarchy_consistency"),
    "id_parent"
  )
  # Both explanations are offered, because the guard cannot tell them apart.
  expect_match(conditionMessage(err), "call.mydb.taxa")
  expect_match(conditionMessage(err), "taxa_hierarchy.R")
})

test_that("the taxa database passes the guard", {
  con <- mock_taxa_con(c("idtax_n", "id_parent", "tax_gen", "tax_level"))

  expect_true(CafriplotsR:::.require_taxa_hierarchy(con, "check_hierarchy_consistency"))
})

test_that("check_hierarchy_consistency refuses a main-database connection", {
  # The failure must come before the first hierarchy query, so dbGetQuery is
  # left unmocked: reaching it would error with something else entirely.
  con <- mock_taxa_con(c("idtax_n", "tax_gen", "tax_level"))

  expect_error(check_hierarchy_consistency(con = con), "id_parent")
})
