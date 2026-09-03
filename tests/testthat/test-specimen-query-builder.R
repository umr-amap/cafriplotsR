# Building the specimen query.
#
# `query_specimens()` turns its filter arguments into SQL conditions over
# `specimens` and runs them as one SELECT. These tests run that translation
# against an in-memory SQLite database of the same shape.

# ── fixture ──────────────────────────────────────────────────────────────────

# Four specimens from two collectors, numbered 10, 20, 30 and 40.
specimen_db <- function() {

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "specimens", data.frame(
    id_specimen = 1:4,
    id_colnam   = c(10L, 10L, 11L, 11L),
    colnbr      = c(10L, 20L, 30L, 40L),
    idtax_n     = c(100L, 101L, 100L, 102L),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "table_colnam", data.frame(
    id_table_colnam = c(10L, 11L),
    colnam          = c("Dauby", "Sonke"),
    surname         = c("Gilles", "Bonaventure"),
    family_name     = c("Dauby", "Sonke"),
    stringsAsFactors = FALSE
  ))

  con
}

selected_specimens <- function(query, con) {
  sort(as.integer(DBI::dbGetQuery(con, query)$id_specimen))
}

# ── one filter at a time ─────────────────────────────────────────────────────

test_that("an argument that was not given adds no condition", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  expect_length(CafriplotsR:::.specimen_condition_collector(con = con), 0)
  expect_length(CafriplotsR:::.specimen_condition_number(con = con), 0)
  expect_length(CafriplotsR:::.specimen_condition_taxonomy(con = con), 0)
})

test_that("a collector is resolved from name to id", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- CafriplotsR:::.specimen_condition_collector(collector = "Dauby", con = con)
  expect_match(cond, "id_colnam IN")
  expect_match(cond, "10", fixed = TRUE)

  query <- CafriplotsR:::.assemble_specimen_query(cond, con)
  expect_equal(selected_specimens(query, con), c(1L, 2L))
})

test_that("an id_colnam is used as given, without a lookup", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- CafriplotsR:::.specimen_condition_collector(id_colnam = 11L, con = con)
  query <- CafriplotsR:::.assemble_specimen_query(cond, con)
  expect_equal(selected_specimens(query, con), c(3L, 4L))
})

test_that("collection numbers are matched exactly or as a range", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  exact <- CafriplotsR:::.specimen_condition_number(number = c(10L, 30L), con = con)
  expect_equal(
    selected_specimens(CafriplotsR:::.assemble_specimen_query(exact, con), con),
    c(1L, 3L)
  )

  # The bounds are separate conditions, so together they are an interval.
  range <- CafriplotsR:::.specimen_condition_number(
    number_min = 20L, number_max = 30L, con = con
  )
  expect_length(range, 2)
  expect_equal(
    selected_specimens(CafriplotsR:::.assemble_specimen_query(range, con), con),
    c(2L, 3L)
  )
})

test_that("taxonomy filters on idtax_n, and says so for the names it cannot use", {
  # genus, species and family live in the taxa database; they have never been
  # applied to the specimens query and must not look as though they were.
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  cond <- CafriplotsR:::.specimen_condition_taxonomy(idtax_n = 100L, con = con)
  expect_equal(
    selected_specimens(CafriplotsR:::.assemble_specimen_query(cond, con), con),
    c(1L, 3L)
  )

  expect_message(
    ignored <- CafriplotsR:::.specimen_condition_taxonomy(genus = "Cola", con = con),
    "Not applied"
  )
  expect_length(ignored, 0)
})

# ── the whole query ──────────────────────────────────────────────────────────

test_that("no filter at all selects every specimen", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  query <- CafriplotsR:::.assemble_specimen_query(character(0), con)
  expect_false(grepl("WHERE", as.character(query), fixed = TRUE))
  expect_equal(selected_specimens(query, con), 1:4)
})

test_that(".specimen_filter_query() combines the filters with AND", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  query <- suppressMessages(CafriplotsR:::.specimen_filter_query(
    con = con, collector = "Dauby", number_min = 20L
  ))
  expect_equal(selected_specimens(query, con), 2L)
})

test_that("an unmatched collector selects no specimen, rather than all of them", {
  # A name that matches nobody must narrow the query to nothing. Dropping the
  # condition instead would return the whole table and read as success.
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  expect_message(
    cond <- CafriplotsR:::.specimen_condition_collector(collector = "Nobody", con = con),
    "No collectors found"
  )
  expect_equal(cond, "FALSE")

  query <- CafriplotsR:::.assemble_specimen_query(cond, con)
  expect_length(selected_specimens(query, con), 0)

  # And it stays unsatisfiable once combined with the other filters.
  query <- suppressMessages(CafriplotsR:::.specimen_filter_query(
    con = con, collector = "Nobody", number_min = 10L
  ))
  expect_length(selected_specimens(query, con), 0)
})

# ── fetching ─────────────────────────────────────────────────────────────────

test_that(".fetch_specimens_by_ids() returns the specimens with collector names", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  res <- suppressMessages(CafriplotsR:::.fetch_specimens_by_ids(c(1L, 3L), con))

  expect_equal(sort(res$id_specimen), c(1L, 3L))
  expect_equal(res$colnam[res$id_specimen == 1], "Dauby")
  expect_equal(res$colnam[res$id_specimen == 3], "Sonke")
})

test_that(".fetch_specimens_by_ids() asks nothing when there is nothing to ask", {
  con <- specimen_db()
  on.exit(DBI::dbDisconnect(con))

  expect_equal(nrow(CafriplotsR:::.fetch_specimens_by_ids(NULL, con)), 0)
  expect_equal(nrow(CafriplotsR:::.fetch_specimens_by_ids(integer(0), con)), 0)
})
