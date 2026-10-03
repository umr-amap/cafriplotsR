local_methods_db <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con), envir = env)
  DBI::dbExecute(con, "
    CREATE TABLE methodslist (
      id_method INTEGER PRIMARY KEY,
      method TEXT,
      description_method TEXT
    )")
  DBI::dbExecute(con, "INSERT INTO methodslist (method) VALUES ('1 ha plot')")
  con
}

test_that(".insert_method() inserts the method and returns its id", {
  con <- local_methods_db()

  id <- .insert_method(con, "small-tree quadrat", "Stems < 10 cm")

  row <- DBI::dbGetQuery(con, "SELECT * FROM methodslist WHERE id_method = $1",
                         params = list(id))
  expect_equal(nrow(row), 1)
  expect_equal(row$method, "small-tree quadrat")
  expect_equal(row$description_method, "Stems < 10 cm")
})

test_that(".insert_method() stores quotes verbatim", {
  con <- local_methods_db()

  id <- .insert_method(con, "Gentry's transect", "it's 0.1 ha")

  row <- DBI::dbGetQuery(con, "SELECT * FROM methodslist WHERE id_method = $1",
                         params = list(id))
  expect_equal(row$method, "Gentry's transect")
  expect_equal(row$description_method, "it's 0.1 ha")
})

test_that(".insert_method() trims the name and stores a blank description as NULL", {
  con <- local_methods_db()

  for (desc in list(NULL, "", "   ")) {
    DBI::dbExecute(con, "DELETE FROM methodslist WHERE method = 'new one'")
    id <- .insert_method(con, "  new one ", desc)
    row <- DBI::dbGetQuery(con, "SELECT * FROM methodslist WHERE id_method = $1",
                           params = list(id))
    expect_equal(row$method, "new one")
    expect_true(is.na(row$description_method))
  }
})

test_that(".insert_method() refuses an empty name", {
  con <- local_methods_db()

  expect_error(.insert_method(con, ""), "required")
  expect_error(.insert_method(con, "   "), "required")
  expect_error(.insert_method(con, NULL), "required")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM methodslist")$n, 1)
})

test_that(".insert_method() refuses a name that already exists", {
  con <- local_methods_db()

  expect_error(.insert_method(con, " 1 HA Plot "), "already exists")
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM methodslist")$n, 1)
})
