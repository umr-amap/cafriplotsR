# .db_append_table() replaces dbWriteTable(append = TRUE) at the call sites
# that write to row-level-security tables, because RPostgres implements those
# with COPY and PostgreSQL refuses COPY FROM under RLS.
#
# The part worth testing without a database is the placeholder layout: the
# parameters are sent column-major, so a mistake in the index arithmetic would
# silently write every value into the wrong column.

test_that(".build_insert_chunk() maps each placeholder to the right value", {
  chunk <- data.frame(
    a = c(1L, 2L, 3L),
    b = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  out <- .build_insert_chunk("\"t\"", "\"a\", \"b\"", chunk)

  # 3 rows, 2 columns: column a is $1..$3, column b is $4..$6.
  expect_equal(
    out$sql,
    "INSERT INTO \"t\" (\"a\", \"b\") VALUES ($1, $4), ($2, $5), ($3, $6)"
  )

  # params are column-major, so $1..$3 are a and $4..$6 are b
  expect_equal(out$params, list(1L, 2L, 3L, "x", "y", "z"))

  # and the two agree: resolving each placeholder by position reconstructs the
  # original row.
  placeholders <- regmatches(out$sql, gregexpr("\\$[0-9]+", out$sql))[[1]]
  idx <- as.integer(sub("\\$", "", placeholders))
  resolved <- out$params[idx]
  expect_equal(resolved, list(1L, "x", 2L, "y", 3L, "z"))
})

test_that(".build_insert_chunk() keeps each column's type", {
  chunk <- data.frame(
    i = 1L,
    d = 2.5,
    s = "text",
    l = TRUE,
    stringsAsFactors = FALSE
  )

  params <- .build_insert_chunk("\"t\"", "\"i\", \"d\", \"s\", \"l\"", chunk)$params

  expect_type(params[[1]], "integer")
  expect_type(params[[2]], "double")
  expect_type(params[[3]], "character")
  expect_type(params[[4]], "logical")
})

test_that(".build_insert_chunk() passes NA through as a typed NA, not a string", {
  chunk <- data.frame(
    i = c(1L, NA_integer_),
    s = c(NA_character_, "b"),
    stringsAsFactors = FALSE
  )

  params <- .build_insert_chunk("\"t\"", "\"i\", \"s\"", chunk)$params

  expect_true(is.na(params[[2]]))
  expect_type(params[[2]], "integer")
  expect_true(is.na(params[[3]]))
  expect_type(params[[3]], "character")
})

test_that(".build_insert_chunk() handles a single row and a single column", {
  one <- .build_insert_chunk("\"t\"", "\"a\"", data.frame(a = 7L))
  expect_equal(one$sql, "INSERT INTO \"t\" (\"a\") VALUES ($1)")
  expect_equal(one$params, list(7L))
})

test_that(".db_append_table() is a no-op on zero rows and never touches the connection", {
  # A connection that fails on any use: proves nothing is sent.
  con <- structure(list(), class = "explodes")
  expect_equal(.db_append_table(con, "t", data.frame(a = integer(0))), 0L)
})

test_that(".db_append_table() rejects input that cannot be inserted", {
  con <- structure(list(), class = "explodes")

  # Rows but no columns. An empty 0x0 frame is a no-op instead, tested above:
  # nothing to insert is not an error.
  expect_error(.db_append_table(con, "t", data.frame(row.names = 1:3)), "no columns")
  expect_error(.db_append_table(con, c("a", "b"), data.frame(a = 1)), "single name")
  expect_error(.db_append_table(con, "t", list(a = 1)), "data frame")
})

test_that(".db_append_table() chunks so no row is sent twice or dropped", {
  # Capture what would be executed, rather than reaching a database.
  sent <- list()
  local_mocked_bindings(
    dbQuoteIdentifier = function(conn, x, ...) paste0("\"", x, "\""),
    dbExecute = function(conn, statement, params = NULL, ...) {
      sent[[length(sent) + 1]] <<- params
      length(params)
    },
    .package = "DBI"
  )

  data <- data.frame(a = 1:10, b = letters[1:10], stringsAsFactors = FALSE)
  n <- .db_append_table(structure(list(), class = "fake"), "t", data,
                        chunk_size = 3)

  expect_equal(n, 10L)
  expect_length(sent, 4)                       # 3 + 3 + 3 + 1
  expect_equal(lengths(sent), c(6L, 6L, 6L, 2L))  # 2 columns per row

  # Every original value arrives exactly once.
  all_params <- unlist(sent, use.names = FALSE)
  expect_setequal(all_params[seq_len(20)], c(as.character(1:10), letters[1:10]))
})

test_that(".db_append_table() derives a chunk size under the 65535 parameter limit", {
  sizes <- list()
  local_mocked_bindings(
    dbQuoteIdentifier = function(conn, x, ...) paste0("\"", x, "\""),
    dbExecute = function(conn, statement, params = NULL, ...) {
      sizes[[length(sizes) + 1]] <<- length(params)
      length(params)
    },
    .package = "DBI"
  )

  # 40 columns: a naive "all rows in one statement" would need 40 * 2000
  # parameters, well past what PostgreSQL accepts.
  wide <- as.data.frame(matrix(1L, nrow = 2000, ncol = 40))
  .db_append_table(structure(list(), class = "fake"), "t", wide)

  expect_true(all(unlist(sizes) <= 65535))
})

test_that(".db_append_table() converts factors, as dbWriteTable does", {
  captured <- NULL
  local_mocked_bindings(
    dbQuoteIdentifier = function(conn, x, ...) paste0("\"", x, "\""),
    dbExecute = function(conn, statement, params = NULL, ...) {
      captured <<- params
      length(params)
    },
    .package = "DBI"
  )

  data <- data.frame(f = factor(c("beta", "alpha")), stringsAsFactors = FALSE)
  .db_append_table(structure(list(), class = "fake"), "t", data)

  # The labels, not the integer codes 2 and 1.
  expect_equal(captured, list("beta", "alpha"))
})
