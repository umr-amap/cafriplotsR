#' Append rows to a table without using COPY
#'
#' @description
#' Drop-in replacement for `DBI::dbWriteTable(..., append = TRUE)` and
#' `DBI::dbAppendTable()` that issues parametrised multi-row `INSERT`
#' statements instead.
#'
#' @details
#' RPostgres implements `dbWriteTable()` and `dbAppendTable()` with
#' `COPY ... FROM STDIN`, and PostgreSQL refuses `COPY FROM` on any table where
#' row-level security applies to the current user:
#'
#' \preformatted{
#'   ERROR:  COPY FROM not supported with row level security
#'   HINT:   Use INSERT statements instead.
#' }
#'
#' The table owner is exempt, which is why this has not bitten yet — almost
#' every write to this database is made by `dauby`, who owns all 38 tables. It
#' would bite the moment RLS reached the child tables: every account in
#' `plots_transects-rw` would stop being able to import, with that error. This
#' function is the "use INSERT statements instead".
#'
#' Rows are sent in chunks of one statement each rather than one statement per
#' row, because one round trip per row against a remote managed instance turns
#' a ten-thousand-row import into minutes of latency. Within a chunk the
#' placeholders are laid out column-major, so the parameter list can be built
#' with one `as.list()` per column instead of a per-cell loop.
#'
#' Columns are matched to the table by name, as `dbWriteTable()` does, so the
#' order of columns in `data` does not matter. Nothing is quoted by hand:
#' values travel as bound parameters, which also removes the injection surface
#' that string-built inserts carry.
#'
#' @section Transactions:
#' A caller that needs all the rows to arrive or none should open its own
#' transaction, as `mod_feat_step6_import.R` and `import_individual_data()`
#' already do. Within one chunk the `INSERT` is atomic by itself; across chunks
#' it is not, which is the one way this differs from a single `COPY`. Passing a
#' pool is safe — one connection is held for the whole call — but a pool cannot
#' join a transaction the caller opened on a different connection.
#'
#' @param con A DBI connection or a `pool` object.
#' @param table Character. Name of the target table.
#' @param data A data frame or tibble. Zero rows is a no-op.
#' @param chunk_size Integer or NULL. Rows per `INSERT`. When NULL (the
#'   default) it is derived from the column count so a chunk stays under
#'   PostgreSQL's limit of 65535 bound parameters per statement.
#'
#' @returns Invisibly, the number of rows inserted.
#'
#' @examples
#' \dontrun{
#' con <- call.mydb()
#' .db_append_table(con, "data_ind_measures_feat", feat_records)
#' }
#'
#' @seealso [DBI::dbAppendTable()], which this replaces at the call sites that
#'   write to row-level-security tables.
#' @keywords internal
#' @export
.db_append_table <- function(con, table, data, chunk_size = NULL) {

  stopifnot(
    "table must be a single name" = is.character(table) && length(table) == 1L,
    "data must be a data frame"   = is.data.frame(data)
  )

  data <- as.data.frame(data, stringsAsFactors = FALSE)

  n_rows <- nrow(data)
  n_cols <- ncol(data)

  if (n_rows == 0L) return(invisible(0L))
  if (n_cols == 0L) {
    cli::cli_abort("{.arg data} has no columns - nothing to insert into {.val {table}}")
  }

  # A factor would reach the server as its integer code, which is never what
  # the caller meant. dbWriteTable() converts them, so this must too.
  for (j in seq_len(n_cols)) {
    if (is.factor(data[[j]])) data[[j]] <- as.character(data[[j]])
  }

  # A pool hands out a connection per call, so a multi-chunk insert could be
  # spread over several backends - and a chunk that failed would leave the
  # earlier ones committed. COPY was one statement on one connection; hold one
  # connection here so the behaviour matches.
  if (inherits(con, "Pool")) {
    con <- pool::poolCheckout(con)
    on.exit(pool::poolReturn(con), add = TRUE)
  }

  quoted_table <- DBI::dbQuoteIdentifier(con, table)
  quoted_cols  <- paste(DBI::dbQuoteIdentifier(con, names(data)), collapse = ", ")

  # 65535 bound parameters per statement is a protocol limit, not a tunable.
  if (is.null(chunk_size)) {
    chunk_size <- max(1L, as.integer(floor(60000 / n_cols)))
  }
  chunk_size <- min(as.integer(chunk_size), n_rows)

  inserted <- 0L
  starts <- seq.int(1L, n_rows, by = chunk_size)

  for (start in starts) {
    end   <- min(start + chunk_size - 1L, n_rows)
    chunk <- data[start:end, , drop = FALSE]

    stmt <- .build_insert_chunk(quoted_table, quoted_cols, chunk)

    DBI::dbExecute(con, stmt$sql, params = stmt$params)
    inserted <- inserted + nrow(chunk)
  }

  invisible(inserted)
}


#' Build one multi-row INSERT and its bound parameters
#'
#' Split out of [.db_append_table()] so the placeholder layout can be tested
#' without a database connection. Takes identifiers already quoted, and so does
#' no quoting of its own.
#'
#' Placeholders are laid out **column-major**: row `i` of column `j` is
#' `$((j-1)*k + i)` for a chunk of `k` rows. That is what allows `params` to be
#' the columns concatenated — one `as.list()` per column — rather than a loop
#' over every cell, which matters when a chunk holds tens of thousands of
#' values.
#'
#' @param quoted_table Character(1). Already-quoted table identifier.
#' @param quoted_cols Character(1). Already-quoted, comma-separated column list,
#'   in the same order as the columns of `chunk`.
#' @param chunk A data frame of at least one row.
#' @returns A list with `sql` and `params`.
#' @keywords internal
.build_insert_chunk <- function(quoted_table, quoted_cols, chunk) {

  k <- nrow(chunk)
  n_cols <- ncol(chunk)

  per_col <- lapply(seq_len(n_cols), function(j) paste0("$", (j - 1L) * k + seq_len(k)))
  tuples  <- paste0("(", do.call(paste, c(per_col, list(sep = ", "))), ")")

  sql <- paste0("INSERT INTO ", quoted_table, " (", quoted_cols, ") VALUES ",
                paste(tuples, collapse = ", "))

  # unlist(recursive = FALSE) concatenates the per-column lists without
  # coercing the scalars inside them to a common type.
  params <- unlist(lapply(chunk, as.list), recursive = FALSE, use.names = FALSE)

  list(sql = sql, params = params)
}
