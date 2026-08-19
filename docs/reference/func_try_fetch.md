# Safely execute a SQL query with automatic retry

This function attempts to execute a SQL query using
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
and [`DBI::dbFetch()`](https://dbi.r-dbi.org/reference/dbFetch.html),
with automatic retries in case of transient database failures such as
connection loss or query preparation errors.

## Usage

``` r
func_try_fetch(con, sql, max_attempts = 10, wait_seconds = 1, verbose = TRUE)
```

## Arguments

- con:

  A DBI connection object.

- sql:

  A SQL query string, typically created using
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html).

- max_attempts:

  Integer. Maximum number of attempts before giving up. Default is 10.

- wait_seconds:

  Numeric. Time in seconds to wait between retries. Default is 1.

- verbose:

  Logical. If `TRUE`, displays informative messages. Default is `TRUE`.

## Value

A tibble containing the query results, with unique column names.

## Details

This function is designed for read queries (e.g., `SELECT`) that return
results. For write queries (e.g., `UPDATE`, `INSERT`, `DELETE`), use a
variant that uses `dbExecute()` or `dbSendStatement()`.

If the database connection is lost, the function stops immediately. If
the query fails to prepare (e.g., due to a lock or temporary issue), the
function retries up to `max_attempts`.
