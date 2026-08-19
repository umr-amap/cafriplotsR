# Collect, print, and save a database activity report in one call

Convenience wrapper that calls
[`get_db_activity`](https://umr-amap.github.io/cafriplotsR/reference/get_db_activity.md),
[`print.db_activity`](https://umr-amap.github.io/cafriplotsR/reference/print.db_activity.md),
and
[`save_db_activity_report`](https://umr-amap.github.io/cafriplotsR/reference/save_db_activity_report.md).

## Usage

``` r
monitor_db(con, output_dir = ".", open = TRUE)
```

## Arguments

- con:

  A database connection from
  [`call.mydb`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md).

- output_dir:

  Directory for the HTML report (passed to
  [`save_db_activity_report`](https://umr-amap.github.io/cafriplotsR/reference/save_db_activity_report.md)).
  Set to `NULL` to skip saving.

- open:

  Logical. Open the HTML report in the browser? Default `TRUE`.

## Value

A `db_activity` object (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
monitor_db(con)

# Save to a specific folder without opening browser
monitor_db(con, output_dir = "~/db_reports", open = FALSE)
} # }
```
