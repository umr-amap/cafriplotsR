# Append a 30-minute activity snapshot to the log files

Calls
[`get_db_activity`](https://umr-amap.github.io/cafriplotsR/reference/get_db_activity.md)
and appends one row per active connection to `connections_log.csv` and
one row per database to `stats_log.csv`. Designed to be called on a
regular schedule (e.g. every 30 minutes via Windows Task Scheduler).

## Usage

``` r
log_db_snapshot(con, log_dir, verbose = TRUE)
```

## Arguments

- con:

  A database connection from
  [`call.mydb`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md).

- log_dir:

  Directory containing the log files. Will be initialised with
  [`init_activity_log`](https://umr-amap.github.io/cafriplotsR/reference/init_activity_log.md)
  if it does not exist yet.

- verbose:

  Logical. Print a one-line confirmation? Default `TRUE`.

## Value

The `db_activity` snapshot (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
log_db_snapshot(con, log_dir = "~/db_logs")
} # }
```
