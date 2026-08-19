# Collect database activity metrics

Queries PostgreSQL system views to capture a snapshot of database
activity: active connections, cache hit rates, table access patterns,
and sizes.

## Usage

``` r
get_db_activity(con)
```

## Arguments

- con:

  A database connection from
  [`call.mydb`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.md).

## Value

A named list with elements:

- timestamp:

  POSIXct snapshot time

- active_connections:

  data.frame from pg_stat_activity

- db_stats:

  data.frame from pg_stat_database (cache, transactions)

- table_stats:

  data.frame from pg_stat_user_tables (top 20 by access)

- db_sizes:

  data.frame of database sizes

- table_sizes:

  data.frame of largest tables in current database

- locks:

  data.frame of ungranted locks (empty if none)

## Details

Restricted to database administrators: PostgreSQL superusers, members of
the predefined `pg_monitor` role (PostgreSQL \>= 10), or members of the
custom `db_manager` role. Regular users will receive an informative
error.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
activity <- get_db_activity(con)
print_db_activity(activity)
} # }
```
