# Summarise logged connection activity

Counts the number of 30-minute slots each user/application was seen
active, and computes delta activity metrics (transactions, inserts,
etc.) between consecutive stats snapshots.

## Usage

``` r
summarize_activity_log(log)
```

## Arguments

- log:

  A list returned by
  [`read_activity_log`](https://umr-amap.github.io/cafriplotsR/reference/read_activity_log.md).

## Value

A list with:

- by_user:

  data.frame — slots active, estimated minutes, and peak hour per user

- by_user_app:

  data.frame — same broken down by application

- hourly:

  data.frame — connection count per hour

- stats_delta:

  data.frame — per-snapshot delta activity metrics

## Examples

``` r
if (FALSE) { # \dontrun{
log     <- read_activity_log("~/db_logs", from = Sys.Date() - 30)
summary <- summarize_activity_log(log)
summary$by_user
} # }
```
