# Read the activity log files into R

Read the activity log files into R

## Usage

``` r
read_activity_log(log_dir, from = NULL, to = NULL)
```

## Arguments

- log_dir:

  Directory containing `connections_log.csv` and `stats_log.csv`.

- from:

  Optional `Date` or `POSIXct`. Filter rows on or after this time.

- to:

  Optional `Date` or `POSIXct`. Filter rows up to and including this
  time.

## Value

A named list with elements `connections` and `stats`, both data.frames
with a `snapshot_time` POSIXct column.

## Examples

``` r
if (FALSE) { # \dontrun{
log <- read_activity_log("~/db_logs")
log <- read_activity_log("~/db_logs", from = Sys.Date() - 30)
} # }
```
