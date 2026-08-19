# Initialise the activity log directory and CSV files

Creates `log_dir` if needed and writes header rows to
`connections_log.csv` and `stats_log.csv` when the files do not yet
exist.

## Usage

``` r
init_activity_log(log_dir)
```

## Arguments

- log_dir:

  Path to the directory that will hold the log files.

## Value

`log_dir` invisibly.
