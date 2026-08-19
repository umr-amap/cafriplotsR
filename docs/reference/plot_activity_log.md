# Plot logged database activity

Produces interactive plotly charts from a
[`summarize_activity_log`](https://umr-amap.github.io/cafriplotsR/reference/summarize_activity_log.md)
result:

1.  Active connections over time (line chart)

2.  Slots active per user (bar chart)

3.  Transactions per 30-min slot by database (line chart)

## Usage

``` r
plot_activity_log(summary, title = NULL)
```

## Arguments

- summary:

  A list returned by
  [`summarize_activity_log`](https://umr-amap.github.io/cafriplotsR/reference/summarize_activity_log.md).

- title:

  Optional character string added to each chart title.

## Value

A named list of three `plotly` objects: `connections_over_time`,
`slots_per_user`, `transactions_over_time`.

## Examples

``` r
if (FALSE) { # \dontrun{
log     <- read_activity_log("~/db_logs", from = Sys.Date() - 30)
summary <- summarize_activity_log(log)
charts  <- plot_activity_log(summary)
charts$connections_over_time
charts$slots_per_user
} # }
```
