# Every column the plot-scope predicates name

The first run of \`inst/scripts/check_plot_access_cost.R\` guessed a
column name that did not exist, and the failure took a whole transaction
with it. Nothing here is trusted against the catalog without being
checked first, so this lists what to check.

## Usage

``` r
.plot_scope_referenced_columns()
```

## Value

Data frame with \`table_name\` and \`column_name\`.
