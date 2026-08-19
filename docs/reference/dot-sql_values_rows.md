# Build a SQL \`VALUES\` clause from a data frame

Column by column rather than row by row: \`apply()\` over a data frame
goes through \`as.matrix()\`, which formats numbers and can pad them
with spaces.

## Usage

``` r
.sql_values_rows(df)
```

## Arguments

- df:

  Data frame to render.

## Value

Single string, \`"(...), (...)"\`.
