# How a child table reaches the plot that owns its rows

Each entry says which column a grant is compared against and, when the
table has no plot column of its own, which table to reach through.

\`hops\` is recorded because it is what the cost tracks:
\`inst/scripts/check_plot_access_cost.R\` found predicate time follows
the number of joins between a row and its plot, not the shape of the
predicate.

## Usage

``` r
.plot_scope_route(table_name)
```

## Arguments

- table_name:

  Character of length 1.

## Value

A list with \`hops\`, and either \`column\` (the table's own plot key)
or \`via\` (a list of \`table\`, \`on_local\`, \`on_remote\`,
\`remote_key\`). May also carry \`or_column\`, a second branch for rows
the join cannot reach.
