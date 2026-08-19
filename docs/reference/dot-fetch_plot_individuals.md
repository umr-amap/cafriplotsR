# Fetch the individuals already recorded for a set of plots

Thin database layer behind \[split_census_table()\]. Kept separate so
the splitting logic stays testable without a connection.

## Usage

``` r
.fetch_plot_individuals(plot_names, con, with_status = TRUE)
```

## Arguments

- plot_names:

  Character vector of plot names.

- con:

  Database connection or pool.

- with_status:

  Logical. Also fetch each stem's most recent \`stem_status\`? Failure
  to do so is not fatal — \`last_status\` comes back as \`NA\`.

## Value

Data frame with \`plot_name\`, \`id_n\`, \`tag\`, \`idtax_n\`,
\`stem_grouping\` and \`last_status\`.
