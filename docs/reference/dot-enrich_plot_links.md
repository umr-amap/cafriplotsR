# Add the parent and child summary columns to a plot table

\`parent_relation\` is rewritten rather than kept, so that the value in
the table always agrees with \`parent_plot_name\` beside it: a query
that selected its own columns may not have carried it at all.

## Usage

``` r
.enrich_plot_links(plots, edges)
```

## Arguments

- plots:

  A tibble of plots carrying \`id_liste_plots\`.

- edges:

  The result of \[.plot_link_edges()\].

## Value

\`plots\` with \`parent_plot_name\`, \`parent_relation\` and
\`n_child_plots\` added.
