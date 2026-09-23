# Find cycles in the plot parent hierarchy

Walks upward from every plot that has a parent, carrying the path
visited so far, and flags the step that lands on a plot already in that
path. The walk stops expanding a row once it is flagged, so the
recursion terminates even though the data does not.

## Usage

``` r
.find_plot_hierarchy_cycles(con, limit = 100, max_depth = 100)
```

## Arguments

- con:

  Raw database connection (not a pool)

- limit:

  Integer, maximum cycles reported

- max_depth:

  Integer, hard stop on chain length

## Value

Data frame with \`id_liste_plots\`, \`plot_name\`, \`depth\`,
\`cycle_path\`
