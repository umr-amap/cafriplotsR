# The ancestor chain of a plot, the plot itself first

\`parent_relation\` on each row is how \*that\* plot sits inside its own
parent, so the rows read as a path: row 1 is \`parent_relation\` of row
2, and so on.

## Usage

``` r
.upd_plot_ancestors(id, con)
```

## Arguments

- id:

  Integer plot id.

- con:

  A DBI connection.

## Value

Data frame with \`id_liste_plots\`, \`plot_name\`, \`parent_relation\`,
\`depth\` (1 for the plot itself), ordered from the plot upwards.

## Details

Carries the visited-path array for the same reason
\[.descendant_plot_ids()\] does: a cycle would otherwise be walked until
the depth limit, and a cycle is exactly what an unchecked hierarchy may
hold.
