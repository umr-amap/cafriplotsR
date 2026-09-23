# Descendants of a set of plots

Walks \`data_liste_plots.id_parent_plot\` downward from \`plot_ids\`,
returning every plot beneath them. Used by \[safe_delete_plot()\] to
expand a deletion set when \`child_plots = "delete"\`.

## Usage

``` r
.descendant_plot_ids(con, plot_ids, max_depth = 50L)
```

## Arguments

- con:

  Database connection or pool

- plot_ids:

  Integer vector of seed plot IDs

- max_depth:

  Integer, how far down to walk. Default 50.

## Value

Data frame with \`id_liste_plots\`, \`plot_name\`, \`parent_relation\`
and \`depth\` (1 = direct child). Empty when there are none.

## Details

Guarded against cycles by a depth limit rather than by a visited set:
the schema does not forbid A -\> B -\> A, and this must terminate on a
database where one exists. Use \[check_plot_hierarchy_consistency()\] to
find out whether one does.
