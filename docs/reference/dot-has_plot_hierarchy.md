# Is the Plot Hierarchy Available?

TRUE when \`inst/migrations/plot_hierarchy.R\` has been applied, i.e.
when \`data_liste_plots\` carries both \`id_parent_plot\` and
\`parent_relation\`.

## Usage

``` r
.has_plot_hierarchy(con)
```

## Arguments

- con:

  Database connection or pool

## Value

Logical, FALSE on any error

## Details

The package works either way. Every caller uses this to decide whether
to offer the parent-plot column, so a database that has not been
migrated simply never sees it rather than failing on an unknown column.
