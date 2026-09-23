# Valid plot parent relations

The closed vocabulary of \`data_liste_plots.parent_relation\`, mirroring
the \`chk_plot_parent_relation\` CHECK constraint added by
\`inst/migrations/plot_hierarchy.R\`. Kept here so the package can
report an unknown value without asking the database what it allows.

## Usage

``` r
.plot_parent_relations()
```

## Value

Character vector of allowed relations
