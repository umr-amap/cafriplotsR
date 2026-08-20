# Plot feature records behind the columns of an extracted plot table

Each row of \`data_liste_sub_plots\` for the plot, annotated with how
many records feed the same extracted column (\`n_records\`) and what
that column would show (\`aggregate_display\`). When \`n_records \> 1\`
the extracted value is an aggregate and only these records can be
edited.

## Usage

``` r
.upd_plot_feature_records(id_plot, con)
```

## Arguments

- id_plot:

  Integer, \`data_liste_plots.id_liste_plots\`.

- con:

  A DBI connection.

## Value

A tibble, one row per subplot record; zero rows if the plot has none.
