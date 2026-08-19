# Census records already recorded for a set of plots

Census records already recorded for a set of plots

## Usage

``` r
.fetch_census_subplots(plot_ids, con)
```

## Arguments

- plot_ids:

  Integer vector of \`data_liste_plots.id_liste_plots\`.

- con:

  Database connection.

## Value

Data frame with \`id_sub_plots\`, \`id_table_liste_plots\`,
\`census_num\`, \`year\`, \`month\`, \`day\`.
