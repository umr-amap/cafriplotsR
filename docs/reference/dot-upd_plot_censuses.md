# Censuses of a plot

Censuses of a plot

## Usage

``` r
.upd_plot_censuses(id_plot, con)
```

## Arguments

- id_plot:

  Integer(1), \`data_liste_plots.id_liste_plots\`.

- con:

  A DBI connection.

## Value

A data frame with \`id_sub_plots\`, \`census\` (its number), \`year\`,
\`month\` and \`day\`, ordered by census number.
