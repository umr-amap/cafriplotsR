# The most recent census of each plot

The pre-selection offered by the census box: one subplot id per plot,
the highest census number it has. Plots with no numbered census
contribute nothing.

## Usage

``` r
.latest_census_per_plot(censuses)
```

## Arguments

- censuses:

  Data frame with \`id_sub_plots\`, \`id_table_liste_plots\` and
  \`census_num\`.

## Value

Integer-ish vector of subplot ids, possibly empty.
