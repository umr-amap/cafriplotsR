# One row per feature: what the extracted table shows, and from how many records

Records carrying \`plot_name\` are summarised per plot, so a selection
of several plots yields one row per plot and feature.

## Usage

``` r
.upd_feature_summary(records)
```

## Arguments

- records:

  Feature records from one of the two resolvers.

## Value

A tibble with one row per feature (per plot, where plots are named).
