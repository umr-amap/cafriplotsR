# Build the collecting team of each plot

Merges the `team_leader` and `additional_people` values decoded from
`plot.xlsx` into a single comma-separated list of people, dropping
anyone named in both. Fed to the `additional_collector` column of the
specimen table.

## Usage

``` r
.build_openforis_collector_team(plots)
```

## Arguments

- plots:

  Plot table produced by
  [`.prepare_openforis_new_plots()`](https://umr-amap.github.io/cafriplotsR/reference/dot-prepare_openforis_new_plots.md).

## Value

Data frame with columns `plot_name` and `additional_collector`, or NULL
when neither people column exists.
