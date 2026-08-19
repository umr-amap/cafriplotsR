# Pivot the forest_type\[1..n\] columns into a long table

Feeds the collapsed `forest_type` column of the plot table built by
[`.prepare_openforis_new_plots()`](https://umr-amap.github.io/cafriplotsR/reference/dot-prepare_openforis_new_plots.md).

## Usage

``` r
.prepare_openforis_forest_types(plots_raw, code_list = NULL)
```

## Arguments

- plots_raw:

  Data frame read from `plot.xlsx`.

- code_list:

  Parsed forest type code list, or NULL.

## Value

Data frame with plot_name and forest_type, or NULL.
