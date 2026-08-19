# Normalise raw OpenForis new-plot tree columns

The new-plot form and the recensus form use different names for the same
fields. This renames the new-plot columns to the recensus vocabulary so
the decoding helpers in `openforis_processing.R` can be reused
unchanged.

## Usage

``` r
.normalise_openforis_new_plot_trees(
  trees_raw,
  plot_name_col = "plot_plot_name",
  tag_col = "tag"
)
```

## Arguments

- trees_raw:

  Data frame read from `tree_list.xlsx`.

- plot_name_col:

  Column holding the plot name.

- tag_col:

  Column holding the tag.

## Value

Data frame with normalised column names, plus `plot_name` and a numeric
`tag`.
