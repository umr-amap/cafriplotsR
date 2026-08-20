# Normalise raw OpenForis small-tree tree columns

Applies the same renaming as the new-plot form — the two share their
measurement vocabulary — and adds `parent_plot_name` and a numeric
`tag`. `plot_name` is deliberately not set here: it is the quadrat the
stem belongs to, which only
[`.assign_small_tree_quadrats()`](https://umr-amap.github.io/cafriplotsR/reference/dot-assign_small_tree_quadrats.md)
can work out.

## Usage

``` r
.normalise_openforis_small_tree_trees(
  trees_raw,
  plot_name_col = "plot_plot_name_old",
  tag_col = "tag",
  plot_name_digits = 3L,
  plot_name_map = NULL
)
```

## Arguments

- trees_raw:

  Data frame read from `tree_list.xlsx`.

- plot_name_col:

  Column holding the parent plot name.

- tag_col:

  Column holding the tag.

- plot_name_digits, plot_name_map:

  Passed to
  [`.normalise_plot_name`](https://umr-amap.github.io/cafriplotsR/reference/dot-normalise_plot_name.md).

## Value

Data frame with normalised column names, plus `parent_plot_name`,
`plot_name_raw` and a numeric `tag`.
