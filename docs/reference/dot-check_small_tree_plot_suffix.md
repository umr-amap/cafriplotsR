# Refuse a small-tree plot suffix that cannot name a plot safely

The suffix must be non-empty, so the small-tree plot is never filed
under its parent's name, and must not end with a digit, which
[`.normalise_plot_name()`](https://umr-amap.github.io/cafriplotsR/reference/dot-normalise_plot_name.md)
would read as the plot number and pad.

## Usage

``` r
.check_small_tree_plot_suffix(plot_suffix)
```

## Arguments

- plot_suffix:

  Value of the `plot_suffix` argument.

## Value

`plot_suffix`, invisibly.
