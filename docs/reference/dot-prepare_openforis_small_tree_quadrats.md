# Build the sampled-quadrat units from a raw OpenForis small-tree plot export

One row of `plot.xlsx` is one 20 x 20 m quadrat of a parent plot, and
becomes one plot in the output. The quadrat code is decoded to its grid
label (`9` to `"20_20"`) and appended to the normalised parent plot
name.

## Usage

``` r
.prepare_openforis_small_tree_quadrats(
  plots_raw,
  quadrat_codes = NULL,
  plot_name_digits = 3L,
  plot_name_map = NULL,
  quadrat_sep = "_"
)
```

## Arguments

- plots_raw:

  Data frame read from `plot.xlsx`.

- quadrat_codes:

  Parsed 20 x 20 m quadrat code list, or NULL to keep the raw codes.

- plot_name_digits, plot_name_map:

  Passed to
  [`.normalise_plot_name`](https://umr-amap.github.io/cafriplotsR/reference/dot-normalise_plot_name.md).

- quadrat_sep:

  Separator between parent plot name and quadrat label.

## Value

Data frame with one row per quadrat: row_id (the row of `plots_raw` it
came from), parent_plot_name, quadrat, quadrat_code, plot_name,
firsttag.
