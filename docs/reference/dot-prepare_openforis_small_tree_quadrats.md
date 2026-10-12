# Build the sampled-quadrat units from a raw OpenForis small-tree plot export

One row of `plot.xlsx` is one 20 x 20 m quadrat of a parent plot. The
quadrat code is decoded to its grid label (`9` to `"20_20"`), which,
appended to the normalised parent plot name, identifies the quadrat
(`quadrat_unit`). Every quadrat of a parent plot belongs to the same
small-tree plot, named after the parent plus `plot_suffix`.

## Usage

``` r
.prepare_openforis_small_tree_quadrats(
  plots_raw,
  quadrat_codes = NULL,
  plot_name_digits = 3L,
  plot_name_map = NULL,
  quadrat_sep = "_",
  plot_suffix = "_smalltree"
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

- plot_suffix:

  Appended to the parent plot name to name the small-tree plot.

## Value

Data frame with one row per quadrat: row_id (the row of `plots_raw` it
came from), parent_plot_name, plot_name, quadrat_unit, quadrat,
quadrat_code, firsttag.
