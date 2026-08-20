# Add the observed tag ranges to the quadrat review table

`firsttag` says where a quadrat's numbering was meant to start; this
adds where it actually ran, how many stems fell in it, and which raw
`plot_plot_name_old` spellings they carried — the three things needed to
judge whether the assignment is right.

## Usage

``` r
.summarise_small_tree_quadrats(
  quadrats,
  trees,
  plot_name_col = "plot_plot_name_old"
)
```

## Arguments

- quadrats:

  Quadrat units.

- trees:

  Assigned tree data frame.

- plot_name_col:

  Name of the raw plot column, used in the message only.

## Value

`quadrats` with n_stems, tag_min, tag_max and raw_plot_names.
