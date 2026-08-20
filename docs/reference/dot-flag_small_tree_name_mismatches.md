# Report stems whose raw plot spelling disagrees with their tag range

Every stem carrying the same `plot_plot_name_old` value was entered as
one quadrat in the field, so they should all land in the same quadrat
once assigned by tag. Where a spelling straddles two quadrats, the rows
in the smaller share are the suspicious ones — either the tag or the
trailing underscore was mistyped.

## Usage

``` r
.flag_small_tree_name_mismatches(trees)
```

## Arguments

- trees:

  Assigned tree data frame (plot_name, plot_name_raw).

## Value

Data frame of the minority rows, or NULL when every spelling is
consistent.
