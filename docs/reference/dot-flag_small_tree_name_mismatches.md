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

  Assigned tree data frame (quadrat_unit, quadrat_label, plot_name_raw).

## Value

Data frame of the minority rows, with the quadrat their tag puts them in
(`quadrat_by_tag`), the one most stems of that spelling landed in
(`quadrat_by_name`) and `keep = "tag"`, or NULL when every spelling is
consistent.

## Details

The table is laid out to be edited and handed back as
`quadrat_decisions`: quadrats are named by their label, which is what a
person types, and `keep` says which side is used.
