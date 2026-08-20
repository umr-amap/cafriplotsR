# Warn about recorded columns this pipeline does not carry forward

Several fields of the small-tree form are conditional on a plot-level
switch and arrive empty in the exports seen so far: `angle` and
`distance_to_next_stem` are collected only when the plot sets
`distance_stems = Yes`, `height_estimate` only when a tree height was
measured. None of them has a home in the output, which is harmless while
they are empty and a silent loss the day a team switches them on.

## Usage

``` r
.warn_unused_openforis_columns(trees)
```

## Arguments

- trees:

  Normalised tree data frame.

## Value

NULL, invisibly. Called for its messages.

## Details

`taxa_vernacular_name` is in the same position for a different reason:
the form records it, the individuals table has nowhere to put it.
