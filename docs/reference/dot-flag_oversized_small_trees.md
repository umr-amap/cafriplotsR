# Report stems at or above the small-tree diameter threshold

The small-tree protocol covers stems below 10 cm; anything at or above
that belongs to the large-stem census of the parent plot and was
probably mis-entered. They are reported, not removed.

## Usage

``` r
.flag_oversized_small_trees(trees, dbh_max = 10)
```

## Arguments

- trees:

  Assigned tree data frame.

- dbh_max:

  Threshold in cm, or NULL to skip the check.

## Value

Data frame of the offending stems, or NULL.
