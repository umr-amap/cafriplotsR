# Flag estimated (as opposed to measured) diameters

The OpenForis `dbh_measurement` field codes 1 = measured, 2 = estimated.
Only estimates are informative, so a single `pom_observation` row is
emitted for them.

## Usage

``` r
.flag_estimated_diameters(data)
```

## Arguments

- data:

  Normalised tree data frame.

## Value

Long-format data frame, or NULL when nothing was estimated.
