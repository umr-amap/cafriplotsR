# Replace or restore missing values in a data frame

Replaces NA values with -9999 for numeric columns and "-9999" for
character columns, or inversely restores NAs from those values if \`inv
= TRUE\`.

## Usage

``` r
replace_NA(df, inv = FALSE)
```

## Arguments

- df:

  A data frame or tibble.

- inv:

  Logical. If TRUE, replaces standard sentinel values back to NA.

## Value

A data frame with modified missing values.

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
