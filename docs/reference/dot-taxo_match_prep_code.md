# Build the data-preparation code

Returns NULL in single-column mode: there is nothing to prepare, the
column is used as it stands. In multi-column mode it reproduces the
genus / epithet / family concatenation the app performs.

## Usage

``` r
.taxo_match_prep_code(cols)
```

## Arguments

- cols:

  The column-selection list returned by the column module
