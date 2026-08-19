# Validate Taxonomy IDs (Internal)

Check that idtax_n values exist in taxa database.

## Usage

``` r
.validate_taxonomy_ids(data, con)
```

## Arguments

- data:

  Data frame with idtax_n column

- con:

  Database connection

## Value

List with errors and warnings
