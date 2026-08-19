# Query Unmatched Specimens (Internal)

Find specimens that have taxonomic discrepancies or are not properly
linked.

## Usage

``` r
.query_unmatched_specimens(con = NULL)
```

## Arguments

- con:

  Database connection. If NULL, calls call.mydb()

## Value

List with problematic specimens

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
