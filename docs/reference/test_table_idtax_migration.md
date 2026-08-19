# Test table_idtax Materialized View Setup

Tests whether the materialized view migration was successful by checking
all components are in place.

## Usage

``` r
test_table_idtax_migration(con)
```

## Arguments

- con:

  Database connection

## Value

List with test results

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
test_table_idtax_migration(con)
} # }
```
