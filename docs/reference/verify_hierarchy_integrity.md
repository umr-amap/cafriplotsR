# Verify Hierarchy Integrity

Checks the integrity of the id_parent hierarchy after migration. Uses
tax_level column to identify taxonomic levels.

## Usage

``` r
verify_hierarchy_integrity(con = NULL)
```

## Arguments

- con:

  Database connection to taxa database

## Value

Data frame with verification results
