# Create linktypelist Lookup Table

Creates the lookup table for specimen link types with priority values.
Higher priority = more authoritative link type.

## Usage

``` r
migration_create_linktypelist(con = NULL, dry_run = FALSE)
```

## Arguments

- con:

  Database connection (must have CREATE TABLE privileges)

- dry_run:

  If TRUE, only print SQL without executing

## Value

TRUE if successful
