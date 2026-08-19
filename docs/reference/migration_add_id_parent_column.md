# Add id_parent Column to table_taxa

Adds the id_parent column with foreign key constraint and index. This is
Phase 1 of the hierarchy migration.

## Usage

``` r
migration_add_id_parent_column(con = NULL, dry_run = FALSE)
```

## Arguments

- con:

  Database connection to taxa database

- dry_run:

  If TRUE, only print SQL without executing

## Value

TRUE if successful
