# Populate id_parent for Existing Entries

Links each taxon to its parent level using tax_level column. This is
Phase 3 of the hierarchy migration.

## Usage

``` r
migration_link_hierarchy(
  con = NULL,
  dry_run = FALSE,
  batch_size = 1000,
  verbose = TRUE
)
```

## Arguments

- con:

  Database connection to taxa database

- dry_run:

  If TRUE, only count what would be updated

- batch_size:

  Number of records to update per batch

- verbose:

  If TRUE, show progress

## Value

Data frame with counts of linked entries
