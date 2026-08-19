# Run Full Specimen Links Migration

Runs all three phases of the specimen links migration: 1. Create
linktypelist lookup table 2. Add id_linktype column and migrate data 3.
Add audit columns

## Usage

``` r
run_specimen_links_migration(con = NULL, dry_run = TRUE)
```

## Arguments

- con:

  Database connection

- dry_run:

  If TRUE, only show what would happen

## Value

List with results from each phase
