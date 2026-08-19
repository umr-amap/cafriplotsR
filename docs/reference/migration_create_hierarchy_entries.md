# Create Missing Hierarchy Entries

Creates entries for genus, family, order, and class levels where they
don't already exist. This is Phase 2 of the hierarchy migration.

## Usage

``` r
migration_create_hierarchy_entries(con = NULL, dry_run = FALSE, verbose = TRUE)
```

## Arguments

- con:

  Database connection to taxa database

- dry_run:

  If TRUE, only count what would be created

- verbose:

  If TRUE, show progress

## Value

Data frame with counts of created entries

## Details

Uses the \`tax_level\` column to identify existing entries at each
level. Valid tax_level values: "class", "higher", "order", "family",
"genus", "species", "infraspecific"

NOTE: Classes are currently stored in a separate table_tax_famclass
table and linked via id_tax_famclass. This migration creates class-level
entries in table_taxa for each class, which will serve as the root of
the hierarchy.
