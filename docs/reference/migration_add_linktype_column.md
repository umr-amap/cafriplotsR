# Add id_linktype Column to data_link_specimens

Adds the id_linktype FK column and migrates existing type strings to FK
references.

## Usage

``` r
migration_add_linktype_column(con = NULL, dry_run = FALSE)
```

## Arguments

- con:

  Database connection

- dry_run:

  If TRUE, only print SQL without executing

## Value

TRUE if successful
