# Add Audit Columns to data_link_specimens

Adds created_by and created_at columns for audit trail.

## Usage

``` r
migration_add_audit_columns(con = NULL, dry_run = FALSE)
```

## Arguments

- con:

  Database connection

- dry_run:

  If TRUE, only print SQL without executing

## Value

TRUE if successful
