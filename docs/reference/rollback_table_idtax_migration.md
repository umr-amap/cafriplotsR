# Rollback table_idtax Migration

Reverts the materialized view migration and restores table_idtax as a
regular table from the backup.

## Usage

``` r
rollback_table_idtax_migration(con)
```

## Arguments

- con:

  Database connection (must have admin privileges)

## Value

Logical, TRUE if rollback successful

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()  # Admin credentials
rollback_table_idtax_migration(con)
} # }
```
