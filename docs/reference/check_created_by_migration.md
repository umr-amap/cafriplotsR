# Check created_by Migration Status

Verifies whether the created_by migration has been applied.

## Usage

``` r
check_created_by_migration(con)
```

## Arguments

- con:

  Database connection

## Value

List with migration status details

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
check_created_by_migration(con)
} # }
```
