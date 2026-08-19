# Check citations migration status

Verifies whether the citations migration has been applied.

## Usage

``` r
check_citations_migration(con)
```

## Arguments

- con:

  Database connection to \`plots_transects\`

## Value

Invisible list with fields \`table_exists\`, \`fk_column_exists\`,
\`migration_complete\`

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
check_citations_migration(con)
} # }
```
