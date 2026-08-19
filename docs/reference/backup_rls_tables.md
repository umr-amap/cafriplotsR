# Export tables blocked by FORCE ROW LEVEL SECURITY via DBI

Exports one or more tables to CSV files using a DBI connection. Use this
alongside \`backup_database(exclude_table_data = ...)\` to cover tables
that pg_dump cannot dump due to \`FORCE ROW LEVEL SECURITY\` on managed
servers (e.g. OVH) where superuser access is unavailable.

## Usage

``` r
backup_rls_tables(
  tables,
  backup_dir,
  con = NULL,
  timestamp = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
)
```

## Arguments

- tables:

  Character vector of table names to export.

- backup_dir:

  Directory to write CSV files into.

- con:

  A DBI connection to the main database. If NULL, calls \`call.mydb()\`.

- timestamp:

  Character. Timestamp string to embed in filenames (defaults to current
  time, formatted \`YYYY-MM-DD_HH-MM-SS\`).

## Value

Named character vector of CSV file paths, one per table.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use together with backup_database() when FORCE RLS blocks pg_dump
backup_database(
  backup_dir          = "D:/my_backups",
  sslmode             = "require",
  exclude_table_data  = "taxa_traits_measures"
)
backup_rls_tables(
  tables     = "taxa_traits_measures",
  backup_dir = "D:/my_backups"
)
} # }
```
