# Update table_idtax (Materialized View Version)

Refreshes the table_idtax materialized view with latest synonym
information from the taxa database. This version works with the
materialized view approach and can be run by non-admin users with
appropriate permissions.

## Usage

``` r
update_taxa_link_table(
  con = NULL,
  con_taxa = NULL,
  force = FALSE,
  warn_days = 90
)
```

## Arguments

- con:

  Database connection to main DB. If NULL, calls call.mydb()

- con_taxa:

  Database connection to taxa DB. If NULL, calls call.mydb.taxa()

- force:

  Logical, if TRUE forces refresh even if recently updated (default
  FALSE)

- warn_days:

  Integer, age threshold in days (default 90)

## Value

List with elements: - success: Logical, TRUE if refresh succeeded -
method: Character, "materialized_view" or "legacy" - message: Character,
status message - record_count: Integer, number of records after
refresh - duration: Numeric, refresh duration in seconds (if available)

## Details

The function first tries to use the PostgreSQL refresh_table_idtax()
function (materialized view approach). If that fails, it falls back to
the legacy method of updating via dbWriteTable (requires admin
permissions).

## Author

Gilles Dauby, <gilles.dauby@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple refresh
update_taxa_link_table()

# Force refresh even if recently updated
update_taxa_link_table(force = TRUE)

# With explicit connections
con <- call.mydb()
result <- update_taxa_link_table(con = con)
} # }
```
