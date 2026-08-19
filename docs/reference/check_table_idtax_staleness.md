# Check table_idtax Staleness

Checks how old the table_idtax materialized view is and whether it needs
refreshing based on a configurable threshold.

## Usage

``` r
check_table_idtax_staleness(con = NULL, warn_days = 90, silent = FALSE)
```

## Arguments

- con:

  Database connection. If NULL, calls call.mydb()

- warn_days:

  Integer, number of days after which to warn (default 90)

- silent:

  Logical, if TRUE suppresses messages (default FALSE)

## Value

List with elements: - is_stale: Logical, TRUE if older than warn_days -
days_old: Numeric, age in days - last_updated: POSIXct, timestamp of
last update - message: Character, status message

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
status <- check_table_idtax_staleness(con)
if (status$is_stale) {
  update_taxa_link_table(con)
}
} # }
```
