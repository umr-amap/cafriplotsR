# Get table_idtax Metadata

Returns metadata about the table_idtax materialized view including last
update time, record count, and who updated it.

## Usage

``` r
get_table_idtax_metadata(con = NULL)
```

## Arguments

- con:

  Database connection. If NULL, calls call.mydb()

## Value

Tibble with metadata, or NULL if not available

## Examples

``` r
if (FALSE) { # \dontrun{
get_table_idtax_metadata()
} # }
```
