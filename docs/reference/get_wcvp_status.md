# Get WCVP Import Status

Returns information about the current WCVP import in the database.

## Usage

``` r
get_wcvp_status(con_taxa = NULL)
```

## Arguments

- con_taxa:

  Connection to the taxa database. If NULL, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

## Value

A list with: `version`, `import_date`, `record_count`, `link_count`,
`imported_by`. Returns NULL if no import found.

## Examples

``` r
if (FALSE) { # \dontrun{
get_wcvp_status()
} # }
```
