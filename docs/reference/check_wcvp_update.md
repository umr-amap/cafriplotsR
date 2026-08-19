# Check if WCVP Update is Available

Compares the database WCVP version with the version available in the
`rWCVP` package.

## Usage

``` r
check_wcvp_update(con_taxa = NULL)
```

## Arguments

- con_taxa:

  Connection to the taxa database. If NULL, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

## Value

Logical. TRUE if a newer version is available.

## Examples

``` r
if (FALSE) { # \dontrun{
if (check_wcvp_update()) {
  import_wcvp_names(con_taxa, force = TRUE)
}
} # }
```
