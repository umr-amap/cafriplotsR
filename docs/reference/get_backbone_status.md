# Get a backbone's import status

Get a backbone's import status

## Usage

``` r
get_backbone_status(backbone, con_taxa = NULL, verbose = TRUE)
```

## Arguments

- backbone:

  Character. Backbone code.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- verbose:

  Logical. Print the status. Default `TRUE`.

## Value

Invisibly, a list with `version`, `import_date`, `record_count`,
`link_count`, `imported_by` and `source_version`; `NULL` when there is
no current import.

## Examples

``` r
if (FALSE) { # \dontrun{
get_backbone_status("wcvp")
} # }
```
