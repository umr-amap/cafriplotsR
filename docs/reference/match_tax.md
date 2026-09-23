# Query and standardize taxonomy

Query and standardize taxonomy for synonymies and add traits information
at species and genus levels

## Usage

``` r
match_tax(idtax, queried_tax = NULL, verbose = TRUE, backbone = "internal")
```

## Arguments

- idtax:

  vector of idtax_n to be search

- queried_tax:

  tibble, output of query_taxa

- verbose:

  logical whether results should be shown in viewer

- backbone:

  Character. Backbone whose names are used: `"internal"` (default) for
  `table_taxa`, or the code of a backbone registered in the taxa
  database (see
  [`list_backbones()`](https://umr-amap.github.io/cafriplotsR/reference/list_backbones.md)),
  such as `"wcvp"`.

## Value

tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
match_tax(idtax = c(3095, 219))
} # }
```
