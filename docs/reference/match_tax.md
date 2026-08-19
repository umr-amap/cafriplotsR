# Query and standardize taxonomy

Query and standardize taxonomy for synonymies and add traits information
at species and genus levels

## Usage

``` r
match_tax(
  idtax,
  queried_tax = NULL,
  verbose = TRUE,
  backbone = c("internal", "wcvp")
)
```

## Arguments

- idtax:

  vector of idtax_n to be search

- queried_tax:

  tibble, output of query_taxa

- verbose:

  logical whether results should be shown in viewer

- backbone:

  Character. Which taxonomic backbone to use for synonym resolution.
  `"internal"` (default) uses the internal `table_taxa`. `"wcvp"` uses
  WCVP via `wcvp_idtax_link` and `wcvp_names`, falling back to internal
  for unlinked taxa.

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
