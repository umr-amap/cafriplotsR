# Save WCVP Links to Database

Writes reviewed matches from
[`match_taxa_to_wcvp()`](https://umr-amap.github.io/cafriplotsR/reference/match_taxa_to_wcvp.md)
to the `wcvp_idtax_link` table.

## Usage

``` r
save_wcvp_links(matches, con_taxa, replace = TRUE, verbose = TRUE)
```

## Arguments

- matches:

  Tibble of matches from
  [`match_taxa_to_wcvp()`](https://umr-amap.github.io/cafriplotsR/reference/match_taxa_to_wcvp.md).

- con_taxa:

  Connection to the taxa database.

- replace:

  Logical. If TRUE, deletes existing links for affected `idtax_n` before
  inserting. Default TRUE.

- verbose:

  Logical. Show progress. Default TRUE.

## Value

Invisible integer: number of links saved.

## Examples

``` r
if (FALSE) { # \dontrun{
matches <- match_taxa_to_wcvp(con_taxa)
save_wcvp_links(matches, con_taxa)
} # }
```
