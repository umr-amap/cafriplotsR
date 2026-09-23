# Save WCVP Links to Database

Superseded by `save_backbone_links(matches, "wcvp", ...)`, which it
calls. Links are written to `taxa_backbone_link`; a taxon left with a
single WCVP link gets it marked preferred.

## Usage

``` r
save_wcvp_links(matches, con_taxa, replace = TRUE, verbose = TRUE)
```

## Arguments

- matches:

  Tibble of matches from
  [`match_taxa_to_wcvp()`](https://umr-amap.github.io/cafriplotsR/reference/match_taxa_to_wcvp.md),
  with `idtax_n`, `plant_name_id`, `match_type` and optionally
  `match_score`.

- con_taxa:

  Connection to the taxa database.

- replace:

  Logical. If TRUE, deletes the existing WCVP links of the affected
  `idtax_n` before inserting. Default TRUE.

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
