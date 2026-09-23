# Save links between internal taxa and a backbone

Writes matches to `taxa_backbone_link`. An existing link (same taxon,
backbone and external ID) has its match details updated; it stays
verified if it was.

Rows whose `decision` is `"rejected"` (see
\[review_backbone_matches()\]) are not saved.

A taxon without a preferred link gets one when a single link can supply
its names: its only verified link, or, when none is verified, its only
link if that link is an exact or manual match. Fuzzy and author-mismatch
links supply names only once verified.

To rebuild all of a backbone's links after a new matching run, use
\[replace_backbone_links()\], which compares first.

## Usage

``` r
save_backbone_links(
  matches,
  backbone,
  con_taxa = NULL,
  replace = TRUE,
  verbose = TRUE
)
```

## Arguments

- matches:

  Data frame with `idtax_n`, `external_id`, `match_type` and optionally
  `match_score`, `verified` and `decision`, e.g. from
  \[match_taxa_to_backbone()\] or \[review_backbone_matches()\].

- backbone:

  Character. Backbone code.

- con_taxa:

  Connection or pool to the taxa database, with write access.

- replace:

  Logical. If `TRUE` (default), the backbone's existing links of the
  taxa in `matches` are deleted first.

- verbose:

  Logical. Show progress. Default `TRUE`.

## Value

Invisible integer: number of links written.

## Examples

``` r
if (FALSE) { # \dontrun{
matches <- match_taxa_to_backbone("wcvp", con_taxa, tax_ids = c(101, 102))
save_backbone_links(matches, "wcvp", con_taxa)
} # }
```
