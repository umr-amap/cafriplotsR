# Look a scientific name up in one backbone

A search for a person choosing a name in an interface, not the bulk
matcher of \[match_taxa_to_backbone()\]. It tries an exact,
case-insensitive match on the whole name first; for a binomial it adds
the infraspecific taxa of that species, so a variety can be picked; and
only if nothing was found does it fall back to the genus with a
four-letter prefix of the epithet.

Every row is annotated with `match_type`, `"exact"` or `"fuzzy"`, which
says how the row was reached, not how good it is.

## Usage

``` r
search_backbone_names(name, backbone, con_taxa = NULL)
```

## Arguments

- name:

  Character. Name to look up, e.g. `"Gilbertiodendron dewevrei"`.

- backbone:

  Character. Backbone code.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

## Value

A data frame with the backbone's canonical columns (`external_id`,
`accepted_external_id`, `taxon_name`, `family`, `genus`, `species`,
`infra_rank`, `infra_epithet`, `authors`, `rank`, `status`,
`status_raw`) plus `match_type`. Zero rows when nothing matches, `NULL`
when the backbone cannot be read.

## Examples

``` r
if (FALSE) { # \dontrun{
search_backbone_names("Gilbertiodendron dewevrei", "apd")
} # }
```
