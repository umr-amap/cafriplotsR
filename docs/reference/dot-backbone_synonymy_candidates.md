# Internal taxa already linked to the same accepted name

Given one name in a backbone, finds the internal taxa linked to the
other names that share its accepted name. When a new taxon is added and
matched to that backbone name, these are the taxa it is likely to be a
synonym of.

A name that is itself accepted has no `accepted_external_id`; its own
identifier is used instead, so its synonyms are found too.

## Usage

``` r
.backbone_synonymy_candidates(external_id, backbone, con_taxa = NULL)
```

## Arguments

- external_id:

  Character or integer. Identifier of the name in the backbone.

- backbone:

  Character. Backbone code.

- con_taxa:

  Connection or pool to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

## Value

A data frame with `idtax_n`, `external_id`, `backbone_name`, `status`,
`status_raw`, `authors`, `match_type`, `tax_gen`, `tax_esp`, `tax_fam`,
`tax_rank01`, `tax_nam01` and `idtax_good_n`; zero rows when there are
none or when the backbone cannot be read.
