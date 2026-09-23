# Resolve synonyms through a backbone

For each internal taxon with a preferred link, follows the backbone's
synonym chain and maps the accepted name back to an internal taxon
linked to it (an internally accepted one first). Taxa without a link, or
whose accepted name is linked to no internal taxon, fall back to the
internal backbone.

## Usage

``` r
.resolve_synonyms_backbone(
  idtax,
  include_synonyms,
  con_taxa,
  backbone,
  max_depth = 5L
)
```

## Arguments

- idtax:

  Vector of taxon IDs, or \`NULL\` for all.

- include_synonyms:

  Logical.

- con_taxa:

  Connection to the taxa database.

- backbone:

  Backbone code.

- max_depth:

  Maximum number of synonym steps followed.

## Value

Tibble with columns \`idtax\`, \`idtax_good\`.
