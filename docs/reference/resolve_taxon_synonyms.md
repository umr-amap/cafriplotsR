# Resolve taxonomic synonyms

Replaces taxon IDs with their accepted names based on synonym
resolution. Based on the logic from merge_individuals_taxa.

## Usage

``` r
resolve_taxon_synonyms(
  idtax = NULL,
  include_synonyms = TRUE,
  con_taxa = NULL,
  backbone = "internal"
)
```

## Arguments

- idtax:

  Vector of taxon IDs (can be synonyms)

- include_synonyms:

  If TRUE, also returns traits for all synonyms

- con_taxa:

  Connection to taxa database

- backbone:

  Character. Backbone used for synonym resolution: `"internal"`
  (default) for `table_taxa`, or the code of a backbone registered in
  the taxa database (see
  [`list_backbones()`](https://umr-amap.github.io/cafriplotsR/reference/list_backbones.md)),
  such as `"wcvp"`, falling back to internal for unlinked taxa.

## Value

Tibble with columns: idtax, idtax_good
