# Resolve taxonomic synonyms

Replaces taxon IDs with their accepted names based on synonym
resolution. Based on the logic from merge_individuals_taxa.

## Usage

``` r
resolve_taxon_synonyms(
  idtax = NULL,
  include_synonyms = TRUE,
  con_taxa = NULL,
  backbone = c("internal", "wcvp")
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

  Character. Which taxonomic backbone to use for synonym resolution.
  `"internal"` (default) uses the internal `table_taxa`. `"wcvp"` uses
  WCVP via `wcvp_idtax_link` and `wcvp_names`, falling back to internal
  for unlinked taxa.

## Value

Tibble with columns: idtax, idtax_good
