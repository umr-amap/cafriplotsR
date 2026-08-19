# Resolve synonyms using WCVP backbone

Uses the WCVP link table and accepted_plant_name_id to resolve synonyms,
falling back to internal backbone for unlinked taxa.

## Usage

``` r
.resolve_synonyms_wcvp(idtax, include_synonyms, con_taxa)
```

## Arguments

- idtax:

  Vector of taxon IDs

- include_synonyms:

  Logical

- con_taxa:

  Connection to taxa database

## Value

Tibble with columns: idtax, idtax_good
