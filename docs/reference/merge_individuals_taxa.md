# Merge individual records with taxonomic information

Merges individual tree records with resolved taxonomy from both direct
links and specimen links, handling synonym resolution

## Usage

``` r
merge_individuals_taxa(
  id_individual = NULL,
  id_plot = NULL,
  id_tax = NULL,
  clean_columns = TRUE,
  con_taxa = NULL,
  con = NULL,
  backbone = c("internal", "wcvp")
)
```

## Arguments

- id_individual:

  vector of individual IDs to filter

- id_plot:

  vector of plot IDs to filter

- id_tax:

  vector of taxa IDs to filter final results

- clean_columns:

  logical, whether to remove redundant columns

- con_taxa:

  connexion

- con:

  connexion

- backbone:

  character. `"internal"` (default) or `"wcvp"`. When `"wcvp"`, results
  are enriched with WCVP names via the link table.

## Value

A tibble with individuals and resolved taxonomy

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
