# Search WCVP Backbone for a Scientific Name

Find Internal Backbone Taxa That Share the Same WCVP Accepted Name

## Usage

``` r
.check_wcvp_synonymy_candidates(plant_name_id, con_taxa)
```

## Arguments

- plant_name_id:

  Integer. WCVP plant_name_id of the matched taxon.

- con_taxa:

  Database connection (or Pool) to the taxa database.

## Value

A data frame with columns `idtax_n`, `plant_name_id`, `wcvp_name`,
`taxon_status`, `taxon_authors`, `match_type`, `tax_gen`, `tax_esp`,
`tax_fam`, `tax_rank01`, `tax_nam01`, `idtax_good_n`, or an empty data
frame on error / no matches.

## Details

Given a `plant_name_id` from `wcvp_names`, retrieves all other WCVP
entries that share the same `accepted_plant_name_id` and are already
linked to internal backbone taxa via `wcvp_idtax_link`. These are
potential synonyms of the taxon being added.
