# Queries the `wcvp_names` table (taxa database) for a given name. First tries an exact case-insensitive match on `taxon_name`; if nothing is found, falls back to a genus-level filter with a prefix match on `species`, annotating rows with a `match_type` column (`"exact"` or `"fuzzy"`).

Returns `NULL` silently if the `wcvp_names` table is absent.

## Usage

``` r
.search_wcvp_backbone(name, con_taxa)
```

## Arguments

- name:

  Character. Scientific name to search (e.g.
  `"Gilbertiodendron dewevrei"`).

- con_taxa:

  Database connection (or Pool) to the taxa database.

## Value

A data frame of matching WCVP records with an extra `match_type` column,
or `NULL` if the table does not exist.
