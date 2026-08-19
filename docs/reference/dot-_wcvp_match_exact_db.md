# Exact Name Match (Database-Compatible Version)

Database-compatible replacement for `rWCVP::wcvp_match_exact`. Matches
input names (from `names_df`) to WCVP taxon names using exact string
comparison, with optional author matching.

## Usage

``` r
._wcvp_match_exact_db(
  names_df,
  wcvp_names,
  name_col,
  author_col = NULL,
  id_col
)
```

## Arguments

- names_df:

  data.frame with columns `name_col`, optionally `author_col`, and
  `id_col`.

- wcvp_names:

  data.frame of WCVP names with columns: `taxon_name`, `plant_name_id`,
  `taxon_authors`, `taxon_rank`, `taxon_status`, `ipni_id`,
  `accepted_plant_name_id`.

- name_col:

  Character. Column in `names_df` holding taxon names.

- author_col:

  Character or NULL. Column in `names_df` holding authors. If NULL,
  author matching is skipped. Default NULL.

- id_col:

  Character. Unique identifier column in `names_df` (used to
  disambiguate rows with identical names).

## Value

data.frame with one row per input name and columns: `name`, `wcvp_id`,
`wcvp_name`, `wcvp_authors`, `wcvp_rank`, `wcvp_status`,
`wcvp_homotypic`, `wcvp_ipni_id`, `wcvp_accepted_id`, `match_type`,
`match_similarity`, `match_edit_distance`, `id_col`. Unmatched rows have
NA in WCVP columns.
