# Exact name match with fuzzy author disambiguation

Internal helper. Runs `._wcvp_match_exact_db` without author filtering,
then adds fuzzy author similarity (Jaro-Winkler via stringdist) and:

- When a name has multiple WCVP hits, selects the hit with the highest
  author similarity.

- Nullifies the match when `author_threshold` is set AND author info is
  present on both sides AND the best similarity is below the threshold.

- Leaves matches intact when either side has no author string (NA).

## Usage

``` r
.wcvp_match_fuzzy_author(
  names_df,
  wcvp_names,
  name_col,
  author_col,
  id_col,
  author_threshold = 0.6
)
```

## Arguments

- names_df:

  data.frame with columns `name_col`, `author_col`, and `id_col`.

- wcvp_names:

  data.frame of WCVP names (from database or `rWCVPdata`).

- name_col:

  Character. Column in `names_df` holding taxon names.

- author_col:

  Character. Column in `names_df` holding author strings.

- id_col:

  Character. Unique row identifier column.

- author_threshold:

  Numeric (0–1). Minimum author similarity to keep a match when author
  info is present on both sides. Default 0.6.

## Value

Same column structure as `._wcvp_match_exact_db`, plus an
`author_similarity` column.
