# Fast Fuzzy Matching Against WCVP Using Genus Blocking

Drop-in replacement for `rWCVP::wcvp_match_fuzzy` that reduces the
comparison space from O(n x 1.4M) to O(n x genus_size) by pre-filtering
WCVP candidates to the same genus as each input name. For a typical
input of 80 000 names this is 1 000-10 000x faster (minutes instead of
days).

## Usage

``` r
.wcvp_match_fuzzy_fast(
  names_df,
  wcvp_names,
  name_col,
  fuzzy_threshold = 0.9,
  genus_threshold = 0.9,
  n_cores = 1L,
  verbose = TRUE
)
```

## Arguments

- names_df:

  data.frame with at least a column named `name_col`.

- wcvp_names:

  data.frame of WCVP names (from database or `rWCVPdata`).

- name_col:

  Character. Column in `names_df` holding taxon names.

- fuzzy_threshold:

  Numeric (0-1). Minimum normalised similarity to report a match.
  Matches below this value are returned as NA rows. Default 0.9.

- genus_threshold:

  Numeric (0-1). Jaro-Winkler threshold used when a genus is not found
  verbatim in WCVP (genus typo fallback). Default 0.9.

- n_cores:

  Integer. Number of parallel workers. On Windows a PSOCK cluster is
  used; on Unix forking via
  [`parallel::mclapply`](https://rdrr.io/r/parallel/mcdummies.html).
  Default 1 (sequential).

- verbose:

  Logical. Show a CLI progress bar over genus blocks. Default TRUE.

## Value

A data.frame with one row per input name and columns matching the output
of `rWCVP::wcvp_match_fuzzy`: `name`, `wcvp_name`, `match_type`,
`multiple_matches`, `match_similarity`, `match_edit_distance`,
`wcvp_id`, `wcvp_authors`, `wcvp_rank`, `wcvp_status`, `wcvp_homotypic`,
`wcvp_ipni_id`, `wcvp_accepted_id`. Unmatched rows have NA in all WCVP
columns.

## Details

Algorithm:

1.  Extract genus (first word) from each input name.

2.  Index WCVP by genus via `data.table` keyed lookup.

3.  For each unique genus, retrieve its WCVP candidates (usually \< 500
    records). If the genus is absent from WCVP, fall back to the closest
    WCVP genus by Jaro-Winkler similarity (`genus_threshold`).

4.  Apply a name-length pre-filter: only candidates whose name length is
    within `floor((1 - fuzzy_threshold) * max_len) + 1` characters of
    the input name length are retained (valid because Levenshtein
    distance is bounded by the length difference).

5.  Compute
    [`stringdist::stringdistmatrix()`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
    within the filtered candidate set and select the closest hit per
    input name.
