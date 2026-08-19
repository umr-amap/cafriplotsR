# Match Internal Taxa to WCVP Names

Matches taxa from the internal `table_taxa` to WCVP names already
uploaded in the database, using exact and optionally fuzzy matching.

## Usage

``` r
match_taxa_to_wcvp(
  con_taxa = NULL,
  tax_ids = NULL,
  methods = c("exact", "fuzzy"),
  fuzzy_threshold = 0.9,
  author_match = c("none", "exact", "fuzzy"),
  author_threshold = 0.6,
  n_cores = 1L,
  verbose = TRUE
)
```

## Arguments

- con_taxa:

  Connection to the taxa database. If NULL, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- tax_ids:

  Optional integer vector of `idtax_n` to match. If NULL, matches all
  accepted taxa.

- methods:

  Character vector of matching methods to use. Default
  `c("exact", "fuzzy")`.

- fuzzy_threshold:

  Numeric (0-1). Minimum similarity for fuzzy matches. Default 0.9.

- author_match:

  Character. How to use author strings during exact name matching.

  - `"none"` (default): ignore authors entirely.

  - `"exact"`: authors must match character-for-character. Reduces false
    positives but misses any formatting difference.

  - `"fuzzy"`: exact name match first, then Jaro-Winkler author
    similarity to select among homonyms and filter below
    `author_threshold`. More tolerant of abbreviation/spacing
    differences.

  Authors are built from `author1` (basionym) and `author2`
  (combination) columns in `table_taxa`: `"(author1) author2"`. Not
  applied to fuzzy name matching (author disambiguation is not
  meaningful when the name itself is inexact).

- author_threshold:

  Numeric (0-1). Minimum Jaro-Winkler similarity required to keep a
  match when `author_match = "fuzzy"` and author info is present on both
  sides. Default 0.6.

- n_cores:

  Integer. Number of parallel workers for fuzzy matching. Uses forking
  on Unix and a PSOCK cluster on Windows. Default 1 (sequential). Set to
  `parallel::detectCores() - 1` to use all available cores.

- verbose:

  Logical. Show progress. Default TRUE.

## Value

A tibble with columns: `idtax_n`, `taxon_name_internal`,
`plant_name_id`, `wcvp_taxon_name`, `match_type`, `match_score`.

## Details

Returns a tibble for review. Does NOT write to the database
automatically. Use
[`save_wcvp_links()`](https://umr-amap.github.io/cafriplotsR/reference/save_wcvp_links.md)
to persist reviewed matches.

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
matches <- match_taxa_to_wcvp(con_taxa)
# Review matches, then save
save_wcvp_links(matches, con_taxa)
} # }
```
