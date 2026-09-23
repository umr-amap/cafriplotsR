# Match Internal Taxa to WCVP Names

Superseded by `match_taxa_to_backbone("wcvp", ...)`, which it calls.
Kept with its original column names for existing scripts.

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

  Connection to the taxa database. If `NULL`, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- tax_ids:

  Optional integer vector of `idtax_n` to match. If `NULL`, matches all
  taxa except morphospecies, mosses, lichens and fungi.

- methods:

  Character vector of matching methods. Default `c("exact", "fuzzy")`;
  fuzzy matching only runs on names without an identical backbone name.

- fuzzy_threshold:

  Numeric (0-1). Minimum name similarity for fuzzy matches. Default 0.9.

- author_match:

  Character. How authors settle exact matches: `"none"` (default)
  ignores them, `"exact"` requires identical strings, `"fuzzy"` compares
  them by Jaro-Winkler similarity. Authors are taken from
  `author1`/`author2`/`author3` of `table_taxa`, for the deepest rank
  present.

- author_threshold:

  Numeric (0-1). Minimum author similarity when
  `author_match = "fuzzy"`. Default 0.6.

- n_cores:

  Integer. Parallel workers for fuzzy matching. Default 1.

- verbose:

  Logical. Show progress. Default `TRUE`.

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
