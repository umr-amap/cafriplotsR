# Match internal taxa to a backbone's names

Matches taxa from the internal `table_taxa` to the names of a backbone
already imported into the taxa database. Returns a tibble for review;
nothing is written. Review the uncertain rows with
\[review_backbone_matches()\], then save with \[save_backbone_links()\]
or rebuild all links with \[replace_backbone_links()\].

Each row is a candidate link, of one of three types:

- `"exact"`: identical name, and authors agree or cannot be compared. A
  taxon with a single exact candidate gets it as its preferred link when
  saved. When several remain and exactly one is an accepted name, only
  the accepted one is returned; when none is accepted and exactly one is
  a synonym beside illegitimate or invalid names, only the synonym.
  Otherwise the candidates (homonyms) wait for review.

- `"author_mismatch"`: identical name, but every candidate's authors
  disagree with the taxon's (only with `author_match` other than
  `"none"`). Often a homonym or a misapplication, sometimes the same
  author written differently. Never preferred until verified, and not
  passed to fuzzy matching.

- `"fuzzy"`: a close name, for taxa without any identical name. Never
  preferred until verified.

Matching with authors compares each taxon's own authors: two internal
taxa with the same name and different authors can be matched to
different backbone names. Authors are compared without basionym authors
in brackets, without what precedes "ex", and ignoring spaces and dots,
so "(Klatt) B.L.Rob." agrees with "B.L.Rob.". A backbone name marked
`auct.` (a misapplication) never matches.

Internal taxa marked `auct.` in an author column (`"ZZ auct."`) are
misapplications and are not matched at all; with
\[replace_backbone_links()\] and `taxa = "all"` they lose any link they
had.

## Usage

``` r
match_taxa_to_backbone(
  backbone,
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

- backbone:

  Character. Backbone code, e.g. `"wcvp"`. A backbone not yet offered to
  users can be matched.

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

A tibble with columns `idtax_n`, `taxon_name_internal`,
`authors_internal`, `external_id`, `backbone_taxon_name`,
`backbone_authors`, `backbone_status`, `match_type`, `match_score` (name
similarity) and `author_score` (author similarity, `NA` when it cannot
be computed).

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
matches <- match_taxa_to_backbone("apd", con_taxa, author_match = "fuzzy")
matches <- review_backbone_matches(matches, review_file = "apd_review.rds")
replace_backbone_links(matches, "apd", con_taxa)
} # }
```
