# Rebuild a backbone's links from a new matching run

Compares new matches with the links stored for a backbone, taxon by
taxon, then (with `dry_run = FALSE`) replaces the stored links in one
transaction.

Taxa with a verified link are left untouched: their stored links are
kept and the new matches for them are ignored.

The comparison is on the link that supplies names (see
\[save_backbone_links()\] for the rule):

- `unchanged`: same preferred backbone name;

- `changed`: another preferred backbone name (`same_name = TRUE` when
  only the ID differs);

- `gained`: a preferred name where there was none;

- `lost`: no preferred name any more, e.g. a fuzzy match that now waits
  for review;

- `none`: no preferred name before or after.

## Usage

``` r
replace_backbone_links(
  matches,
  backbone,
  con_taxa = NULL,
  taxa = c("matched", "all"),
  dry_run = TRUE,
  verbose = TRUE
)
```

## Arguments

- matches:

  Data frame from \[match_taxa_to_backbone()\], optionally reviewed with
  \[review_backbone_matches()\]. Rows whose `decision` is `"rejected"`
  are dropped.

- backbone:

  Character. Backbone code.

- con_taxa:

  Connection or pool to the taxa database, with write access when
  `dry_run = FALSE`.

- taxa:

  Character. Which stored links are replaced: `"matched"` (default) only
  those of the taxa present in `matches`; `"all"` every link of the
  backbone, so taxa that no longer match lose theirs. Use `"all"` after
  matching every taxon.

- dry_run:

  Logical. If `TRUE` (default), compare and report only.

- verbose:

  Logical. Show the comparison. Default `TRUE`.

## Value

Invisibly, a tibble with one row per taxon: `idtax_n`,
`taxon_name_internal`, `old_external_id`, `old_name`, `new_external_id`,
`new_name`, `old_links`, `new_links`, `change`, `same_name`.

## Examples

``` r
if (FALSE) { # \dontrun{
matches <- match_taxa_to_backbone("wcvp", con_taxa, author_match = "fuzzy")
cmp <- replace_backbone_links(matches, "wcvp", con_taxa, taxa = "all")
replace_backbone_links(matches, "wcvp", con_taxa, taxa = "all", dry_run = FALSE)
} # }
```
