# Review uncertain backbone matches

Opens a table of the matches from \[match_taxa_to_backbone()\] that need
a person's judgement, to accept or reject them quickly:

- **Fuzzy name**: a close backbone name;

- **Authors differ**: an identical name whose authors disagree;

- **Several candidates**: a taxon matched to several identical names
  (homonyms).

Words that differ between the internal and the backbone name, and
between their authors, are highlighted. Select rows and press A to
accept, R to reject, U to undo; the first remaining row is then
selected, so a list can be worked through from the keyboard. Accepting a
candidate rejects the taxon's other undecided candidates. Rows can also
be accepted in bulk above a name and author score.

With `review_file`, decisions are written to that file after each change
and loaded when the review is opened again with the same file, so a long
review can be spread over several sessions, even with a new matching run
(decisions are kept by taxon and backbone ID).

No database connection is used.

## Usage

``` r
review_backbone_matches(
  matches,
  review_file = NULL,
  open = TRUE,
  launch.browser = TRUE
)
```

## Arguments

- matches:

  Data frame from \[match_taxa_to_backbone()\].

- review_file:

  Optional path of an `.rds` file holding the decisions.

- open:

  Logical. Open the review page. With `FALSE`, the decisions of
  `review_file` are applied to `matches` and returned at once, which is
  what a script needs after a new matching run.

- launch.browser:

  Logical. Open in the web browser (default) rather than the RStudio
  viewer.

## Value

The matches, with `review_kind`, `decision` (`"accepted"`, `"rejected"`
or `NA`) and `verified` (`TRUE` for accepted rows). Pass them to
\[save_backbone_links()\] or \[replace_backbone_links()\]: rejected rows
are not saved, accepted rows are saved verified, undecided fuzzy and
author-mismatch rows are saved but supply no names.

## Examples

``` r
if (FALSE) { # \dontrun{
matches <- match_taxa_to_backbone("wcvp", con_taxa, author_match = "fuzzy")
matches <- review_backbone_matches(matches, review_file = "wcvp_review.rds")
table(matches$review_kind, matches$decision, useNA = "ifany")
replace_backbone_links(matches, "wcvp", con_taxa, taxa = "all")
} # }
```
