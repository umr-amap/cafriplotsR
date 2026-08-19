# Collect the identification revisions a census import should ask about

Thin wrapper tying the two database reads to the pure classifier, so a
caller with connections gets the finished table in one call.

## Usage

``` r
collect_taxon_revisions(
  split,
  con = NULL,
  con_taxa = NULL,
  unidentified_idtax = 351190L
)
```

## Arguments

- split:

  A \`census_split\` from \[split_census_table()\].

- con:

  Main database connection or pool.

- con_taxa:

  Taxa database connection or pool.

- unidentified_idtax:

  Taxon id standing for "not identified".

## Value

The frame described in \[.classify_taxon_revisions()\].
