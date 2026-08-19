# Render the revision table for display

Kept out of the server so the column set and the wording can be tested
without a Shiny session.

## Usage

``` r
.census_revision_display(rev, decisions = NULL, i18n = NULL)
```

## Arguments

- rev:

  Frame from \[.classify_taxon_revisions()\].

- decisions:

  Character vector, one decision per row.

- i18n:

  Optional translator.

## Value

Data frame ready for \`DT::datatable()\`.
