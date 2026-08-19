# Query fuzzy match

\`r lifecycle::badge("superseded")\`

This function has been superseded by the intelligent matching functions
in \`taxonomic_matching.R\`. For better quality matches with
genus-constrained fuzzy search, use \[match_taxonomic_names()\] instead.

Extract from a sql database a fuzzy match on a given field using
PostgreSQL SIMILARITY.

## Usage

``` r
query_fuzzy_match(tbl, field, values_q, con)
```

## Arguments

- tbl:

  tibble with one field listing names to be searched

- field:

  string column name to be search

- values_q:

  string names to be searched

- con:

  PqConnection connection to RPostgres database

## Value

A tibble with matched records from the database

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
