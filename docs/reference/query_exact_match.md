# Query exact match

\`r lifecycle::badge("superseded")\`

This function has been superseded by the intelligent matching functions
in \`taxonomic_matching.R\`. For better quality matches that handle
infraspecific ranks properly, use \[match_taxonomic_names()\] with
\`method = "exact"\`.

Extract from a sql database an exact match on a given field.

## Usage

``` r
query_exact_match(tbl, field, values_q, con)
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

A list of two elements: (1) res_q with matched records, (2) query_tb
with match status

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
