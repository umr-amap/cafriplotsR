# Query fuzzy match

\`r lifecycle::badge("superseded")\`

This function has been superseded by the intelligent matching functions
in \`taxonomic_matching.R\`. For better quality matches with
genus-constrained fuzzy search, use \[match_taxonomic_names()\] instead.

Extract from a sql database a fuzzy match on a given field using
PostgreSQL SIMILARITY.

\`r lifecycle::badge("superseded")\`

This function has been superseded by the intelligent matching functions
in \`taxonomic_matching.R\`. For better quality matches that handle
infraspecific ranks properly, use \[match_taxonomic_names()\] with
\`method = "exact"\`.

Extract from a sql database an exact match on a given field.

## Usage

``` r
.add_modif_field(dataset)
```

## Arguments

- dataset:

  tibble to add dates fields

- tbl:

  tibble with one field listing names to be searched

- field:

  string column name to be search

- values_q:

  string names to be searched

- con:

  PqConnection connection to RPostgres database

## Value

A tibble with matched records from the database Query exact match

A list of two elements: (1) res_q with matched records, (2) query_tb
with match status Internal function

Add modification day month and year column before adding/updating

Add modification date fields to dataset

tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
