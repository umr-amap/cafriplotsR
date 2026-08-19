# Query Link Between Specimens and Individuals

Query existing links between specimens and individuals. Returns link
type information when available.

## Usage

``` r
query_link_individual_specimen(id_ind = NULL, id_specimen = NULL, con = NULL)
```

## Arguments

- id_ind:

  Integer vector of individual IDs to query

- id_specimen:

  Integer vector of specimen IDs to query

- con:

  Database connection. If NULL, calls call.mydb()

## Value

Tibble with link information including link type

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
