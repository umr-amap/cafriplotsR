# Explore allometric relation

Provide allometric data and graph dbh-height of selected taxa

## Usage

``` r
explore_allometric_taxa(
  genus_searched = NULL,
  tax_esp_searched = NULL,
  tax_fam_searched = NULL,
  id_search = NULL
)
```

## Arguments

- genus_searched:

  string

- tax_esp_searched:

  string

- tax_fam_searched:

  string

- id_search:

  integer

## Value

A tibble

A tibble of taxa or individuals if extract_individuals is TRUE

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
