# Get summary of individuals linked to a specimen

Internal helper function to retrieve and summarize individuals linked to
a specimen. Returns a summary grouped by plot showing number of
individuals and their current taxonomy.

## Usage

``` r
.get_linked_individuals_summary(id_specimen, con)
```

## Arguments

- id_specimen:

  integer, specimen ID

- con:

  database connection

## Value

A tibble with columns: plot_name, n_individuals, idtax_n,
full_name_no_auth
