# Get backups of modified taxonomic data

List taxonomic data that has been modified

## Usage

``` r
get_updates_diconame(
  id = NULL,
  last_months = NULL,
  last_10_entry = TRUE,
  last = NULL
)
```

## Arguments

- id:

  look backups for a specific id (of taxonomic table)

- last_months:

  look backups performed this last month

- last_10_entry:

  look the last 10 backups performed

- last:

  look the last backup

## Value

A tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
