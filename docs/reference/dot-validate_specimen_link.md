# Validate Specimen Link Before Adding

Internal function to validate that all foreign key references exist.

## Usage

``` r
.validate_specimen_link(
  id_specimen,
  id_n,
  id_linktype,
  con,
  id_liste_plots = NA_integer_
)
```

## Arguments

- id_specimen:

  Integer specimen ID

- id_n:

  Integer individual ID. \`NA\` for a plot-level link.

- id_linktype:

  Integer link type ID

- con:

  Database connection

- id_liste_plots:

  Integer plot ID. \`NA\` for an individual-level link.

## Value

List with valid (logical) and errors (character vector)
