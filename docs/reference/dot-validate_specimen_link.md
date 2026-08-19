# Validate Specimen Link Before Adding

Internal function to validate that all foreign key references exist.

## Usage

``` r
.validate_specimen_link(id_specimen, id_n, id_linktype, con)
```

## Arguments

- id_specimen:

  Integer specimen ID

- id_n:

  Integer individual ID

- id_linktype:

  Integer link type ID

- con:

  Database connection

## Value

List with valid (logical) and errors (character vector)
