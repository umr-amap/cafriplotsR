# Taxonomic Validator Module - Server

Taxonomic Validator Module - Server

## Usage

``` r
mod_taxonomic_validator_server(id, preliminary_links, con_taxa, i18n)
```

## Arguments

- id:

  Module namespace ID

- preliminary_links:

  Reactive containing retrieved specimens from Step 4

- con_taxa:

  Reactive taxa database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive list containing validated links
