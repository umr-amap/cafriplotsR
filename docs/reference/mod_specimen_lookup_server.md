# Specimen Lookup Module - Server

Specimen Lookup Module - Server

## Usage

``` r
mod_specimen_lookup_server(id, data, mappings, con_main, con_taxa, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive containing uploaded user data

- mappings:

  Reactive containing column mappings

- con_main:

  Reactive main database connection pool

- con_taxa:

  Reactive taxa database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive list containing matched data and status
