# Specimen Add Module - Server

Specimen Add Module - Server

## Usage

``` r
mod_specimen_add_server(id, pool_main, pool_taxa, i18n)
```

## Arguments

- id:

  Character, module namespace ID

- pool_main:

  Reactive returning database connection pool

- pool_taxa:

  Reactive returning taxa database connection pool

- i18n:

  Reactive returning shiny.i18n translator

## Value

Reactive with newly added specimen info
