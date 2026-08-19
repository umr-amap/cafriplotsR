# Individual Search Module - Server

Individual Search Module - Server

## Usage

``` r
mod_individual_search_server(id, pool_main, pool_taxa, i18n)
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

Reactive list with selected individuals
