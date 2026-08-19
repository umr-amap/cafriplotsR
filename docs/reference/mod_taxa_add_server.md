# Taxa Add Module - Server

Server logic for adding new taxonomic entries

## Usage

``` r
mod_taxa_add_server(id, pool, pool_main = NULL, has_write_permission, i18n)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Reactive returning taxa database connection pool

- has_write_permission:

  Reactive returning TRUE if user can write

- i18n:

  Reactive returning shiny.i18n translator
