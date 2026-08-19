# Taxa Update Module - Server

Server logic for updating taxonomic records

## Usage

``` r
mod_taxa_update_server(id, pool, selected_taxon, has_write_permission, i18n)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Reactive returning taxa database connection pool

- selected_taxon:

  Reactive returning selected taxon data from search module

- has_write_permission:

  Reactive returning TRUE if user can write

- i18n:

  Reactive returning shiny.i18n translator
