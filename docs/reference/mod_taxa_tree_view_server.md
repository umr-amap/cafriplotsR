# Taxa Tree View Module - Server

Server logic for displaying taxonomic hierarchy

## Usage

``` r
mod_taxa_tree_view_server(id, pool, selected_taxon, i18n)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Reactive returning taxa database connection pool

- selected_taxon:

  Reactive returning selected taxon data from search module

- i18n:

  Reactive returning shiny.i18n translator
