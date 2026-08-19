# Taxa Search & Browser Module - Server

Server logic for taxonomic search and browsing

## Usage

``` r
mod_taxa_search_server(
  id,
  pool,
  i18n,
  is_public = shiny::reactive(FALSE),
  reset = shiny::reactive(NULL)
)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Reactive returning taxa database connection pool

- i18n:

  Reactive returning shiny.i18n translator

- is_public:

  Reactive returning TRUE if user connected with public credentials
  (used for R code generation). Default: always FALSE.

## Value

Reactive containing selected taxon data (full row from table_taxa)
