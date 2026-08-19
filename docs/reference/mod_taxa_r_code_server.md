# Taxa R Code Preview Module - Server

Generates equivalent R code reproducing the taxa search and (optionally)
trait extraction performed in the Shiny app.

## Usage

``` r
mod_taxa_r_code_server(
  id,
  search_params,
  selected_taxon,
  traits_fetched,
  is_public,
  i18n
)
```

## Arguments

- id:

  Module namespace ID

- search_params:

  Reactive returning a named list of search parameters captured at last
  search execution.

- selected_taxon:

  Reactive returning the selected taxon data frame (one or more rows,
  each with \`idtax_n\`).

- traits_fetched:

  Reactive returning TRUE once trait extraction has been triggered in
  the trait table module.

- is_public:

  Reactive returning TRUE if user connected with public credentials.

- i18n:

  Reactive returning shiny.i18n translator

## Value

NULL (invisible)
