# Taxa Traits Table Module - Server

Fetches and displays taxa-level traits in wide/long format with citation
information for a selected taxon.

## Usage

``` r
mod_taxa_traits_table_server(id, selected_taxon, i18n)
```

## Arguments

- id:

  Module namespace ID

- selected_taxon:

  Reactive returning a single-row data frame with at least \`idtax_n\`
  (from \`table_taxa\`). Reset to NULL clears results.

- i18n:

  Reactive returning shiny.i18n translator

## Value

NULL (invisible)
