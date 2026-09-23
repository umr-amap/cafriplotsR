# Trait Preview & Import Module - Server

Trait Preview & Import Module - Server

## Usage

``` r
mod_trait_preview_import_server(
  id,
  data,
  mapping,
  pool,
  i18n,
  citation = shiny::reactive(NULL)
)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive returning uploaded data frame

- mapping:

  Reactive returning mapping result from mod_trait_column_mapping_server

- pool:

  Reactive returning database connection pool

- i18n:

  Reactive returning translator

- citation:

  Reactive returning the citation step result (\`id_citation\`,
  \`citation\`), or NULL when no citation step is used

## Value

Reactive list with import_result
