# Trait Preview & Import Module - Server

Trait Preview & Import Module - Server

## Usage

``` r
mod_trait_preview_import_server(id, data, mapping, pool, i18n)
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

## Value

Reactive list with import_result
