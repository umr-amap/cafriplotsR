# Trait Metadata Mapping Module - Server

Trait Metadata Mapping Module - Server

## Usage

``` r
mod_trait_metadata_mapping_server(id, data, trait_mapping, pool, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive: uploaded data frame

- trait_mapping:

  Reactive: result from mod_trait_column_mapping_server

- pool:

  Reactive: database connection pool

- i18n:

  Reactive: shiny.i18n translator

## Value

Reactive list: valid, idtax_col, metadata_cols, feature_cols,
available_traits
