# Trait Column Mapping Module - Server

Trait Column Mapping Module - Server

## Usage

``` r
mod_trait_column_mapping_server(id, data, pool, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive: uploaded data frame

- pool:

  Reactive: database connection pool

- i18n:

  Reactive: shiny.i18n translator

## Value

Reactive list: valid, trait_cols (user_col → trait_name),
available_traits
