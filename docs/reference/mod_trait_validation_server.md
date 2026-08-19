# Trait Validation Module - Server

Trait Validation Module - Server

## Usage

``` r
mod_trait_validation_server(id, data, mapping, pool, i18n)
```

## Arguments

- id:

  Module namespace ID

- data:

  Reactive: uploaded data frame

- mapping:

  Reactive: combined mapping result (trait_cols, metadata_cols,
  feature_cols, idtax_col, available_traits)

- pool:

  Reactive: database connection pool

- i18n:

  Reactive: shiny.i18n translator

## Value

Reactive list: valid, cleaned_data, errors, warnings, changes_made,
summary
