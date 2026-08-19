# Batch validation server

Batch validation server

## Usage

``` r
mod_specid_batch_validation_server(
  id,
  matched_data,
  mappings,
  pool_main,
  pool_taxa,
  i18n
)
```

## Arguments

- id:

  Module id

- matched_data:

  Reactive matched data (with id_colnam if applicable)

- mappings:

  Reactive mapping list

- pool_main:

  Reactive main DB pool

- pool_taxa:

  Reactive taxa DB pool

- i18n:

  Reactive translator

## Value

list(validated_data, is_valid)
