# Specimen Import Module - Server

Specimen Import Module - Server

## Usage

``` r
mod_specimen_import_server(
  id,
  matched_data,
  mappings,
  matching_complete,
  con,
  i18n
)
```

## Arguments

- id:

  Module namespace ID

- matched_data:

  Reactive containing matched data with IDs

- mappings:

  Reactive containing column mappings

- matching_complete:

  Reactive boolean indicating if matching is complete

- con:

  Reactive database connection pool

- i18n:

  Reactive returning translator object

## Value

Reactive list containing import results
