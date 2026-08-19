# Backbone Cache Selection Module - Server

Server logic for backbone cache selection. When triggered, checks if
cache exists. If cache exists, shows modal dialog with cache metadata
and lets user choose between cached or fresh internal backbone. If no
cache, immediately returns "download".

## Usage

``` r
mod_backbone_cache_selection_server(id, i18n, trigger)
```

## Arguments

- id:

  Character, module namespace ID

- i18n:

  Reactive returning shiny.i18n translator object

- trigger:

  Reactive that triggers cache check (e.g., button click event)

## Value

Reactive character, user's choice: "cache", "download", or NULL
