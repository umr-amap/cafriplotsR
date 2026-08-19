# Extraction Configuration Module - Server

Server logic for extraction configuration

## Usage

``` r
mod_extraction_config_server(id, selected_plots, i18n)
```

## Arguments

- id:

  Module namespace ID

- selected_plots:

  Reactive containing selected plot IDs

- i18n:

  Reactive returning shiny.i18n translator

## Value

A reactive list with:

- options:

  Named list of extraction options

- execute_trigger:

  Reactive counter incremented on extract

- individual_features_options:

  Named list of advanced query options

- individual_features_trigger:

  Reactive counter incremented on query
