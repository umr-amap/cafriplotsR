# Code Preview Module - Server

Server logic for generating and displaying equivalent R code

## Usage

``` r
mod_code_preview_server(
  id,
  filters,
  selected_plots,
  extraction_options,
  metadata_available,
  individuals_available,
  i18n,
  individual_features_options = NULL,
  individual_features_available = NULL,
  n_metadata_plots = NULL
)
```

## Arguments

- id:

  Module namespace ID

- filters:

  Reactive returning named list of filter values

- selected_plots:

  Reactive returning vector of selected plot IDs

- extraction_options:

  Reactive returning named list of extraction options

- metadata_available:

  Reactive returning TRUE when metadata has been queried

- individuals_available:

  Reactive returning TRUE when individuals have been extracted

- i18n:

  Reactive returning shiny.i18n translator

- individual_features_options:

  Reactive returning named list of individual features options
  (optional)

- individual_features_available:

  Reactive returning TRUE when individual features have been queried
  (optional)

- n_metadata_plots:

  Reactive returning the total number of plots in the metadata query
  result (optional). Used to determine whether the selected plots
  represent a subset or all queried plots.
