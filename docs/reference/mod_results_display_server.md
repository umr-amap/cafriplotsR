# Results Display Module - Server

Server logic for results display and download

## Usage

``` r
mod_results_display_server(
  id,
  results,
  individual_features_results = NULL,
  i18n,
  con = NULL,
  citation_data = NULL
)
```

## Arguments

- id:

  Module namespace ID

- results:

  Reactive containing query_plots() results

- individual_features_results:

  Reactive containing query_individual_features() results (optional)

- i18n:

  Reactive returning shiny.i18n translator

- con:

  Reactive returning a database connection (for column documentation)

- citation_data:

  Reactive returning a citation summary data.frame (optional, see
  mod_citation_panel_server)
