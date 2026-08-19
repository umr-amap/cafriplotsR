# Name Review Module - Server

Name Review Module - Server

## Usage

``` r
mod_name_review_server(
  id,
  match_results,
  mode = "interactive",
  max_suggestions = 10,
  min_similarity = 0.3,
  i18n,
  backbone = shiny::reactive(NULL)
)
```

## Arguments

- id:

  Character, module ID

- match_results:

  Reactive list from auto matching module

- mode:

  Character, review mode ("interactive" or "batch")

- max_suggestions:

  Integer, maximum suggestions per name

- min_similarity:

  Numeric, minimum similarity threshold

- i18n:

  Reactive returning shiny.i18n translator

- backbone:

  Reactive returning the cached backbone tibble (or NULL). When
  non-NULL, custom searches and selection lookups go through the cached
  backbone — required for offline mode.

## Value

Reactive list with updated match results
