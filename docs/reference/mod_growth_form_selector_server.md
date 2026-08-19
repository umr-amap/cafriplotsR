# Growth Form Selector Module - Server

Server logic for hierarchical growth form selection

## Usage

``` r
mod_growth_form_selector_server(id, pool, i18n)
```

## Arguments

- id:

  Module namespace ID

- pool:

  Reactive returning taxa database connection pool

- i18n:

  Reactive returning shiny.i18n translator

## Value

List with: - growth_form_selections: Reactive list of selected growth
form paths - basisofrecord: Reactive character - measurementremarks:
Reactive character - is_valid: Reactive logical indicating if selections
are complete
