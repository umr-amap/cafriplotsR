# Manual mode server

Manual mode server

## Usage

``` r
mod_specid_manual_server(
  id,
  pool_main,
  pool_taxa,
  i18n,
  active = shiny::reactive(TRUE)
)
```

## Arguments

- id:

  Module id

- pool_main:

  Reactive main DB pool

- pool_taxa:

  Reactive taxa DB pool

- i18n:

  Reactive translator

- active:

  Reactive logical, TRUE when this mode is active
