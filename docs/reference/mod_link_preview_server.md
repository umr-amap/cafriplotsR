# Link Preview Module - Server

Link Preview Module - Server

## Usage

``` r
mod_link_preview_server(
  id,
  pool_main,
  selected_specimens,
  selected_individuals,
  i18n
)
```

## Arguments

- id:

  Character, module namespace ID

- pool_main:

  Reactive returning database connection pool

- selected_specimens:

  Reactive returning selected specimens data

- selected_individuals:

  Reactive returning selected individuals data

- i18n:

  Reactive returning shiny.i18n translator

## Value

Reactive with link creation results
