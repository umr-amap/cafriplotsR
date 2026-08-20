# Record update module - server

Record update module - server

## Usage

``` r
mod_update_record_server(
  id,
  entity = c("plot", "individual"),
  pool_main,
  pool_taxa,
  i18n
)
```

## Arguments

- id:

  Character, module namespace id.

- entity:

  Either \`"plot"\` or \`"individual"\`.

- pool_main:

  Reactive returning the main database pool.

- pool_taxa:

  Reactive returning the taxa database pool.

- i18n:

  Reactive returning a \`shiny.i18n\` translator.

## Value

Invisibly \`NULL\`.
