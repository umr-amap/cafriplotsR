# Column Select Module - Server

Column Select Module - Server

## Usage

``` r
mod_column_select_server(id, data, initial_column = NULL, i18n)
```

## Arguments

- id:

  Character, module ID

- data:

  Reactive data.frame from data input module

- initial_column:

  Character, optional pre-selected column name

- i18n:

  Reactive returning shiny.i18n translator

## Value

Reactive list with \$column (selected column name), \$include_authors
(logical), and \$data (potentially modified data)
