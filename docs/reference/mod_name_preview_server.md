# Name Preview Module - Server

Name Preview Module - Server

## Usage

``` r
mod_name_preview_server(id, data, column_name, i18n)
```

## Arguments

- id:

  Character, module ID

- data:

  Reactive data.frame, post column-selection data

- column_name:

  Reactive character, the column that will be matched

- i18n:

  Reactive returning shiny.i18n translator

## Value

Reactive returning the summary list from
[`.summarise_names_to_match()`](https://umr-amap.github.io/cafriplotsR/reference/dot-summarise_names_to_match.md),
or NULL when no column is selected.
