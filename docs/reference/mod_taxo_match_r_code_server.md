# Taxonomic Matching R Code Preview Module - Server

Generates R code reproducing the automatic matching run in the app with
\`match_taxonomic_names()\`.

## Usage

``` r
mod_taxo_match_r_code_server(
  id,
  match_results,
  column_info,
  name_backbone = NULL,
  is_offline = NULL,
  i18n
)
```

## Arguments

- id:

  Module namespace ID

- match_results:

  Reactive returning the auto-matching result list (as returned by
  \`mod_auto_matching_server()\`); its \`\$params\` element carries the
  settings actually used for the last run.

- column_info:

  Reactive returning the column-selection list (as returned by
  \`mod_column_select_server()\`).

- name_backbone:

  Reactive returning the code of the backbone whose names were requested
  for the output, or `"internal"`.

- is_offline:

  Reactive returning TRUE when the app runs on the cached backbone
  without a database connection.

- i18n:

  Reactive returning shiny.i18n translator

## Value

NULL (invisible)
