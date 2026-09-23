# Auto Matching Module - Server

Auto Matching Module - Server

## Usage

``` r
mod_auto_matching_server(
  id,
  data,
  column_name,
  include_authors,
  i18n,
  name_backbone = NULL,
  is_offline = shiny::reactive(FALSE)
)
```

## Arguments

- id:

  Character, module ID

- data:

  Reactive data.frame from data input module

- column_name:

  Reactive character, name of column to match

- include_authors:

  Reactive logical, whether to include author names

- i18n:

  Reactive returning shiny.i18n translator

- name_backbone:

  Reactive returning the code of the backbone whose names should appear
  in the output, or `"internal"` (the default) to keep the internal
  ones. See
  [`list_backbones()`](https://umr-amap.github.io/cafriplotsR/reference/list_backbones.md).

## Value

Reactive list containing:

- `data`: Updated data frame with match results

- `unmatched`: Data frame of unmatched names

- `stats`: List of matching statistics

- `params`: Settings used by the last run (column, similarity threshold,
  author matching, name backbone, offline flag)
