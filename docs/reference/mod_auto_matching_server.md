# Auto Matching Module - Server

Auto Matching Module - Server

## Usage

``` r
mod_auto_matching_server(
  id,
  data,
  column_name,
  include_authors,
  min_similarity = 0.3,
  i18n,
  use_wcvp_names = NULL,
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

- min_similarity:

  Numeric (0-1), minimum similarity threshold for fallback. Note: UI
  displays as percentage (0-100) but parameter uses decimal (default:
  0.3 = 30%)

- i18n:

  Reactive returning shiny.i18n translator

## Value

Reactive list containing:

- `data`: Updated data frame with match results

- `unmatched`: Data frame of unmatched names

- `stats`: List of matching statistics
