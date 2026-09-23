# Fuzzy Suggestions Module - Server

Fuzzy Suggestions Module - Server

## Usage

``` r
mod_fuzzy_suggestions_server(
  id,
  input_name,
  max_suggestions = shiny::reactive(10),
  min_similarity = shiny::reactive(0.3),
  include_authors = shiny::reactive(FALSE),
  i18n,
  backbone = shiny::reactive(NULL),
  matching_backbone = backbone
)
```

## Arguments

- id:

  Character, module ID

- input_name:

  Reactive character, the name to find suggestions for

- max_suggestions:

  Reactive or numeric, maximum suggestions to show

- min_similarity:

  Reactive or numeric, minimum similarity threshold

- include_authors:

  Reactive or logical, whether to include author names

- i18n:

  Reactive returning shiny.i18n translator

- backbone:

  Reactive returning the cached backbone tibble (or NULL). When
  non-NULL, all per-level searches run R-side without DB access —
  required for offline mode.

- matching_backbone:

  Reactive returning a backbone already passed through
  \`.prepare_backbone_for_matching()\`, or NULL. Used for the
  \`match_taxonomic_names()\` calls so the name index is not rebuilt for
  every name. Falls back to \`backbone\`.

## Value

Reactive integer, idtax_n of selected suggestion (or NULL)
