# Taxonomic Name Standardization App

Main Shiny application for standardizing taxonomic names against the
backbone database. This app provides a modern, modular interface for
matching species names with intelligent fuzzy matching and manual review
capabilities.

## Usage

``` r
app_taxonomic_match(
  data = NULL,
  name_column = NULL,
  language = "fr",
  min_similarity = 0.6,
  max_suggestions = 10,
  mode = "interactive",
  pool_taxa = NULL
)
```

## Arguments

- data:

  Optional data.frame or reactive, pre-loaded data to standardize

- name_column:

  Optional character, pre-selected column name containing taxa

- language:

  Character, initial language ("en" or "fr"), default: "fr"

- min_similarity:

  Numeric, minimum similarity for fuzzy matching (0-1), default: 0.3

- max_suggestions:

  Integer, maximum suggestions per name, default: 10

- mode:

  Character, review mode ("interactive" or "batch"), default:
  "interactive"

- pool_taxa:

  Optional connection pool for taxa database (will prompt for login if
  NULL)

## Value

Shiny app object
