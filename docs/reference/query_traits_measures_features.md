# Query features associated with trait measurements

Retrieves and aggregates features (metadata) linked to specific trait
measurements. Handles numeric, character, ordinal, and table-referenced
value types.

## Usage

``` r
query_traits_measures_features(
  id_trait_measures = NULL,
  id_trait = NULL,
  src = c("individuals", "taxa"),
  format = c("wide", "long"),
  con = NULL
)
```

## Arguments

- id_trait_measures:

  Integer vector of trait measurement IDs

- id_trait:

  Integer vector of trait IDs to filter features (optional). When
  provided, only features linked to the specified trait(s) are returned.

- src:

  Source type: "individuals" or "taxa"

- format:

  Output format: "wide" (pivoted) or "long" (raw)

- con:

  Database connection (optional)

## Value

Tibble with measurement features in requested format
