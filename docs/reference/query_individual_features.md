# Query individual features with improved architecture

Query individual features with improved architecture

## Usage

``` r
query_individual_features(
  individual_ids = NULL,
  trait_ids = NULL,
  id_trait_measures = NULL,
  include_multi_census = FALSE,
  format = c("wide", "long"),
  issues = c("remove", "include", "ignore"),
  include_metadata = FALSE,
  include_individuals = FALSE,
  census_strategy = c("last", "first", "mean"),
  con = NULL,
  backbone = c("internal", "wcvp")
)
```

## Arguments

- individual_ids:

  Numeric vector of individual IDs

- trait_ids:

  Numeric vector of trait IDs (optional filter)

- id_trait_measures:

  Integer vector of trait measurement IDs to filter on (optional). When
  provided, only the specified measurements are returned.

- include_multi_census:

  Include multiple census data

- format:

  Output format: "wide" (pivot) or "long" (raw)

- issues:

  Character. How to handle flagged measurements: "remove" (default, drop
  flagged rows), "include" (keep rows and add issue column), or "ignore"
  (keep rows, no issue column).

- include_metadata:

  Include trait measurement features (only available with format="long")

- include_individuals:

  Include linked individual data

- census_strategy:

  Character. Which census to keep when \`include_multi_census = FALSE\`:
  "last" (default), "first", or "mean" (no census filtering, all
  measurements kept). The census is selected per plot across all traits,
  so a trait with no measurement at the selected census is dropped from
  the result and named in a warning.

- con:

  Database connection (optional)

- backbone:

  Character. Which taxonomic backbone to use for synonym resolution when
  fetching linked individuals. `"internal"` (default) uses the internal
  `table_taxa`. `"wcvp"` uses WCVP via `wcvp_idtax_link`.

## Value

Tibble with individual features in requested format
