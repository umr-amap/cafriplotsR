# Aggregate individual features to individual level

Takes raw trait measurements and aggregates them by individual, handling
multiple census and multiple values appropriately. Uses data.table for
optimal performance on large datasets.

## Usage

``` r
get_individual_aggregated_features(
  individual_ids = NULL,
  trait_ids = NULL,
  plot_ids = NULL,
  include_multi_census = FALSE,
  issues = c("remove", "include", "ignore"),
  aggregation_mode = c("auto", "mean", "last", "mode", "concat"),
  include_measurement_ids = FALSE,
  census_strategy = c("last", "first", "mean"),
  con = NULL
)
```

## Arguments

- individual_ids:

  Vector of individual IDs

- trait_ids:

  Vector of trait IDs to extract (optional)

- plot_ids:

  Vector of plot IDs (optional, for filtering)

- include_multi_census:

  Include census-specific values

- issues:

  Character. How to handle flagged measurements: "remove" (default, drop
  flagged rows), "include" (keep rows and add issue column), or "ignore"
  (keep rows, no issue column).

- aggregation_mode:

  How to aggregate: "mean", "last", "mode", "concat"

- include_measurement_ids:

  Include aggregated id_trait_measures column (default FALSE)

- census_strategy:

  Character. Which census to keep when \`include_multi_census = FALSE\`:
  "last" (default), "first", or "mean" (no census filtering, all
  measurements kept). The census is selected per plot across all traits,
  so a trait with no measurement at the selected census is dropped from
  the result and named in a warning.

- con:

  Database connection

## Value

Tibble with one row per individual and aggregated feature values
