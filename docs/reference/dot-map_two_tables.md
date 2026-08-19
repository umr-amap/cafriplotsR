# Map Two Separate Tables (Old Workflow) - Internal

Refactored old workflow for backward compatibility. Maps individuals and
features tables separately.

## Usage

``` r
.map_two_tables(
  individuals_data,
  features_data = NULL,
  con,
  similarity_threshold = 0.6,
  interactive = TRUE
)
```

## Arguments

- individuals_data:

  Data frame with individual columns

- features_data:

  Data frame with trait columns (optional)

- con:

  Database connection

- similarity_threshold:

  Fuzzy matching threshold

- interactive:

  Enable interactive review

## Value

List with individuals, features, and mapping_info
