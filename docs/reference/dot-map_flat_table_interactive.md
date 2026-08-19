# Map Flat Table Interactively (New Workflow) - Internal

Main coordinator for single flat table workflow. Performs automatic
mapping where possible, then interactively classifies unmapped columns
as either individual columns or trait/feature columns.

## Usage

``` r
.map_flat_table_interactive(
  data,
  con,
  similarity_threshold = 0.6,
  interactive = TRUE
)
```

## Arguments

- data:

  Data frame with all columns mixed together

- con:

  Database connection

- similarity_threshold:

  Fuzzy matching threshold

- interactive:

  Enable interactive classification

## Value

List with individuals, features, and mapping_info
