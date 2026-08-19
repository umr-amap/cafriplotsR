# Query subplot features with improved architecture

Retrieves subplot features (measurements at plot level) with optional
aggregation and enrichment with subplot observation features.

## Usage

``` r
query_plot_features(
  plot_ids = NULL,
  subplot_ids = NULL,
  subplot_type = NULL,
  format = c("wide", "long"),
  include_subplot_obs_features = FALSE,
  con = NULL
)
```

## Arguments

- plot_ids:

  Vector of plot IDs

- subplot_ids:

  Vector of subplot IDs (optional)

- subplot_type:

  Filter by subplot type (e.g., "census")

- format:

  Output format: "wide" (pivoted) or "long" (raw)

- include_subplot_obs_features:

  Include subplot observation features

- con:

  Database connection (optional)

## Value

List with components: - features_raw: Raw subplot features -
features_aggregated: Aggregated features by plot (if format="wide") -
census_info: Census-specific information (if census data present)
