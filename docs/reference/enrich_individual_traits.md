# Enrich with individual-level traits

Enrich with individual-level traits

## Usage

``` r
enrich_individual_traits(
  individuals,
  con,
  show_multiple_census,
  issues = c("remove", "include", "ignore"),
  include_measurement_ids = FALSE,
  census_strategy = c("last", "first", "mean"),
  individual_features_format = c("wide", "long", "census_pairs")
)
```
