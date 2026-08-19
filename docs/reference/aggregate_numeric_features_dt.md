# Aggregate numeric features using data.table

Aggregate numeric features using data.table

## Usage

``` r
aggregate_numeric_features_dt(
  data,
  include_census,
  mode,
  include_issue = FALSE,
  include_measurement_ids = FALSE,
  census_strategy = c("last", "first", "mean")
)
```
