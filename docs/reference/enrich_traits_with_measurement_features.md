# Enrich trait data with measurement features (generic)

Adds measurement-level metadata/features to trait data. Only works with
long format data.

## Usage

``` r
enrich_traits_with_measurement_features(
  data,
  src = c("individuals", "taxa"),
  format = c("long", "wide")
)
```

## Arguments

- data:

  Trait data frame

- src:

  Source: "individuals" or "taxa"

- format:

  Data format: "long" or "wide"

## Value

Enriched data frame
