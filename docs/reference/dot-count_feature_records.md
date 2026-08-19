# Count Expected Feature Records (Internal)

Counts the number of non-NA trait values in wide-format features_data,
matching the skip-if-NA logic in .prepare_features_data(). Used for
dry-run reporting without executing the full prepare step.

## Usage

``` r
.count_feature_records(features_data)
```

## Arguments

- features_data:

  Wide-format data frame (one row per individual, one column per trait
  plus linking columns).

## Value

Integer: number of stem attribute records that would be inserted.
