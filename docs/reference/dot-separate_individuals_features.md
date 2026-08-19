# Separate Flat Table into Individuals and Features - Internal

Based on column classifications, separates the flat table into two
dataframes: individuals (core data) and features (traits).

## Usage

``` r
.separate_individuals_features(data, column_classifications)
```

## Arguments

- data:

  Original flat table data frame

- column_classifications:

  Data frame with classification results

## Value

List with individuals and features dataframes
