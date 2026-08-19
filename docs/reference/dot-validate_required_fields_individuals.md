# Validate Required Fields for Individuals (Internal)

Validate Required Fields for Individuals (Internal)

## Usage

``` r
.validate_required_fields_individuals(
  data,
  required_cols,
  warning_only_cols = NULL
)
```

## Arguments

- data:

  Data frame

- required_cols:

  Required column names

- warning_only_cols:

  Columns that should generate warnings instead of errors (optional)

## Value

List with errors and warnings
