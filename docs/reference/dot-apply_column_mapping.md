# Apply Column Mapping (Internal Helper)

Applies the final mapping to rename columns in user data.

## Usage

``` r
.apply_column_mapping(user_data, mapping)
```

## Arguments

- user_data:

  Data frame with user column names

- mapping:

  Named character vector: user_col_name = database_col_name

## Value

Data frame with renamed columns
