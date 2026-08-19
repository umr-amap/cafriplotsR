# Fuzzy Match Column (Internal Helper)

Uses string similarity to find best match

## Usage

``` r
.fuzzy_match_column(user_col_clean, schema_cols, threshold = 0.6)
```

## Arguments

- user_col_clean:

  Cleaned user column name

- schema_cols:

  Database column names

- threshold:

  Similarity threshold

## Value

List with match and similarity, or NULL
