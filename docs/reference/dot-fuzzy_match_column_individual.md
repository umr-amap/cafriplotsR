# Fuzzy Match Column for Individuals (Internal Helper)

Uses string similarity to find best match.

## Usage

``` r
.fuzzy_match_column_individual(user_col_clean, expected_cols, threshold = 0.6)
```

## Arguments

- user_col_clean:

  Cleaned user column name

- expected_cols:

  Expected database column names

- threshold:

  Similarity threshold

## Value

List with match and similarity, or NULL
