# Find Synonym Match for Individual Columns (Internal Helper)

Searches synonym dictionary for match with robust normalization. Handles
spaces, underscores, dots interchangeably.

## Usage

``` r
.find_synonym_match_individual(user_col_clean, synonyms)
```

## Arguments

- user_col_clean:

  Cleaned user column name (lowercase, trimmed)

- synonyms:

  Synonym dictionary

## Value

Database column name or NULL
