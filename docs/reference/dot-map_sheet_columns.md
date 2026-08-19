# Map Sheet Columns (Internal Helper)

Maps columns for a single sheet using exact, synonym, and fuzzy
matching.

## Usage

``` r
.map_sheet_columns(
  user_data,
  expected_columns,
  synonyms,
  sheet_name,
  similarity_threshold = 0.6,
  interactive = TRUE
)
```

## Arguments

- user_data:

  Data frame with user columns

- expected_columns:

  Character vector of valid database columns

- synonyms:

  Synonym dictionary

- sheet_name:

  Sheet name for messaging

- similarity_threshold:

  Fuzzy matching threshold

- interactive:

  Allow interactive review

## Value

List with mapping results
