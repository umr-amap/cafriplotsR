# Convert matched lookup names to IDs and discard unmatched values

Converts exact-matched names to their database IDs and removes any
unmatched values to ensure data contains ONLY numeric IDs (no mix of IDs
and text names).

## Usage

``` r
.convert_and_clean_lookup_values(data, exact_matches, mappings, con)
```

## Arguments

- data:

  Data frame with user data

- exact_matches:

  List of exact-matched values (output from .analyze_lookup_columns)

- mappings:

  Column mappings (user_col -\> db_col)

- con:

  Database connection pool

## Value

Updated data frame with names replaced by IDs and unmatched values
removed
