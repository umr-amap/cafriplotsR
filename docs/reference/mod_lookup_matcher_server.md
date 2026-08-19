# Lookup Matcher Module - Server

Lookup Matcher Module - Server

## Usage

``` r
mod_lookup_matcher_server(id, invalid_values, con, people_cols_override = NULL)
```

## Arguments

- id:

  Module namespace ID

- invalid_values:

  Reactive list of invalid values by column (e.g., list(method =
  c("val1", "val2")))

- con:

  Reactive database connection pool

- people_cols_override:

  Optional character vector (or reactive returning one) of additional
  column names that should be treated as people columns (allow_add_new =
  TRUE). Useful when the caller already knows which columns are people
  columns and wants to bypass auto-detection.

## Value

Reactive list of resolved matches
