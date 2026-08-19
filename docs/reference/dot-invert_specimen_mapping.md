# Invert a map_user_columns() Result for Specimens (Internal)

[`map_user_columns`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md)
returns a `user_column -> database_field` mapping. The specimen wizard
is oriented `database_field -> user_column`, so this inverts the result.
Because
[`map_user_columns()`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md)
de-duplicates targets, each database field maps to at most one user
column.

## Usage

``` r
.invert_specimen_mapping(res)
```

## Arguments

- res:

  Result list from
  [`map_user_columns()`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md)
  (or NULL).

## Value

Named list keyed by database field; each element is a list with
`user_col`, `method`, and `confidence`.
