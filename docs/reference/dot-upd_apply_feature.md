# Write feature records

Write feature records

## Usage

``` r
.upd_apply_feature(entity, values, con)
```

## Arguments

- entity:

  \`"plot"\` or \`"individual"\`.

- values:

  A named list keyed by record id; each element a named list of
  feature-table column -\> new value.

- con:

  A DBI connection.

## Value

The change tibble that was applied, or \`NULL\`.
