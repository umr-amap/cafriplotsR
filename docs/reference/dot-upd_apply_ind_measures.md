# Write staged measurements

Runs inside the caller's transaction (\[.upd_apply_all()\]).

## Usage

``` r
.upd_apply_ind_measures(add, con)
```

## Arguments

- add:

  \`NULL\`, or staged rows (display columns, prefixed with \`.\`,
  included).

- con:

  A DBI connection (not a pool).

## Value

The number of rows inserted.
