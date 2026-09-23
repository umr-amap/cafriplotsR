# Depth of the deepest parent chain

Only meaningful on an acyclic hierarchy; call it after the cycle check
has come back clean.

## Usage

``` r
.plot_hierarchy_depth(con, max_depth = 100)
```

## Arguments

- con:

  Raw database connection (not a pool)

- max_depth:

  Integer, hard stop on chain length

## Value

Integer depth, or NA on failure
