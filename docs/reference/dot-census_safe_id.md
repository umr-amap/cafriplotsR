# Input id fragment for a user column name

Two columns differing only in punctuation collapse onto the same id,
which is why the same rule has to be used everywhere a column is turned
into an input name.

## Usage

``` r
.census_safe_id(col)
```

## Arguments

- col:

  Character vector of column names.

## Value

Character vector safe for use in an input id.
