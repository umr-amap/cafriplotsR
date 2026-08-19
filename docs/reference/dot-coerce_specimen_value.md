# Coerce a user supplied value to the type expected by a specimen column

Coerce a user supplied value to the type expected by a specimen column

## Usage

``` r
.coerce_specimen_value(value, type, field)
```

## Arguments

- value:

  Length-one value (or `NA`).

- type:

  Character, one of `"character"`, `"numeric"`, `"integer"`.

- field:

  Character, field name (used in error messages).

## Value

A length-one vector of the requested type. Empty strings and `NA` are
returned as the typed `NA` (meaning "set to NULL").
