# Compare a specimen number as text, whatever column type it arrived in

`readxl` returns a column of bare collection numbers as numeric and a
column of prefixed ones as character, so the two sides of a remap have
to be brought to a common form before they can be matched.
[`format()`](https://rdrr.io/r/base/format.html) rather than
[`as.character()`](https://rdrr.io/r/base/character.html) because the
latter renders a large number in scientific notation, which no remap
table spells that way.

## Usage

``` r
.specimen_key(x)
```

## Arguments

- x:

  Vector of specimen numbers.

## Value

Character vector, trimmed.
