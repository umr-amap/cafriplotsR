# Share of "species epithet" values that are really full names

An epithet is a single lower-case word ("kola"). A value is counted as a
full name when its first word repeats that row's genus, or, with no
genus column, when it starts with a capitalised word followed by another
word ("Garcinia kola").

## Usage

``` r
.share_binomial_epithets(species, genus = NULL)
```

## Arguments

- species:

  Vector, the column chosen as species epithet.

- genus:

  Vector of the same length, the genus column, or NULL.

## Value

Numeric share in 0-1 (NA when no values), with attribute \`example\`
holding the first offending value.
