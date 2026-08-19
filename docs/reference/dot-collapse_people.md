# Collapse people strings into a single list of names

Field teams separate names with either a comma or a semicolon, so both
are treated as separators. Names appearing in more than one input are
kept once.

## Usage

``` r
.collapse_people(...)
```

## Arguments

- ...:

  Character vectors of people names.

## Value

Single comma-separated string, or NA when nothing is left.
