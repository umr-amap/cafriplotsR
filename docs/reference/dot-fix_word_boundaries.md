# Rewrite regex word boundaries as unicode-safe lookarounds

Converts each word-boundary escape in a pattern into `(?<![[:alpha:]])`
or `(?![[:alpha:]])`, depending on whether the boundary opens or closes
a token. Called from `.load_observations_ontology`.

## Usage

``` r
.fix_word_boundaries(pattern)
```

## Arguments

- pattern:

  Character. A regular expression, possibly containing word-boundary
  escapes.

## Value

Character. The pattern with each boundary rewritten as a lookaround.
