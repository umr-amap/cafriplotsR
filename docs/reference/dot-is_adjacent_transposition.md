# Is \`a\` \`b\` with two adjacent characters swapped?

Transposing two digits is one of the commonest tag typos, yet it costs 2
in edit distance and so hides below a distance-1 threshold. It gets its
own test.

## Usage

``` r
.is_adjacent_transposition(a, b)
```

## Arguments

- a, b:

  Single strings.

## Value

\`TRUE\` when the two differ only by one adjacent swap.
