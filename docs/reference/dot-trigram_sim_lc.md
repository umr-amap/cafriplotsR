# \`.trigram_sim()\` for an \`x\` that is already lowercase

Lowercasing 365,000 backbone names costs ~0.4 s, and they are the same
names on every call. The index stores them lowered once; this skips the
redundant pass. Same result as \`.trigram_sim(x, y)\` when \`x\` is
lowercase.

## Usage

``` r
.trigram_sim_lc(x_lc, y)
```

## Arguments

- x_lc:

  Lowercase character vector

- y:

  Single character string to compare each \`x_lc\` against
