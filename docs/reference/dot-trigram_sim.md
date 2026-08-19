# Trigram-Jaccard similarity (R-side equivalent of pg_trgm SIMILARITY)

PostgreSQL's \`SIMILARITY()\` from \`pg_trgm\` is Jaccard on padded
trigrams. \`stringdist::stringsim(method = "jaccard", q = 3)\` gives the
same coefficient on q-grams; padding is not identical to pg_trgm's
leading-two- spaces / trailing-one-space padding, so absolute scores
differ slightly. Empirically (see commit history) the two agree at
correlation ~0.99 with mean absolute delta ~0.03 on a representative
sample of taxonomic names, so the same \`min_similarity\` thresholds
carry over without retuning.

## Usage

``` r
.trigram_sim(x, y)
```

## Arguments

- x:

  Character vector

- y:

  Single character string to compare each \`x\` against
