# Find the nearest existing tag for each candidate

Find the nearest existing tag for each candidate

## Usage

``` r
.nearest_tags(candidates, pool, max_dist = 1L)
```

## Arguments

- candidates:

  Character vector of tags with no exact match.

- pool:

  Character vector of tags known for the same plot.

- max_dist:

  Maximum edit distance to report.

## Value

Data frame with \`tag\`, \`nearest_tag\`, \`distance\` and
\`transposed\`; \`nearest_tag\` is \`NA\` when nothing is close enough.
