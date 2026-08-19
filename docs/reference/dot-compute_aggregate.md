# Compute one aggregate value

Pure function that turns a numeric (or character) vector into a single
aggregated value. NA values are removed before computation.

## Usage

``` r
.compute_aggregate(values, method, method_param = NULL)
```

## Arguments

- values:

  Numeric vector (or character for \`mode\`/\`concat\`/\`count\`).

- method:

  One of \`"mean"\`, \`"median"\`, \`"min"\`, \`"max"\`, \`"sum"\`,
  \`"sd"\`, \`"percentile"\`, \`"mode"\`, \`"concat"\`, \`"count"\`.

- method_param:

  Numeric parameter required by \`"percentile"\` (e.g. 95 for the 95th
  percentile). Ignored for other methods.

## Value

Length-1 list with components \`value_num\` (numeric) and \`value_char\`
(character) — exactly one is non-NA depending on the method's output
type. For \`"count"\` and \`"n"\`, returns \`value_num\`.
