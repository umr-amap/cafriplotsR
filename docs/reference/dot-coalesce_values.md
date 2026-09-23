# Take \`a\`, and \`b\` where \`a\` is missing

Blank strings count as missing. When the two vectors have different
classes and both contribute, the result is character.

## Usage

``` r
.coalesce_values(a, b)
```

## Arguments

- a, b:

  Vectors of equal length, or NULL.

## Value

A vector, or NULL when both are NULL.
