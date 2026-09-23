# Which values of a name column hold no name?

One rule for the preview, the pipeline and the review list, so the three
count the same names.

## Usage

``` r
.is_missing_name(x)
```

## Arguments

- x:

  Vector of names.

## Value

Logical vector, \`TRUE\` for NA, empty or whitespace-only values.
