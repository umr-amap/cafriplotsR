# Rename bracketed OpenForis columns (e.g. "pheno\[1\]" -\> "pheno_1")

Rename bracketed OpenForis columns (e.g. "pheno\[1\]" -\> "pheno_1")

## Usage

``` r
.rename_indexed(nms, stem, prefix)
```

## Arguments

- nms:

  Character vector of column names.

- stem:

  Base name without index (e.g. "pheno").

- prefix:

  Replacement prefix, index appended (e.g. "pheno\_").

## Value

The modified character vector.
