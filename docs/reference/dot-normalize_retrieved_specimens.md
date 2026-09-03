# Ensure a retrieved specimen table carries the expected columns

Adds any column missing from a \`query_specimens()\` result - which
happens when no specimen matches, or when collector/taxonomy enrichment
was skipped - filled with \`NA\` of the right type, so callers can join
and select on them unconditionally.

## Usage

``` r
.normalize_retrieved_specimens(specimens)
```

## Arguments

- specimens:

  Data frame returned by \`query_specimens()\`, or \`NULL\`.

## Value

A tibble with at least the columns of \[.empty_retrieved_specimens()\].
