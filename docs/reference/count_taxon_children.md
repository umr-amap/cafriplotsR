# Count Children at Each Level

Returns counts of children taxa at each hierarchical level for a given
taxon. Uses tax_level column to identify levels.

## Usage

``` r
count_taxon_children(idtax_n, con = NULL)
```

## Arguments

- idtax_n:

  The taxon ID to count children for

- con:

  Database connection (optional)

## Value

Named vector with counts per level
