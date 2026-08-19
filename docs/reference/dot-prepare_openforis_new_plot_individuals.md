# Build the individuals table from normalised OpenForis new-plot tree data

Every stem of a newly established plot is a new individual. The taxon
name is the field identification with the morphospecies label appended
when one was recorded (e.g. "Drypetes sp." + "sp1"); `tax_appendix` is
kept in its own column for review rather than merged.

## Usage

``` r
.prepare_openforis_new_plot_individuals(
  trees,
  specimen_prefix = NULL,
  quadrat_codes = NULL,
  morpho_codes = NULL
)
```

## Arguments

- trees:

  Normalised tree data frame.

- specimen_prefix:

  Prefix applied to voucher numbers. NULL to skip.

- quadrat_codes, morpho_codes:

  Parsed code lists, or NULL.

## Value

Data frame with one row per stem.
