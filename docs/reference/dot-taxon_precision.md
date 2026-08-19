# Rank a taxonomic level so two determinations can be compared

A file that names a genus where the database names a species is almost
always a data entry regression rather than a revision, and has to be
told apart from a genuine correction. Morphospecies (\`Baphia sp1\`)
carry no \`tax_level\` but do carry a genus and an epithet, so they rank
with species.

## Usage

``` r
.taxon_precision(tax_level, morpho = NULL)
```

## Arguments

- tax_level:

  Character vector from \`table_taxa.tax_level\`.

- morpho:

  Logical vector from \`table_taxa.morpho_species\`.

## Value

Integer vector, higher means more precise; \`NA\` when unknown.
