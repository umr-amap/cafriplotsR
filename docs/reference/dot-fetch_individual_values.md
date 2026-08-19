# Fetch individual measurements for a single trait, joined to taxon ids.

Returns columns \`idtax\`, \`traitvalue\`, \`traitvalue_char\`,
\`valuetype\`. Rows whose \`issue\` is non-null are excluded.

## Usage

``` r
.fetch_individual_values(source_trait_id, con)
```
