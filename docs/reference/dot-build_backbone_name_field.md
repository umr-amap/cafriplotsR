# Build the searchable full-name field from a backbone tibble

Mirrors the SQL \`concat(tax_gen, ' ', tax_esp, ' ' \|\| tax_rank01,
...)\` construction but in R, returning a character vector aligned with
the input rows. Where \`tax_esp\` is NA, falls back to \`tax_gen\`.

## Usage

``` r
.build_backbone_name_field(backbone, include_authors = FALSE)
```
