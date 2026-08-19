# Export taxa trait measurements for citation backfill

Exports \`taxa_traits_measures\` to a data frame (optionally saved as
Excel) with a blank \`id_citation\` column ready to be filled in
manually. Once filled, pass the result to \`apply_citation_backfill()\`.

## Usage

``` r
export_taxa_traits_for_citation_backfill(
  con = NULL,
  file = NULL,
  only_missing = TRUE
)
```

## Arguments

- con:

  Database connection to \`plots_transects\`. If NULL, calls
  \`call.mydb()\`.

- file:

  Path to an \`.xlsx\` file to write. If NULL (default), returns the
  data frame without writing.

- only_missing:

  Logical. If TRUE (default), export only rows where \`id_citation IS
  NULL\`.

## Value

A data frame with columns \`id_trait_measures\`, \`idtax\`, \`trait\`,
\`fk_id_trait\`, \`basisofrecord\`, \`measurementremarks\`,
\`id_citation\`.

## Details

The export contains only the columns needed to identify each row and
assign a citation: \`id_trait_measures\`, \`idtax\`, \`fk_id_trait\`,
\`basisofrecord\`, \`measurementremarks\`, and the current
\`id_citation\` (NA where unset). Trait names from \`traitlist\` are
joined for readability.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Return as data frame
df <- export_taxa_traits_for_citation_backfill(con)

# Write to Excel for manual editing
export_taxa_traits_for_citation_backfill(con, file = "traits_to_cite.xlsx")
} # }
```
