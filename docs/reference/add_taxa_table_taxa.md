# Add formatted taxa information

Helper function to add formatted taxonomic names (species,
infraspecific, with authors)

## Usage

``` r
add_taxa_table_taxa(ids = NULL, backbone = c("internal", "wcvp"))
```

## Arguments

- ids:

  vector of idtax_n to retrieve

- backbone:

  character. `"internal"` (default) or `"wcvp"`. When `"wcvp"`, standard
  taxonomy columns (`tax_fam`, `tax_gen`, `tax_esp`, etc.) are replaced
  with WCVP values where a link exists. The original internal name is
  kept in `alt_taxon_name` and a `name_source` column indicates the
  source per row.

## Value

tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
