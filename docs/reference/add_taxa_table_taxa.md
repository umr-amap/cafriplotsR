# Add formatted taxa information

Helper function to add formatted taxonomic names (species,
infraspecific, with authors)

## Usage

``` r
add_taxa_table_taxa(ids = NULL, backbone = "internal")
```

## Arguments

- ids:

  vector of idtax_n to retrieve

- backbone:

  Character. Backbone whose names are used: `"internal"` (default) for
  `table_taxa`, or the code of a backbone registered in the taxa
  database (see
  [`list_backbones()`](https://umr-amap.github.io/cafriplotsR/reference/list_backbones.md)),
  such as `"wcvp"`. With another backbone, standard taxonomy columns
  (`tax_fam`, `tax_gen`, `tax_esp`, etc.) are replaced with that
  backbone's values where a preferred link exists. The original internal
  name is kept in `alt_taxon_name` and a `name_source` column indicates
  the source per row.

## Value

tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
