# Get WCVP Names for Internal Taxa

Looks up WCVP names for given `idtax_n` values via the link table.
Optionally resolves WCVP synonyms to their accepted names.

## Usage

``` r
get_wcvp_names(idtax_n, con_taxa = NULL, resolve_synonyms = TRUE)
```

## Arguments

- idtax_n:

  Integer vector of internal taxon IDs.

- con_taxa:

  Connection to the taxa database. If NULL, calls
  [`call.mydb.taxa()`](https://umr-amap.github.io/cafriplotsR/reference/call.mydb.taxa.md).

- resolve_synonyms:

  Logical. If TRUE and a linked WCVP name is a synonym, follow
  `accepted_plant_name_id` to the accepted name. Default TRUE.

## Value

A tibble with columns: `idtax_n`, `plant_name_id`, `wcvp_taxon_name`,
`wcvp_family`, `wcvp_taxon_status`, `wcvp_taxon_authors`, `name_source`.

## Details

Taxa not found in the link table get `name_source = "internal"`.

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
wcvp_info <- get_wcvp_names(c(123, 456, 789), con_taxa)
} # }
```
