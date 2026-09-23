# Get WCVP Names for Internal Taxa

Superseded by `get_backbone_names(idtax_n, "wcvp")`, which it calls.
Kept with its original `wcvp_*` column names for existing scripts and
the Shiny modules.

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

  Logical. If TRUE and a linked WCVP name is a synonym, follow its
  pointer to the accepted name. Default TRUE.

## Value

A tibble with columns: `idtax_n`, `wcvp_plant_name_id`,
`wcvp_accepted_plant_name_id`, `wcvp_taxon_name`, `wcvp_family`,
`wcvp_genus`, `wcvp_species`, `wcvp_taxon_status`, `wcvp_taxon_authors`,
`name_source`.

## Details

Only preferred links are used. Taxa without one get
`name_source = "internal"`, as do all taxa when the WCVP backbone is not
available.

## Examples

``` r
if (FALSE) { # \dontrun{
con_taxa <- call.mydb.taxa()
wcvp_info <- get_wcvp_names(c(123, 456, 789), con_taxa)
} # }
```
