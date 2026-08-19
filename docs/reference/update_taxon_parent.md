# Update Taxon Parent (with consistency check)

Safely updates a taxon's parent (id_parent) while maintaining
consistency with flat taxonomic columns. This is the SAFE way to modify
hierarchy.

## Usage

``` r
update_taxon_parent(
  idtax_n,
  new_parent_id,
  con = NULL,
  update_flat_columns = TRUE
)
```

## Arguments

- idtax_n:

  Taxon ID to update

- new_parent_id:

  New parent taxon ID

- con:

  Database connection to taxa database

- update_flat_columns:

  Logical, if TRUE updates flat columns to match new parent (default
  TRUE)

## Value

Logical, TRUE if successful

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb.taxa()

# Move species to different genus (updates both id_parent and tax_gen)
update_taxon_parent(
  idtax_n = 12345,
  new_parent_id = 67890,
  con = con
)
} # }
```
