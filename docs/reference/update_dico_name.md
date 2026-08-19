# Update taxonomic data

Update taxonomic data

## Usage

``` r
update_dico_name(
  genus_searched = NULL,
  tax_esp_searched = NULL,
  tax_fam_searched = NULL,
  tax_order_searched = NULL,
  id_searched = NULL,
  new_tax_gen = NULL,
  new_tax_esp = NULL,
  new_tax_fam = NULL,
  new_tax_order = NULL,
  new_tax_rank1 = NULL,
  new_tax_rank = NULL,
  new_tax_name1 = NULL,
  new_tax_famclass = NULL,
  new_introduced_status = NULL,
  new_tax_rankesp = NULL,
  ask_before_update = TRUE,
  add_backup = TRUE,
  show_results = TRUE,
  cancel_synonymy = FALSE,
  synonym_of = NULL,
  exact_match = FALSE,
  con = NULL
)
```

## Arguments

- genus_searched:

  string genus name searched

- tax_esp_searched:

  string species name searched

- tax_fam_searched:

  string family name searched

- new_tax_gen:

  string new genus name

- new_tax_esp:

  string new species name

- new_tax_fam:

  string new family name

- new_tax_rank1:

  string new rank

- new_tax_name1:

  string new name of rank1

- ask_before_update:

  logical TRUE by default, ask for confirmation before updating

- add_backup:

  logical TRUE by default, add backup of modified data

- show_results:

  logical TRUE by default, show the data that has been modified

- synonym_of:

  list if the selected taxa should be put in synonymy with an existing
  taxa, add in a list at least one values to identify to which taxa it
  will be put in synonymy: genus, species or id

- new_full_name_auth:

  string new full name with authors

- new_taxook:

  integer new tax code

- new_morphocat:

  integer new morphocat code

- new_detvalue:

  integer new detvalue code

- new_full_name_no_auth:

  string new full name without authors - if not provided concatenate of
  new_esp and new_genus

- new_full_name_used:

  string new full_name_used

- new_full_name_used2:

  string new full_name_used2

- id_search:

  integer id of the taxa searched

- no_synonym_modif:

  logical FALSE by default, if TRUE and if the selected taxa is
  considered as synonym, then this will be modified and the selected
  taxa will not longer be a synonym

## Value

No return value individuals updated

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
