# Update specimens table

Update specimens table

## Usage

``` r
update_ident_specimens(
  colnam = NULL,
  number = NULL,
  id_speci = NULL,
  id_colnam = NULL,
  new_genus = NULL,
  new_species = NULL,
  new_family = NULL,
  id_new_taxa = NULL,
  new_detd = NULL,
  new_detm = NULL,
  new_dety = NULL,
  new_detby = NULL,
  new_detvalue = NULL,
  new_colnbr = NULL,
  new_suffix = NULL,
  add_backup = TRUE,
  show_results = TRUE,
  only_new_ident = TRUE,
  ask_before_update = TRUE
)
```

## Arguments

- colnam:

  string collector name

- number:

  integer specimen number

- id_speci:

  integer id of specimen

- id_colnam:

  integer id of collector name

- new_genus:

  string new genus name

- new_species:

  string new species name

- new_family:

  string new family name

- id_new_taxa:

  integer id of the new taxa

- new_detd:

  integer day of identification

- new_detm:

  logical if you want to see previous modification of the entry - useful
  to see previous identification for example

- new_dety:

  logical if labels should be produced

- new_detby:

  string if labels are produced title of the label

- new_detvalue:

  string if labels are produced name of the rtf file

- add_backup:

  string if labels are produced name of the rtf file

- show_results:

  string if labels are produced name of the rtf file

- only_new_ident:

  string if labels are produced name of the rtf file

## Value

A tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
