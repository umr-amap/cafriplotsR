# Update diconame data based on id of taxa

Update diconame \_ one or more entry at a time

## Usage

``` r
update_dico_name_batch(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  id_col = 1,
  launch_update = FALSE,
  add_backup = TRUE,
  ask_before_update = FALSE
)
```

## Arguments

- new_data:

  tibble

- col_names_select:

  vector string of new_data to be used for update

- col_names_corresp:

  vector string of corresponding fields to update

- id_col:

  integer indicate which name of col_names_select is the id for matching
  data

- launch_update:

  logical if TRUE updates are performed

- add_backup:

  logical whether backup of modified data should be recorded

## Value

No return value individuals updated

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
