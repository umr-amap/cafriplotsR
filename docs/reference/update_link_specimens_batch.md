# Update plot data data

Update plot data plot \_ at a time

## Usage

``` r
update_link_specimens_batch(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  id_col = 1,
  launch_update = FALSE,
  add_backup = TRUE
)
```

## Arguments

- new_data:

  data frame data containing id and new values

- col_names_select:

  string plot name of the selected plots

- col_names_corresp:

  string of the selected plots

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
