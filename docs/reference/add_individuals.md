# Add new individuals data

Add new individuals data

## Usage

``` r
add_individuals(
  new_data,
  col_names_select,
  col_names_corresp,
  id_col,
  features_field = NULL,
  launch_adding_data = FALSE
)
```

## Arguments

- new_data:

  tibble new data to be import

- col_names_select:

  string

- col_names_corresp:

  string

- id_col:

  integer indicate which name of col_names_select is the id for matching
  plot in metadata

- features_field:

  vector string of field names in new_data containing the features
  associated with individual or stem data

- launch_adding_data:

  logical FALSE whether adding should be done or not

## Value

No return value individuals updated

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
