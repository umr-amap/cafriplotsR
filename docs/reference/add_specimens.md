# Add new specimens data

Add new specimens data

## Usage

``` r
add_specimens(
  new_data,
  col_names_select,
  col_names_corresp,
  plot_name_field = NULL,
  collector_field = NULL,
  launch_adding_data = FALSE
)
```

## Arguments

- new_data:

  tibble new data to be imported

- col_names_select:

  string plot name of the selected plots

- col_names_corresp:

  string country of the selected plots

- plot_name_field:

  integer indicate which name of col_names_select is the id for matching
  liste plots table

- collector_field:

  integer indicate which name of col_names_select is the id for matching
  collector

- launch_adding_data:

  logical FALSE whether adding should be done or not

## Value

No return value individuals updated

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
