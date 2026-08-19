# Add an observation in trait measurement table at species level

Add a trait measure in trait measurement table

## Usage

``` r
add_sp_traits_measures(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  traits_field,
  collector = NULL,
  idtax = NULL,
  features_field = NULL,
  add_data = FALSE,
  ask_before_update = TRUE,
  basisofrecord = NULL,
  measurementremarks = NULL,
  interactive = TRUE,
  con = NULL
)
```

## Arguments

- new_data:

  tibble

- col_names_select:

  string vector

- col_names_corresp:

  string vector

- collector:

  string column name which contain the collector name

- idtax:

  string column name which contain the individual tag for linking

- add_data:

  logical whether or not data should be added - by default FALSE

- plot_name_field:

  string column name which contain the plot_name for linking

- id_plot_name:

  string column name which contain the ID of plot_name

- id_tag_plot:

  string column name which contain the ID of individuals table

## Value

list of tibbles that should be/have been added

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
