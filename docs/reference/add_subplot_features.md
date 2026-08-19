# Add an observation in subplot_features table

Add a observation in subplot_features table

## Usage

``` r
add_subplot_features(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  plot_name_field = NULL,
  id_plot_name = NULL,
  id_plot_name_corresp = "id_table_liste_plots_n",
  subplottype_field,
  features_field = NULL,
  add_data = FALSE,
  ask_before_update = TRUE,
  interactive = TRUE,
  verbose = TRUE,
  check_existing_data = TRUE,
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

- plot_name_field:

  string column name which contain the plot_name for linking

- id_plot_name:

  string id of plot_name

- subplottype_field:

  string vector listing trait columns names in new_data

- add_data:

  logical whether or not data should be added - by default FALSE

- ask_before_update:

  logical ask before adding

- interactive:

  logical whether to use interactive prompts (default TRUE)

- verbose:

  logical

- check_existing_data:

  logical if it should be checked if imported data already exist in the
  database

- con:

  database connection (optional, will create if NULL)

## Value

list of tibbles that should be/have been added

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
