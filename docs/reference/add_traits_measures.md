# Add an observation in trait measurement table

Add a trait measure in trait measurement table

## Usage

``` r
add_traits_measures(
  new_data,
  col_names_select = NULL,
  col_names_corresp = NULL,
  collector_field = NULL,
  plot_name_col = NULL,
  tag_col = NULL,
  id_individual_col = NULL,
  traits_field,
  features_field = NULL,
  allow_multiple_value = FALSE,
  add_data = FALSE,
  census_col = NULL,
  id_sub_plots_col = NULL
)
```

## Arguments

- new_data:

  tibble

- col_names_select:

  string vector

- col_names_corresp:

  string vector

- collector_field:

  string column name which contain the collector name

- plot_name_col:

  string column name containing plot names for linking to
  `id_liste_plots`. Use together with `tag_col` to resolve individuals
  by plot name + tag combination.

- tag_col:

  string column name containing individual tag numbers. Used together
  with `plot_name_col` to resolve individuals by plot + tag.

- id_individual_col:

  string column name containing individual IDs (`id_n` from
  `data_individuals`). Self-sufficient — no plot ID argument is required
  when this is provided; the plot ID is resolved internally only when
  needed for census lookup.

- traits_field:

  string vector listing trait columns names in new_data

- features_field:

  string vector listing features (column names) to link to measurements
  in new_data

- allow_multiple_value:

  if multiple values linked to one individual can be uploaded at once

- add_data:

  logical whether or not data should be added - by default FALSE

- census_col:

  string. Optional column name in `new_data` containing the census
  typevalue (integer, e.g. 1, 2, 3). The corresponding `id_sub_plots` is
  resolved automatically from the database per plot × census. Rows with
  different census values are handled correctly within a single call.
  Takes precedence over the interactive census prompt but is overridden
  by `id_sub_plots_col`.

- id_sub_plots_col:

  string. Optional column name in `new_data` containing the
  `id_sub_plots` value directly. When provided, no database lookup or
  interactive prompt is performed. Takes precedence over `census_col`.

## Value

list of tibbles that should be/have been added

## Details

This function now uses database transactions to ensure atomic
operations. When `features_field` is provided, both measurements and
their features are added together in a single transaction. If any error
occurs, all changes are rolled back automatically.

The function uses PostgreSQL's RETURNING clause for efficient ID
retrieval, eliminating the need for separate queries to fetch generated
IDs. This improves reliability and prevents race conditions from
concurrent operations.

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
