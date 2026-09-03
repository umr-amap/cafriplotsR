# Add 1-ha plot quadrat coordinates

Reshape a table of quadrat (jalon) GPS coordinates - one row per
measured quadrat, with its theoretical X/Y position within the plot and
its measured latitude and longitude - into the wide, one-row-per-plot
layout used by the `ddlat_plot_X_Y_<X>_<Y>` and `ddlon_plot_X_Y_<X>_<Y>`
subplot features, and optionally add those features to the database with
[`add_subplot_features`](https://umr-amap.github.io/cafriplotsR/reference/add_subplot_features.md).

Several measurements of the same quadrat of the same plot are averaged.
Quadrats missing for a given plot are left empty and are not added.

## Usage

``` r
add_plot_coordinates(
  dataset,
  ddlat = "Latitude",
  ddlon = "Longitude",
  launch_add_data = FALSE,
  X_theo = "X_theo",
  Y_theo = "Y_theo",
  check_existing_data = TRUE,
  add_cols = NULL,
  cor_cols = NULL,
  collector_field = NULL,
  plot_name_field = "plot_name",
  con = NULL
)
```

## Arguments

- dataset:

  tibble with one row per measured quadrat

- ddlat:

  column name of dataset containing latitude in decimal degrees

- ddlon:

  column name of dataset containing longitude in decimal degrees

- launch_add_data:

  logical, whether data should be added to the database, `FALSE` by
  default

- X_theo:

  column that contain the X quadrat name

- Y_theo:

  column that contain the Y quadrat name

- check_existing_data:

  check if data already exists

- add_cols:

  string character vectors with columns names of dataset of additonal
  information

- cor_cols:

  string character vectors with colums names corresponding to add_cols

- collector_field:

  string vector of size one with column name containing the name of the
  person collecting data. It is kept in the returned tables but is not
  written to the database: `data_liste_sub_plots` only stores a
  collector for `table_colnam` features.

- plot_name_field:

  column name holding the plot name, `"plot_name"` by default

- con:

  database connection, created if `NULL` and needed

## Value

a named list of two tibbles, `ddlat` and `ddlon`, with one row per plot
and one column per quadrat

## Details

Column names are matched case-insensitively, so a dataset holding
`x_theo` and `y_theo` is accepted with the default `X_theo` / `Y_theo`
arguments. A column that cannot be found at all raises an error, instead
of silently producing a single meaningless `X_theo_Y_theo` quadrat.

When `launch_add_data = TRUE`, or when `con` is provided, the generated
feature names are checked against `subplotype_list` before anything is
written:
[`add_subplot_features`](https://umr-amap.github.io/cafriplotsR/reference/add_subplot_features.md)
would otherwise fall back to an interactive fuzzy match for an unknown
feature and risk storing the coordinates under the wrong subplot type.
Missing types can be created with
[`add_subplottype`](https://umr-amap.github.io/cafriplotsR/reference/add_subplottype.md).

## Author

Gilles Dauby, <gilles.dauby@ird.fr>

## Examples

``` r
if (FALSE) { # \dontrun{
jalons <- readxl::read_excel("SOMALOMO_jalons.xlsx")

# rehearsal: reshape and print, nothing is written
coords <- add_plot_coordinates(jalons, ddlat = "latitude", ddlon = "longitude")

# add to the database, dating the features with the survey year and month
jalons <- dplyr::mutate(jalons, coly = 2026, colm = 6)
add_plot_coordinates(jalons, ddlat = "latitude", ddlon = "longitude",
                     add_cols = c("coly", "colm"),
                     cor_cols = c("year", "month"),
                     launch_add_data = TRUE)
} # }
```
