# Add 1ha IRd plot coordinates

print table as html in viewer reordered

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
  collector_field = NULL
)
```

## Arguments

- dataset:

  tibble

- ddlat:

  column name of dataset containing latitude in decimal degrees

- ddlon:

  column name of dataset containing longitude in decimal degrees

- launch_add_data:

  whether addd data or not

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
  person collecting data

## Value

print html in viewer

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
