# Project stems in geographical space

Project stems in geogaphical space

## Usage

``` r
proj_rel_xy(coord_sf, coord_rel)
```

## Arguments

- coord_sf:

  sf polygon output of query_plots, using extract_coordinates = TRUE

- coord_rel:

  tibble extract of query_plots individuals with relative coordinates

## Details

The coord_rel should have the columns x_100 and y_100 that are the
relative coordinates in the plot

## Author

Gilles Dauby
