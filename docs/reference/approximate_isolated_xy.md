# Interpolate x y position based on neighnour

Experimental - Interpolate x y position based on neighnour, for isolated
missing value only

## Usage

``` r
approximate_isolated_xy(
  dataset,
  col_subplot = "quadrat",
  col_plot = "plot_name",
  col_pos_x = "position_x_iphone",
  col_pos_y = "position_y_iphone",
  tag = "tag"
)
```

## Arguments

- dataset:

  extract

- col_subplot:

  string column name of quadrat

- col_plot:

  string column name of plot

- col_pos_x:

  numeric column name of x relative positioning within quadrat

- col_pos_y:

  numeric column name of y relative positioning within quadrat

- tag:

  numeric tag of individual within plot

## Details

Interpolate and fill x and y positioning

## Author

Gilles Dauby
