# Validate Plot Access (Internal)

Check that plots exist in database and user has access.

## Usage

``` r
.validate_plot_access(data, con)
```

## Arguments

- data:

  Data frame with plot_name column

- con:

  Database connection

## Value

List with errors and warnings
