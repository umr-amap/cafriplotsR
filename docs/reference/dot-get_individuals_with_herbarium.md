# Get Individuals with Herbarium Information (Internal)

Queries individuals based on plot filters and filters to only those with
herbarium information and no existing links.

## Usage

``` r
.get_individuals_with_herbarium(filters, con)
```

## Arguments

- filters:

  Named list of filter values from mod_plot_filters

- con:

  Database connection pool

## Value

Data frame with individuals
