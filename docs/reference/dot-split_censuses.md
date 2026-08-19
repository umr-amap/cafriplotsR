# Split plot data into census

split plot data into a list where each element is a census

## Usage

``` r
.split_censuses(meta, dataset)
```

## Arguments

- meta:

  tibble output of query_plot with no export individuals

- dataset:

  tibble output of query_plot with export individuals

## Value

A list with as many tibble as census

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
