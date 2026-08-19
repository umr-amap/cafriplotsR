# Internal function

Compare values between of given columns and identify different values
based on id matches

## Usage

``` r
.find_ids(dataset, col_new, id_col_nbr, type_data)
```

## Arguments

- dataset:

  tibble contain values to compare and id for matching

- col_new:

  string vector containing column names of dataset

- id_col_nbr:

  string vector

- type_data:

  string indicate which table of database is targetted. e.g.
  'individuals'

## Value

tibble

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
