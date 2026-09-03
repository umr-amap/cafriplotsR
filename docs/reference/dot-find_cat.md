# Internal function

Semi automatic matching with a table for comparison

## Usage

``` r
.find_cat(value_to_search, compared_table, column_name, field_label = NULL)
```

## Arguments

- value_to_search:

  string vector of one element

- compared_table:

  tibble with one column where the value should be compared

- column_name:

  string name of the column of compared_table

- field_label:

  string shown at the prompt in place of column_name

## Value

vector

## Author

Gilles Dauby, <gilles.dauby@ird.fr>
