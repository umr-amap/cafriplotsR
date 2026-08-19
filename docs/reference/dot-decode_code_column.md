# Decode a coded column against an OpenForis code list

Decode a coded column against an OpenForis code list

## Usage

``` r
.decode_code_column(values, code_list, code_col, label_col)
```

## Arguments

- values:

  Vector of codes (numeric or character).

- code_list:

  Parsed code list data frame, or NULL.

- code_col:

  Name of the code column in `code_list`.

- label_col:

  Name of the label column in `code_list`.

## Value

Character vector of labels, NA where decoding failed.
