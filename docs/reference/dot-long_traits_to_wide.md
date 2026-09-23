# Spread long-format trait data into one column per trait

Each row keeps its other columns (idtax, metadata) and gets its value in
the column of its trait, with NA in the other trait columns. This is the
shape \`add_sp_traits_measures()\` expects, and it drops the NA cells
trait by trait. Rows whose trait name is missing or not mapped are
dropped.

## Usage

``` r
.long_traits_to_wide(
  df,
  name_col,
  value_num_col = NULL,
  value_char_col = NULL,
  name_map,
  valuetypes = NULL
)
```

## Arguments

- df:

  Uploaded data frame.

- name_col:

  Column holding the trait names.

- value_num_col, value_char_col:

  Value columns; \`""\` or NULL when absent. At least one is required.

- name_map:

  Named character vector: trait name in the file -\> trait in
  \`traitlist\`, \`""\` for skipped names.

- valuetypes:

  Named character vector: trait -\> valuetype.

## Value

A data.frame without the name and value columns, plus one column per
mapped trait.

## Details

Numeric and integer traits read the numeric value column first and fall
back on the character one; other traits do the reverse.
