# Label a staged measurement for display

Label a staged measurement for display

## Usage

``` r
.upd_ind_measure_label(
  row,
  trait,
  census = NA_character_,
  label = NA_character_,
  staged = NULL
)
```

## Arguments

- row:

  One-row data frame from \[.upd_ind_measure_check()\].

- trait:

  Character(1). The feature name.

- census:

  Character(1). How the census reads, \`NA\` for none.

- label:

  Character(1). How the value reads (a person's name for a
  \`table_colnam\` feature), \`NA\` to show the stored value.

- staged:

  Measurements already staged, to continue their \`.key\`.

## Value

\`row\` with \`.trait\`, \`.census\`, \`.label\` and a unique \`.key\`.
