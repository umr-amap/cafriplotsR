# Which database column a feature record's value lives in

A \`table\_\*\` feature is a numeric feature whose value is the
referenced table's id, so it shares the numeric column with plain
numeric features. The character column is never used for one.

## Usage

``` r
.upd_value_column(valuetype, entity = c("plot", "individual"))
```

## Arguments

- valuetype:

  The feature's \`valuetype\`.

- entity:

  \`"plot"\` or \`"individual"\`.

## Value

\`"typevalue"\` / \`"typevalue_char"\` for plots, \`"traitvalue"\` /
\`"traitvalue_char"\` for individuals.
