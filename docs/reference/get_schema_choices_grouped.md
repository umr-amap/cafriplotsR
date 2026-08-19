# Build Grouped Choices for Schema Column Dropdowns

Builds a named list of named character vectors suitable for
`selectInput` or `selectizeInput` with optgroups. Columns are grouped by
their category from the database or hardcoded config.

## Usage

``` r
get_schema_choices_grouped(
  column_descriptions,
  schema_columns,
  user_col = NULL
)
```

## Arguments

- column_descriptions:

  Named list of column descriptions (from
  [`.get_column_descriptions()`](https://umr-amap.github.io/cafriplotsR/reference/dot-get_column_descriptions.md)).
  Each element should be a list with at least `description` and
  optionally `category`.

- schema_columns:

  Character vector of all valid schema column names.

- user_col:

  Optional: name of the user column being mapped, used to sort choices
  within each group by string similarity (most relevant first).

## Value

A named list where names are category labels and values are named
character vectors of column choices (suitable for selectInput
optgroups).
