# Render the explanation shown under a mapping dropdown

Turns one entry of \[.get_column_descriptions()\] into the small panel
that tells the user what the column they just picked actually holds.
Returns \`NULL\` when there is nothing to say, so callers can drop it in
unconditionally.

## Usage

``` r
.column_description_box(info, i18n = NULL)
```

## Arguments

- info:

  One element of the list returned by \[.get_column_descriptions()\], or
  \`NULL\` for an unmapped column.

- i18n:

  Optional translator object. Labels fall back to English without one;
  the description text itself comes from the database and is never
  translated.

## Value

A \`shiny.tag\`, or \`NULL\`.
