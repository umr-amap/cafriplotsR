# Get Link Types from Lookup Table

Returns the available link types from the linktypelist lookup table.

## Usage

``` r
get_linktypes(con = NULL, scope = NULL)
```

## Arguments

- con:

  Database connection. If NULL, calls call.mydb()

- scope:

  Character. Restrict to link types of this scope: \`"individual"\` for
  types that link a specimen to a tree, \`"plot"\` for types that link
  it to a plot only. \`NULL\` (the default) returns every type.

## Value

Tibble with id_linktype, linktype, description, priority, scope columns

## Details

\`scope\` was added by the \`reference_plot_linktype\` migration. On a
database where that has not run the column is absent, and it is
reconstructed here from the link type names so that callers can rely on
it either way.
