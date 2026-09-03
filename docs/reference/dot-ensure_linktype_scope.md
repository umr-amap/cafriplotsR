# Ensure a Link Type Table Carries a scope Column

\`linktypelist.scope\` is added by the \`reference_plot_linktype\`
migration. Before it has run the column is absent, so it is
reconstructed from the link type names: everything is individual-level
except \`reference_plot\`. This lets the rest of the package treat scope
as always present.

## Usage

``` r
.ensure_linktype_scope(linktypes)
```

## Arguments

- linktypes:

  Tibble as returned by the linktypelist query

## Value

The same tibble, with a \`scope\` column
