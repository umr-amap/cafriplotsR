# Lay UI blocks out in rows of equal-width columns

A single \`fluidRow\` of many blocks floats badly once the blocks have
descriptions of different lengths under them, so the blocks are chunked
into their own rows.

## Usage

``` r
.census_ui_grid(blocks, per_row = 3)
```

## Arguments

- blocks:

  List of UI elements.

- per_row:

  Blocks per row; must divide 12.

## Value

A list of \`fluidRow\`s, or \`NULL\` when there is nothing to lay out.
