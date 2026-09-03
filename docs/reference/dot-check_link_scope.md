# Check Links Against the Scope of Their Link Type

An \`individual\` link type fills \`id_n\` and leaves \`id_liste_plots\`
NULL; a \`plot\` type does the reverse. This reports rows that do
neither.

## Usage

``` r
.check_link_scope(links, con)
```

## Arguments

- links:

  Tibble with id_linktype, id_n and id_liste_plots columns

- con:

  Database connection

## Value

Character vector of error messages, empty if every link is coherent
