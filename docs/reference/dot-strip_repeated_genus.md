# Drop a genus repeated at the start of an epithet

\`"Garcinia"\` + \`"Garcinia kola"\` would otherwise combine into
\`"Garcinia Garcinia kola"\`, which matches nothing.

## Usage

``` r
.strip_repeated_genus(species, genus)
```

## Arguments

- species, genus:

  Character scalars.

## Value

\`species\` without a leading copy of \`genus\`.
