# Guess the trait matching each name

Exact (case-insensitive) match first, then the closest trait by
Jaro-Winkler similarity above 0.72.

## Usage

``` r
.auto_map_trait_names(user_names, traits)
```

## Arguments

- user_names:

  Column names (wide) or trait names (long).

- traits:

  Data frame with a \`trait\` column.

## Value

Named character vector, \`"trait:\<name\>"\` or \`""\`, named by
\`user_names\`.
