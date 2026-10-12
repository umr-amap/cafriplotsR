# Features offered when adding a measurement to an individual

Features already measured on this individual come first, then every
other feature of \`traitlist\`.

## Usage

``` r
.upd_ind_trait_choices(traits, measured = character(0))
```

## Arguments

- traits:

  Data frame of \`traitlist\` rows, with at least \`id_trait\`,
  \`trait\` and \`valuetype\`.

- measured:

  Character vector of the features already recorded for the individual.

## Value

A tibble with the \`traitlist\` columns and \`group\` (\`"measured"\` or
\`"other"\`), in display order.
