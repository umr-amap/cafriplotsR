# Guess an individual feature for each unclaimed column

Exact name first, then the synonym dictionaries. The individual-feature
synonyms matter most here: they are what sends a \`subplot\` or
\`sous_parcelle\` column to the \`quadrat\` feature now that
\`sous_plot_name\` is gone. Keys that are not real features are dropped,
so a dictionary entry such as \`census_id\` cannot produce an unmappable
guess.

## Usage

``` r
.census_feature_guess(free_cols, traits)
```

## Arguments

- free_cols:

  Character vector of column names still unclaimed.

- traits:

  Trait table from \`traitlist\`.

## Value

Named character vector, column name to feature name, \`""\` when no
guess was found.
