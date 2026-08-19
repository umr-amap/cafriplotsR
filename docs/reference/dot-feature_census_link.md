# Should a feature be attached to the census that recorded it?

Answers for each named feature. The database has the last word: if
\`traitlist\` carries a \`census_link\` column, that is the policy, and
the built-in default fills in only where the column says nothing.
Without the column — or without a connection — the default stands alone.

## Usage

``` r
.feature_census_link(features, con = NULL)
```

## Arguments

- features:

  Character vector of feature (trait) names.

- con:

  Optional database connection or pool.

## Value

Named character vector parallel to \`features\`, each \`"always"\` or
\`"never"\`.

## Details

Anything not named is \`"always"\`. A census import exists to record
what was measured during a campaign, so attaching the measurement is the
behaviour that has to be argued out of, not into.
