# The features in a set that are never attached to a census

A convenience over an already-read policy, for callers deciding what to
offer rather than what to write. An empty policy — not read yet, or
unreadable — names nothing, leaving the caller's default in place.

## Usage

``` r
.never_linked_features(features, policy)
```

## Arguments

- features:

  Character vector of feature (trait) names.

- policy:

  Named character vector as returned by \[.feature_census_link()\].

## Value

Character vector, the subset of \`features\` the policy calls
\`"never"\`.
