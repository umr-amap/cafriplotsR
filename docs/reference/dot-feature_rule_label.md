# How an extraction treats one feature, in words

The rule comes from \`.upd_agg_rule()\` and says what the extraction
\*does\*. A feature backed by a single record still goes through that
rule, so the single-record case is worded separately here rather than
given its own rule.

## Usage

``` r
.feature_rule_label(rule, n, i18n)
```

## Arguments

- rule:

  One of the \`.upd_agg_rule()\` values.

- n:

  Number of records backing the feature.

- i18n:

  A \`shiny.i18n\` translator (already resolved, not the reactive).

## Value

A single string.
