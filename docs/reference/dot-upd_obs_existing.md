# Rows a new value is checked against: stored and already staged

Rows a new value is checked against: stored and already staged

## Usage

``` r
.upd_obs_existing(stored, staged)
```

## Arguments

- stored:

  Stored \`data_subplot_feat\` rows, as
  \[fetch_subplot_obs_features_long()\] returns them, or \`NULL\`.

- staged:

  Staged additions, or \`NULL\`.

## Value

A data frame with \`id_sub_plots\`, \`id_type_sub_plot\`, \`typevalue\`,
\`typevalue_char\`.
