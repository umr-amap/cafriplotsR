# Group and order feature types for the observation-feature picker

Pure part of \[.upd_obs_feature_types()\].

## Usage

``` r
.upd_group_obs_feature_types(types, used, all_types = FALSE)
```

## Arguments

- types:

  Data frame with \`id_subplotype\`, \`type\`, \`valuetype\`.

- used:

  Integer vector of \`id_subplotype\` already used in
  \`data_subplot_feat\`.

- all_types:

  Logical. Keep the types of neither group?

## Value

A tibble, see \[.upd_obs_feature_types()\].
