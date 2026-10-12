# Feature types offered for subplot observation features

The people types (\`valuetype == "table_colnam"\`) come first, then the
other types already used in \`data_subplot_feat\`. With \`all_types =
TRUE\` every remaining type of \`subplotype_list\` is offered as well.

## Usage

``` r
.upd_obs_feature_types(con, all_types = FALSE)
```

## Arguments

- con:

  A DBI connection.

- all_types:

  Logical. Offer every feature type?

## Value

A tibble with \`id_subplotype\`, \`type\`, \`valuetype\` and \`group\`
(\`"people"\`, \`"used"\` or \`"other"\`), in display order.
