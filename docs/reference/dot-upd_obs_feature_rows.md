# Build the data_subplot_feat rows for new observation features

One row per value. A people feature takes several person ids at once;
every other type takes one value. Values that are blank, or already
recorded for the same subplot and type, are dropped.

## Usage

``` r
.upd_obs_feature_rows(
  id_sub_plots,
  id_type,
  valuetype,
  values,
  existing = NULL
)
```

## Arguments

- id_sub_plots:

  Integer(1). The subplot record being described.

- id_type:

  Integer(1). \`subplotype_list.id_subplotype\` of the feature.

- valuetype:

  Character(1). \`subplotype_list.valuetype\` of the feature.

- values:

  Vector of values (person ids for a people feature).

- existing:

  Data frame of the observation features already recorded, with
  \`id_sub_plots\`, \`id_type_sub_plot\`, \`typevalue\`,
  \`typevalue_char\`.

## Value

A data frame ready for \`data_subplot_feat\`, zero rows if nothing is
left to add. Stops on a value a numeric type cannot hold.

## Details

The value goes where the feature wizard and \`add_subplot_features()\`
put it: \`typevalue\` for \`numeric\`, \`integer\` and \`table_colnam\`
types (the person id is a number), \`typevalue_char\` for the others.
