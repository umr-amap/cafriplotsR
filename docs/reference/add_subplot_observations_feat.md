# Add subplot observations features

A short description...

## Usage

``` r
add_subplot_observations_feat(
  new_data,
  id_sub_plots = "id_sub_plots",
  features,
  allow_multiple_value = FALSE,
  add_data = FALSE,
  interactive = TRUE
)
```

## Arguments

- new_data:

  A data frame containing the new observations to add.

- id_sub_plots:

  A single string specifying the column name for subplot IDs. Optional.

- features:

  A character vector of feature names to process.

- allow_multiple_value:

  A single logical value indicating whether multiple values are allowed.
  Optional.

- add_data:

  A single logical value indicating whether to actually add data to the
  database. Optional.

## Value

A list containing \`list_features_add\`, which is a list of data frames
for each processed feature. The function may error if features are not
found in the data, if no valid values exist, or if subplot IDs are not
found in the database.
