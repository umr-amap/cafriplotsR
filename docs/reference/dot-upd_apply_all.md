# Write a record's flat columns and its feature records as one unit

Flat columns and features live in different tables, so applying them
separately can leave a record half-updated if the second write fails.
Both go inside one transaction; anything raised rolls the whole edit
back. For a plot the parent link joins them, written by
\[.upd_apply_plot_link()\] rather than through \`values\` - see
\[.upd_entity_spec()\] for why it cannot go the ordinary way.

## Usage

``` r
.upd_apply_all(
  entity,
  id,
  values,
  features,
  con,
  link = NULL,
  obs = NULL,
  measures = NULL
)
```

## Arguments

- entity:

  \`"plot"\` or \`"individual"\`.

- id:

  The record id.

- values:

  Named list of database column -\> new value for the flat table.

- features:

  Named list keyed by feature record id (see \[.upd_apply_feature()\]).

- con:

  A DBI connection.

- link:

  Plots only: \`NULL\`, or a list with \`id_parent_plot\` and
  \`parent_relation\` to write as the plot's parent link.

- obs:

  Plots only: \`NULL\`, or the staged observation feature changes (see
  \[.upd_apply_obs_features()\]).

- measures:

  Individuals only: \`NULL\`, or the staged new measurements (see
  \[.upd_apply_ind_measures()\]).

## Value

A list with \`n_direct\`, \`n_feature\`, \`n_link\`, \`n_obs_added\`,
\`n_obs_removed\` and \`n_measures_added\`: how many values or rows were
written.
