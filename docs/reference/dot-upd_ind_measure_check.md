# Check a new measurement before it is staged

Problems that stop the value from being staged: \* \`census_missing\`:
the feature belongs to a census and none was chosen. \* \`duplicate\`: a
value of this feature is already stored, or staged, for the same census
(or, for a feature never linked to a census, at all). Edit the stored
one in section 4 instead. \* \`below_min\` / \`above_max\`: outside the
feature's allowed range.

## Usage

``` r
.upd_ind_measure_check(
  row,
  trait,
  policy = "always",
  stored = NULL,
  staged = NULL,
  censuses = NULL
)
```

## Arguments

- row:

  One-row data frame from \[.upd_ind_measure_row()\].

- trait:

  One-row data frame of \`traitlist\` (\`valuetype\`, and
  \`minallowedvalue\`, \`maxallowedvalue\` when present).

- policy:

  \`"always"\` or \`"never"\`, from \[.feature_census_link()\].

- stored:

  Data frame from \[.upd_ind_stored_measures()\], or \`NULL\`.

- staged:

  Measurements already staged, or \`NULL\`.

- censuses:

  Data frame from \[.upd_plot_censuses()\], or \`NULL\`.

## Value

A list with \`errors\` and \`warnings\` (character vectors of the codes
above) and \`row\`, with \`id_sub_plots\` cleared for a feature never
linked to a census and \`issue\` completed for a warning.

## Details

And one that only warns, recorded in \`issue\` the way
\`add_traits_measures()\` records it: \* \`lower_than_previous\`: a
numeric value lower than the largest value of the same feature at an
earlier census.
