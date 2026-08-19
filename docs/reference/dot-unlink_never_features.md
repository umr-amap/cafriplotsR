# Drop the census link from features that do not belong to a census

Sets \`id_sub_plots\` back to \`NA\` on every row whose \`trait_name\`
the policy calls \`"never"\`, and says which features were left out.
Rows for any other feature, and rows that carry no census to begin with,
are returned untouched.

## Usage

``` r
.unlink_never_features(data, policy, quiet = FALSE)
```

## Arguments

- data:

  Data frame with \`trait_name\` and \`id_sub_plots\` columns.

- policy:

  Named character vector as returned by \[.feature_census_link()\].

- quiet:

  Logical. Suppress the message naming the features left out.

## Value

\`data\`, with \`id_sub_plots\` cleared where the policy says
\`"never"\`.
