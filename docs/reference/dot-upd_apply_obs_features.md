# Write staged observation feature changes

Runs inside the caller's transaction (\[.upd_apply_all()\]): removals
first, then additions, so replacing a person by the same person is
harmless.

## Usage

``` r
.upd_apply_obs_features(changes, con)
```

## Arguments

- changes:

  \`NULL\`, or a list with \`add\` (staged rows, display columns
  included) and \`remove\` (integer \`id_subplot_feat\`).

- con:

  A DBI connection (not a pool).

## Value

A list with \`n_added\` and \`n_removed\`.
