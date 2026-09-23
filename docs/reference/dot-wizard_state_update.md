# Fold the result of one wizard step into the wizard state

A step module keeps returning a result after the user has left it, and
returns a degraded one while its UI is being rebuilt, so an invalid
result is only believed while the user is looking at that step. When the
result does change, every later snapshot is dropped: a metadata mapping,
a validated table or a preview built on the previous mapping describes
columns that may no longer exist. Switching step 2 between wide and long
is the case that makes this visible — without it, the Next button stays
enabled on the mapping of the format the user just left.

## Usage

``` r
.wizard_state_update(state, field, res, showing)
```

## Arguments

- state:

  Named list of snapshots in wizard order, e.g.
  `list(trait_mapping = , metadata_mapping = , validation = , import = )`.

- field:

  Name of the snapshot this result belongs to.

- res:

  Result returned by the step module.

- showing:

  TRUE when the user is currently on that step.

## Value

The state, unchanged when the result must be ignored or repeats the
stored one; otherwise with \`field\` updated and every later entry NULL.
