# Label staged additions for display

Label staged additions for display

## Usage

``` r
.upd_obs_label_rows(rows, type, people = NULL, staged = NULL)
```

## Arguments

- rows:

  Data frame from \[.upd_obs_feature_rows()\].

- type:

  Character(1). The feature type name.

- people:

  Named vector (name -\> id) for a people feature, else \`NULL\`.

- staged:

  Additions already staged, to continue their \`.key\` sequence.

## Value

\`rows\` with \`.type\`, \`.label\` and a unique \`.key\`.
