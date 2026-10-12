# Observation features of one subplot record, with their pending status

Observation features of one subplot record, with their pending status

## Usage

``` r
.upd_obs_view(stored, staged, drop, id_sub_plots)
```

## Arguments

- stored:

  Stored rows (see \[.upd_obs_existing()\]).

- staged:

  Staged additions, or \`NULL\`.

- drop:

  Integer vector of \`id_subplot_feat\` staged for removal.

- id_sub_plots:

  Integer(1). The subplot record shown.

## Value

A data frame with \`id_subplot_feat\` (\`NA\` for an addition),
\`type\`, \`label\`, \`status\` (\`"stored"\`, \`"remove"\` or
\`"add"\`) and \`key\` (the addition's \`.key\`, \`NA\` for stored
rows).
