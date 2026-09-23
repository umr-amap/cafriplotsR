# Write a plot's parent link

Both columns move in one UPDATE. Writing them separately - which is what
the generic direct-update path does, one statement per column - cannot
work here: the row between the two statements holds a parent without a
relation, or a relation without a parent, and
\`chk_plot_parent_relation_paired\` rejects both.

## Usage

``` r
.upd_apply_plot_link(id, parent_id, relation, con)
```

## Arguments

- id:

  Integer plot id.

- parent_id:

  New parent id, \`NA\` to detach.

- relation:

  New relation, \`NA\` to detach.

- con:

  A DBI connection to the main database.

## Value

\`1L\` when the link was written, \`0L\` when it already matched.

## Details

Nothing is written unless the link actually differs from what is stored,
and nothing is written at all if \[.upd_validate_plot_link()\] finds a
problem.
