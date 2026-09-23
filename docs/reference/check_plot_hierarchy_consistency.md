# Check plot hierarchy consistency

Validates the plot parent hierarchy in \`data_liste_plots\`. The
database constraints added by \`inst/migrations/plot_hierarchy.R\` check
one row at a time: they stop a plot from being its own parent, and they
refuse a parent without a relation. Nothing in the schema can see a
\*chain\*, so nothing stops A -\> B -\> A. That is the main thing this
function looks for.

Checks performed:

- \`cycles\`:

  A plot that is its own ancestor. Walks the chain with a recursive
  query. \*\*Not auto-fixable\*\* - which edge to break is a judgement
  call.

- \`self_parent\`:

  A plot pointing at itself. Blocked by \`chk_plot_not_own_parent\`, so
  this can only appear in a database where the constraint was never
  added. Auto-fixable.

- \`dangling_parent\`:

  \`id_parent_plot\` pointing at a plot that does not exist. Blocked by
  the foreign key; possible in a restored copy that predates it.
  Auto-fixable.

- \`relation_without_parent\`:

  A \`parent_relation\` with no parent, which means nothing.
  Auto-fixable.

- \`parent_without_relation\`:

  A parent with no relation. \*\*Not auto-fixable\*\* - only a human
  knows whether the child tiles the parent or overlaps it, and guessing
  wrong corrupts every aggregation over that pair.

- \`unknown_relation\`:

  A \`parent_relation\` outside the vocabulary. \*\*Not
  auto-fixable.\*\*

When the hierarchy is clean the function reports its shape - how many
plots have a parent, under which relation, and how deep the deepest
chain runs - because a chain deeper than two is worth a second look.

If \`inst/migrations/plot_hierarchy.R\` has not been applied there is
nothing to check, and the function says so and returns \`NULL\`.

## Usage

``` r
check_plot_hierarchy_consistency(
  con = NULL,
  fix = FALSE,
  force = FALSE,
  limit = 100,
  max_depth = 100
)
```

## Arguments

- con:

  Database connection or pool to the main database. If NULL, connects
  with \[call.mydb()\].

- fix:

  Logical. Attempt to repair the auto-fixable issues? Default FALSE.
  Writes to the database, so it asks for confirmation first unless
  \`force = TRUE\`.

- force:

  Logical. Skip the confirmation prompt when \`fix = TRUE\`. Default
  FALSE.

- limit:

  Integer. Maximum rows returned per issue type. Default 100.

- max_depth:

  Integer. How far the chain walk follows a parent link before giving
  up. Default 100, which is far beyond any real hierarchy and exists
  only so a cycle cannot spin forever.

## Value

\`NULL\` if the hierarchy is consistent (or unmigrated), invisibly when
\`fix = TRUE\`. Otherwise a named list of data frames, one per issue
type found. When \`fix = TRUE\`, the list of remaining issues after
repair.

## See also

\[check_hierarchy_consistency()\] for the taxonomic equivalent,
\[safe_delete_plot()\] which refuses to orphan a child plot.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Report only
issues <- check_plot_hierarchy_consistency(con)

# Repair what can be repaired unambiguously
check_plot_hierarchy_consistency(con, fix = TRUE)
} # }
```
