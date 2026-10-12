# Take the right to delete plots back

Clears \`can_delete\` and, unless the account keeps it on some other
plot, revokes DELETE on the tables as well.

## Usage

``` r
revoke_delete_right(
  con,
  user,
  ids = NULL,
  tables = .plot_scope_tables(),
  revoke_table = TRUE,
  dry_run = TRUE
)
```

## Arguments

- con:

  A connection to plots_transects, as the owner of the tables.

- user:

  Character. The database role to grant to.

- ids:

  Integer vector of plot IDs, or \`NULL\` for every plot the account
  already has access to.

- tables:

  Character vector of tables to grant DELETE on. Defaults to the seven a
  plot deletion reaches.

- revoke_table:

  Logical. Also revoke the table privilege once the account holds
  \`can_delete\` on no plot. Default \`TRUE\`.

- dry_run:

  Logical. \`TRUE\` (the default) reports and changes nothing.

## Value

Invisibly the number of \`plot_access\` rows updated.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
revoke_delete_right(con, "arthur", dry_run = FALSE)
} # }
```
