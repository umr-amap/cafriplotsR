# Give an account the right to delete plots

Grants DELETE on the seven tables a plot deletion reaches, and sets
\`can_delete\` on that account's \`plot_access\` rows for the named
plots.

Both layers are needed, and they are not equally precise. \`can_delete\`
is per plot, and gates \`data_liste_plots\` only. The child tables are
plot-scoped since step 5, but on \`can_write\` - so an account that may
write a plot may delete rows beneath it, and the account-wide table
privilege is the only gate on that. This function says so on every call
rather than leaving the caller to infer a per-plot delete on individuals
and measurements that does not exist.

## Usage

``` r
grant_delete_right(
  con,
  user,
  ids = NULL,
  tables = .plot_scope_tables(),
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

- dry_run:

  Logical. \`TRUE\` (the default) reports and changes nothing.

## Value

Invisibly the number of \`plot_access\` rows updated.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
grant_delete_right(con, "arthur", ids = c(2101, 2102))
grant_delete_right(con, "arthur", ids = c(2101, 2102), dry_run = FALSE)
} # }
```
