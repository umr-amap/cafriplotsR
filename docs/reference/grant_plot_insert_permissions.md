# Setup permissions for users to insert plots

Grants necessary table-level permissions for users to insert plots into
data_liste_plots. Row-level security policies control which plots users
can see/modify after insertion.

\*\*For database administrators only.\*\*

There are two approaches: 1. \*\*Grant to specific users\*\* - Use
\`grant_plot_insert_permissions()\` 2. \*\*Grant to a role\*\* - Grant
to a role and assign users to that role

## Usage

``` r
grant_plot_insert_permissions(con, user = NULL, grant_to_public = FALSE)
```

## Arguments

- con:

  Database connection (must have GRANT privilege)

- user:

  Character. Username or role name to grant permissions to

- grant_to_public:

  Logical. If TRUE, grants to PUBLIC (all users). Default FALSE for
  security. Use with caution.

## Value

TRUE if successful, FALSE otherwise

## Details

This function grants table-level privileges: - \*\*SELECT\*\*: Read
plots (RLS controls which rows) - \*\*INSERT\*\*: Create new plots (RLS
auto-sets created_by) - \*\*UPDATE\*\*: Modify plots (RLS controls which
rows) - \*\*DELETE\*\*: Delete plots (RLS controls which rows)

After granting table privileges, Row-Level Security (RLS) policies
control: - Users can always see/modify plots they created (via
created_by column) - Admins can grant access to other users' plots via
\`define_user_policy()\`

## See also

\- \`define_user_policy()\` - Grant access to specific plots -
\`diagnose_plot_permissions()\` - Diagnose permission issues

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Grant to specific user
grant_plot_insert_permissions(con, "john.doe")

# Grant to a role (then assign users to role)
grant_plot_insert_permissions(con, "data_contributors")
# Then in PostgreSQL: GRANT data_contributors TO john_doe;

# Grant to all users (use with caution!)
grant_plot_insert_permissions(con, grant_to_public = TRUE)
} # }
```
