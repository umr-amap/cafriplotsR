# Define user policy for row-level security

Creates or updates row-level security policies for a user on a specified
table. Supports different modes for managing plot access: replace
(default), add, or remove.

\*\*Access model\*\*: Users have access to plots through two
mechanisms: 1. \*\*Creator access\*\* (automatic): Users can always
access plots they created (via global \`creator_access\_\*\` policies
using the \`created_by\` column) 2. \*\*Explicit grants\*\* (via this
function): Admin grants access to other users' plots

This function creates per-user policies for explicit grants. The global
policies handle INSERT (open to all) and creator access (automatic).

## Usage

``` r
define_user_policy(
  con,
  user,
  ids,
  table = "data_liste_plots",
  policy_name = NULL,
  operations = "SELECT",
  drop_existing = TRUE,
  mode = c("replace", "add", "remove"),
  grant_table_privileges = TRUE
)
```

## Arguments

- con:

  A database connection object.

- user:

  Character. The username to create the policy for.

- ids:

  Integer vector. Plot IDs to grant additional SELECT/UPDATE/DELETE
  access to. These are plots created by OTHER users that this user
  should also access.

- table:

  Character. The table to apply the policy to. Default is
  "data_liste_plots".

- policy_name:

  Character. Custom policy name (optional). If NULL, generates from
  username.

- operations:

  Character vector. Operations to allow: "SELECT", "INSERT", "UPDATE",
  "DELETE", or "ALL". Default is "SELECT". Note: INSERT is handled
  globally and skipped.

- drop_existing:

  Logical. Whether to drop existing policies before creating new ones.
  Default TRUE. Ignored when mode is "add" or "remove".

- mode:

  Character. How to handle existing plot access: - "replace" (default):
  Replace existing access with new IDs - "add": Add new IDs to existing
  access - "remove": Remove specified IDs from existing access

- grant_table_privileges:

  Logical. Whether to automatically grant table-level SELECT, INSERT,
  UPDATE, DELETE privileges to the user. Default TRUE. These are
  required for the RLS policies to work - RLS controls which rows, table
  privileges control which operations. Without these privileges, RLS
  policies have no effect.

## Value

Invisibly returns TRUE on success, FALSE on failure.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Replace all access with new plots
define_user_policy(con, "user1", c(1, 2, 3), operations = "ALL")

# Add plots to existing access
define_user_policy(con, "user1", c(4, 5), operations = "ALL", mode = "add")

# Remove plots from existing access
define_user_policy(con, "user1", c(2), operations = "ALL", mode = "remove")
} # }
```
