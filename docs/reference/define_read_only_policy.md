# Define read-only policy for a user

Convenience wrapper for
[`define_user_policy`](https://umr-amap.github.io/cafriplotsR/reference/define_user_policy.md)
that grants SELECT-only access.

## Usage

``` r
define_read_only_policy(
  con,
  user,
  ids,
  table = "data_liste_plots",
  mode = c("replace", "add", "remove")
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

- mode:

  Character. How to handle existing plot access: - "replace" (default):
  Replace existing access with new IDs - "add": Add new IDs to existing
  access - "remove": Remove specified IDs from existing access

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Grant read-only access to plots 1, 2, 3
define_read_only_policy(con, "user1", c(1, 2, 3))

# Add more plots to existing read access
define_read_only_policy(con, "user1", c(4, 5), mode = "add")
} # }
```
