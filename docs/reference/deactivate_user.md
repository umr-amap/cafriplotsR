# Deactivate a user

Marks a user as inactive in the registry and revokes their database
permissions. Does NOT drop the PostgreSQL role (that must be done on
OVH).

## Usage

``` r
deactivate_user(con_main, con_taxa = NULL, username, revoke_permissions = TRUE)
```

## Arguments

- con_main:

  Connection to main database.

- con_taxa:

  Connection to taxa database (optional).

- username:

  Character. Username to deactivate.

- revoke_permissions:

  Logical. If TRUE (default), revokes all table permissions and drops
  RLS policies.

## Value

TRUE if successful, FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
con_taxa <- call.mydb.taxa()
deactivate_user(con, con_taxa, "jdupont")
} # }
```
