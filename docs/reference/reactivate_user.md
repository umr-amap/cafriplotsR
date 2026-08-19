# Reactivate a user

Marks a previously deactivated user as active in the registry. Does NOT
restore permissions — use \`setup_user_permissions()\` to re-grant
access.

## Usage

``` r
reactivate_user(con, username)
```

## Arguments

- con:

  Connection to main database.

- username:

  Character. Username to reactivate.

## Value

TRUE if successful, FALSE otherwise.
