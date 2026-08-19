# Get all registered users

Retrieves all users from the \`user_registry\` table, optionally
filtered by active status.

## Usage

``` r
get_registered_users(con, active_only = TRUE)
```

## Arguments

- con:

  Connection to main database.

- active_only:

  Logical. If TRUE (default), only return active users.

## Value

A data.frame with user information.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
get_registered_users(con)
get_registered_users(con, active_only = FALSE)
} # }
```
