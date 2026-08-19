# Register a user in the registry

Adds or updates a user's metadata in the \`user_registry\` table. The
user must already exist as a PostgreSQL role (created via OVH portal).

## Usage

``` r
register_user(
  con,
  username,
  email = NULL,
  first_name = NULL,
  last_name = NULL,
  institution = NULL,
  notes = NULL
)
```

## Arguments

- con:

  Connection to main database.

- username:

  Character. PostgreSQL username (must match the role name).

- email:

  Character. User's email address.

- first_name:

  Character. User's first name (optional).

- last_name:

  Character. User's last name (optional).

- institution:

  Character. User's institution (optional).

- notes:

  Character. Additional notes (optional).

## Value

TRUE if successful, FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
register_user(con,
  username = "jdupont",
  email = "j.dupont@institution.org",
  first_name = "Jean",
  last_name = "Dupont",
  institution = "IRD"
)
} # }
```
