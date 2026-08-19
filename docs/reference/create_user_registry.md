# Create user registry table

Creates the \`user_registry\` table in the main database to store user
metadata (email, institution, etc.). This table is used for user
tracking and communication, not for authentication.

\*\*For database administrators only.\*\*

## Usage

``` r
create_user_registry(con)
```

## Arguments

- con:

  Connection to main database.

## Value

TRUE if successful, FALSE otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()
create_user_registry(con)
} # }
```
