# Get email addresses of all registered users

Retrieves email addresses for all active registered users. Useful for
sending bulk communications.

## Usage

``` r
get_user_emails(con, as_string = FALSE)
```

## Arguments

- con:

  Connection to main database.

- as_string:

  Logical. If TRUE, returns emails as a semicolon-separated string (for
  pasting into email clients). If FALSE (default), returns a character
  vector.

## Value

Character vector of emails, or a single string if \`as_string = TRUE\`.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Get as vector
emails <- get_user_emails(con)

# Get as string for email client
get_user_emails(con, as_string = TRUE)
} # }
```
