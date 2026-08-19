# Get plot IDs accessible to a user

Extracts the plot IDs that a user has access to based on their row-level
security policies on data_liste_plots.

## Usage

``` r
get_user_accessible_plots(con, user, table = "data_liste_plots")
```

## Arguments

- con:

  A database connection object.

- user:

  Character. The username to check.

- table:

  Character. The table to check policies on. Default is
  "data_liste_plots".

## Value

A data frame with columns: - user: the username - cmd: the operation
type (SELECT, INSERT, UPDATE, DELETE, ALL) - plot_ids: vector of
accessible plot IDs

Returns NULL if no policies found for the user.

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Get plots accessible to user 'klein'
get_user_accessible_plots(con, "klein")

# Get just the plot IDs as a vector
result <- get_user_accessible_plots(con, "klein")
plot_ids <- result$plot_ids[[1]]
} # }
```
