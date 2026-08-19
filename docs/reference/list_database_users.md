# List all database users and their permissions

Retrieves comprehensive information about all users in both databases,
including their roles, privileges, and row-level security policies.

## Usage

``` r
list_database_users(
  con_main = NULL,
  con_taxa = NULL,
  include_system_users = FALSE
)
```

## Arguments

- con_main:

  Connection to main database (optional). If NULL, will connect
  automatically.

- con_taxa:

  Connection to taxa database (optional). If NULL, will connect
  automatically.

- include_system_users:

  Logical. Include system users like postgres? Default FALSE.

## Value

A list with components: - \`users\`: Data frame of all users with their
role attributes - \`table_privileges\`: Data frame of table-level
privileges per user - \`policies\`: Data frame of row-level security
policies (main db only) - \`accessible_plots\`: Data frame summarizing
plot access per user

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all user information
user_info <- list_database_users()

# View users
user_info$users

# View who can access which plots
user_info$accessible_plots

# View detailed table privileges
user_info$table_privileges
} # }
```
