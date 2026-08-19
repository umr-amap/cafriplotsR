# Setup complete import wizard permissions

One-command setup to grant all necessary permissions for users to use
the import wizard. This includes: - Secure function for adding people
(table_colnam) - Table permissions for inserting plots - Table
permissions for lookup tables

\*\*For database administrators only.\*\*

## Usage

``` r
setup_import_wizard_permissions(
  con,
  user = NULL,
  grant_to_public = FALSE,
  grant_all_tables = TRUE
)
```

## Arguments

- con:

  Database connection (must have superuser or GRANT privileges)

- user:

  Character. Username or role to grant permissions to. If NULL and
  grant_to_public = FALSE, only sets up secure functions.

- grant_to_public:

  Logical. If TRUE, grants to all users (PUBLIC). Default FALSE.

- grant_all_tables:

  Logical. If TRUE, uses \`grant_all_table_permissions()\` instead of
  specific tables. Default TRUE to avoid missing table errors.

## Value

TRUE if all setups successful, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Setup for specific user (grants on all tables)
setup_import_wizard_permissions(con, "john.doe")

# Setup for all users (grants on all tables)
setup_import_wizard_permissions(con, grant_to_public = TRUE)

# Setup with specific tables only
setup_import_wizard_permissions(con, grant_to_public = TRUE, grant_all_tables = FALSE)
} # }
```
