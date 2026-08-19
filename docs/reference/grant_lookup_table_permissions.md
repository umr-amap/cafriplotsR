# Grant table-level permissions for lookup tables

Grants INSERT, UPDATE, SELECT permissions on common lookup tables.
Alternative to SECURITY DEFINER functions - gives direct table access.

\*\*For database administrators only.\*\*

## Usage

``` r
grant_lookup_table_permissions(
  con,
  user,
  tables = c("table_colnam", "table_countries", "methodslist", "table_citations"),
  operations = c("SELECT", "INSERT", "UPDATE")
)
```

## Arguments

- con:

  Database connection (must have GRANT privilege)

- user:

  Character. Username to grant permissions to

- tables:

  Character vector. Tables to grant permissions on. Default:
  c("table_colnam", "table_countries", "methodslist")

- operations:

  Character vector. Operations to grant. Default: c("SELECT", "INSERT",
  "UPDATE")

## Value

TRUE if successful, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# Grant permissions to a specific user
grant_lookup_table_permissions(con, "john.doe")

# Grant only to specific table
grant_lookup_table_permissions(con, "john.doe", tables = "table_colnam")

# Grant full access including DELETE
grant_lookup_table_permissions(con, "admin_user",
                               operations = c("SELECT", "INSERT", "UPDATE", "DELETE"))
} # }
```
