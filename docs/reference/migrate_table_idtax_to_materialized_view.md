# Migrate table_idtax to Materialized View

Converts table_idtax from a regular table to a PostgreSQL materialized
view. This is a ONE-TIME migration that should only be run by database
admin.

## Usage

``` r
migrate_table_idtax_to_materialized_view(
  con,
  con_taxa = NULL,
  data_manager_users = c("dauby", "alex", "libalah"),
  setup_pg_cron = FALSE,
  dry_run = FALSE
)
```

## Arguments

- con:

  Main database connection (must have admin/superuser privileges)

- con_taxa:

  Taxa database connection (for initial data fetch)

- data_manager_users:

  Character vector of usernames to grant refresh permission. Default:
  c("dauby", "alex", "libalah")

- setup_pg_cron:

  Logical, attempt to set up automatic monthly refresh using pg_cron
  extension. Default FALSE. Requires pg_cron extension installed.

- dry_run:

  Logical, if TRUE only prints SQL commands without executing. Default
  FALSE.

## Value

List with migration results and any errors

## Details

After migration: - Users with data_manager_role can refresh the view -
Automatic staleness tracking via metadata table - Optional automatic
monthly refresh (if pg_cron available)

This function performs the following steps: 1. Creates backup table
(table_idtax_backup) 2. Drops existing table_idtax 3. Creates
materialized view table_idtax 4. Creates unique indexes 5. Creates
metadata tracking table 6. Creates PostgreSQL functions for refresh and
staleness checking 7. Creates data_manager_role and grants permissions
8. Grants role to specified users 9. Optionally sets up pg_cron
automatic refresh

ROLLBACK: If migration fails, restore from backup using:
rollback_table_idtax_migration(con)

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect as admin
con <- call.mydb()  # Use admin credentials
con_taxa <- call.mydb.taxa()

# Dry run first (just show commands)
migrate_table_idtax_to_materialized_view(con, con_taxa, dry_run = TRUE)

# Actual migration
result <- migrate_table_idtax_to_materialized_view(
  con,
  con_taxa,
  data_manager_users = c("dauby", "alex", "libalah")
)

# With automatic refresh setup
result <- migrate_table_idtax_to_materialized_view(
  con,
  con_taxa,
  data_manager_users = c("dauby", "alex", "libalah"),
  setup_pg_cron = TRUE
)
} # }
```
