# Migration: Add created_by Column for Row-Level Security

This migration adds a \`created_by\` column to \`data_liste_plots\`
table that enables automatic access for plot creators. After this
migration, users will automatically have SELECT, UPDATE, DELETE access
to plots they create.

## Usage

``` r
migrate_add_created_by(con, backfill_user = "dauby", dry_run = FALSE)
```

## Arguments

- con:

  Database connection (must have admin privileges)

- backfill_user:

  Username to assign as creator for existing plots (default: "dauby")

- dry_run:

  If TRUE, only print SQL without executing (default: FALSE)

## Value

Invisible TRUE on success

## Details

This migration: 1. Adds \`created_by\` column with DEFAULT current_user
2. Backfills existing plots to a specified admin user 3. Creates global
RLS policies for creator access 4. Keeps INSERT open for all users

After migration, access works as follows: - INSERT: Anyone can insert
(policy: WITH CHECK true) - SELECT/UPDATE/DELETE own plots: Via
created_by = current_user - SELECT/UPDATE/DELETE others' plots: Via
explicit grants (define_user_policy)

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect as admin
con <- call.mydb(user = "admin", password = "xxx")

# Preview the migration
migrate_add_created_by(con, dry_run = TRUE)

# Run the migration
migrate_add_created_by(con, backfill_user = "dauby")
} # }
```
