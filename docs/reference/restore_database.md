# Restore database from backup

Restores a PostgreSQL database from a backup file created by
\`backup_database()\`.

\*\*WARNING\*\*: This will OVERWRITE the current database! Use with
caution.

## Usage

``` r
restore_database(
  backup_file,
  database = c("main", "taxa"),
  user = NULL,
  password = NULL,
  clean = FALSE,
  confirm = TRUE,
  verbose = TRUE
)
```

## Arguments

- backup_file:

  Full path to the backup file (.dump format).

- database:

  Which database to restore to: \`"main"\` or \`"taxa"\`. Must match the
  database that was backed up.

- user:

  Database username. If NULL, uses stored credentials.

- password:

  Database password. If NULL, uses stored credentials.

- clean:

  If TRUE, drops existing database objects before restoring. Default is
  FALSE (safer).

- confirm:

  If TRUE, requires interactive confirmation before restoring. Default
  is TRUE.

- verbose:

  Logical. If TRUE, shows pg_restore output. Default is TRUE.

## Value

Logical. TRUE if restore was successful, FALSE otherwise.

## Details

This function requires PostgreSQL's \`pg_restore\` utility to be
installed and available in your PATH.

\*\*IMPORTANT\*\*: This operation will overwrite data in the target
database. Always verify you're restoring to the correct database and
have a recent backup.

## Examples

``` r
if (FALSE) { # \dontrun{
# List available backups first
backups <- list_backups()

# Restore from most recent backup
restore_database(
  backup_file = backups$path[1],
  database = "main"
)

# Restore with clean option (drops existing objects first)
restore_database(
  backup_file = "~/database_backups/plots_transects_backup_2026-02-13.dump",
  database = "main",
  clean = TRUE
)
} # }
```
