# Backup PostgreSQL database with timestamp

Creates a timestamped backup of the main or taxa database using
PostgreSQL's pg_dump utility. Backup files are named with format:
\`\<database\>\_backup_YYYY-MM-DD_HH-MM-SS.dump\`

## Usage

``` r
backup_database(
  backup_dir = "~/database_backups",
  database = c("main", "taxa"),
  compress = TRUE,
  user = NULL,
  password = NULL,
  verbose = TRUE,
  sslmode = "prefer",
  exclude_table_data = NULL
)
```

## Arguments

- backup_dir:

  Directory where backup files will be stored. Defaults to
  \`~/database_backups\`. Directory will be created if it doesn't exist.

- database:

  Which database to backup: \`"main"\` (plots_transects) or \`"taxa"\`
  (rainbio). Default is \`"main"\`.

- compress:

  Logical. If TRUE, creates compressed backup (smaller file size,
  slower). Default is TRUE.

- user:

  Database username. If NULL, uses stored credentials.

- password:

  Database password. If NULL, uses stored credentials.

- verbose:

  Logical. If TRUE, shows pg_dump output. Default is TRUE.

- sslmode:

  SSL mode passed to pg_dump via the PGSSLMODE environment variable.
  Common values: \`"require"\`, \`"prefer"\` (default), \`"disable"\`.
  Set to \`"require"\` for managed cloud databases (OVH, etc.) that
  enforce SSL.

- exclude_table_data:

  Character vector of table names whose data should be excluded from the
  backup (schema is still included). Useful when a table has FORCE ROW
  LEVEL SECURITY that blocks pg_dump. Example:
  \`"taxa_traits_measures"\`. The proper fix is \`ALTER TABLE \<table\>
  NO FORCE ROW LEVEL SECURITY\` on the server.

## Value

Character string with the full path to the created backup file.

## Details

This function requires PostgreSQL's \`pg_dump\` utility to be installed
and available in your PATH.

The function will: - Connect to the database to retrieve connection
parameters - Generate a timestamped filename - Execute pg_dump to create
the backup - Return the path to the created backup file

\*\*Security Note\*\*: The password is passed via the PGPASSWORD
environment variable, which is cleared immediately after use.

## Examples

``` r
if (FALSE) { # \dontrun{
# Backup main database to default location
backup_file <- backup_database()

# Backup taxa database to specific directory
backup_file <- backup_database(
  backup_dir = "D:/my_backups",
  database = "taxa"
)

# Backup without compression (faster, larger file)
backup_file <- backup_database(compress = FALSE)
} # }
```
