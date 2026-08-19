# List available database backups

Lists all backup files in the specified directory, showing their
timestamps and sizes.

## Usage

``` r
list_backups(
  backup_dir = "~/database_backups",
  database = c("all", "main", "taxa"),
  pattern = "_backup_.*\\.dump$"
)
```

## Arguments

- backup_dir:

  Directory containing backup files. Defaults to \`~/database_backups\`.

- database:

  Filter backups by database: \`"main"\`, \`"taxa"\`, or \`"all"\`.
  Default is \`"all"\`.

- pattern:

  Custom file pattern to match. Default matches standard backup naming.

## Value

A data.frame with columns: file, database, timestamp, size_mb, path

## Examples

``` r
if (FALSE) { # \dontrun{
# List all backups
backups <- list_backups()

# List only main database backups
backups <- list_backups(database = "main")

# List backups in custom directory
backups <- list_backups(backup_dir = "D:/my_backups")
} # }
```
