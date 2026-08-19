# Delete old database backups

Removes backup files older than a specified number of days.

## Usage

``` r
cleanup_old_backups(
  backup_dir = "~/database_backups",
  days_to_keep = 30,
  database = c("all", "main", "taxa"),
  dry_run = TRUE
)
```

## Arguments

- backup_dir:

  Directory containing backup files. Defaults to \`~/database_backups\`.

- days_to_keep:

  Number of days to keep backups. Backups older than this will be
  deleted. Default is 30 days.

- database:

  Filter by database: \`"main"\`, \`"taxa"\`, or \`"all"\`. Default is
  \`"all"\`.

- dry_run:

  If TRUE, shows what would be deleted without actually deleting.
  Default is TRUE.

## Value

Integer. Number of files deleted (or that would be deleted in dry_run
mode).

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview what would be deleted (dry run)
cleanup_old_backups(days_to_keep = 30, dry_run = TRUE)

# Actually delete old backups
cleanup_old_backups(days_to_keep = 30, dry_run = FALSE)

# Delete only main database backups older than 7 days
cleanup_old_backups(database = "main", days_to_keep = 7, dry_run = FALSE)
} # }
```
