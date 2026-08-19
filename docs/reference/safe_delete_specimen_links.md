# Safely delete individual-specimen links

\*\*DANGER: This permanently deletes data from the database!\*\*

This function safely deletes one or more rows from
\`data_link_specimens\` (the table that records which individual trees
are linked to herbarium specimens). Links can be selected by individual
ID, specimen ID, or direct link ID.

\*\*Individuals and specimens are preserved\*\* — only the link records
are removed.

\*\*Safety features:\*\* - Dry-run mode to preview what will be
deleted - Shows a summary of every link that would be removed - Requires
explicit confirmation (unless \`force = TRUE\`) - Uses database
transactions (rollback on error) - Detailed logging of each step

## Usage

``` r
safe_delete_specimen_links(
  individual_ids = NULL,
  specimen_ids = NULL,
  link_ids = NULL,
  con = NULL,
  dry_run = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- individual_ids:

  Integer vector. Delete all links involving these individual ID(s)
  (\`id_n\` from \`data_individuals\`). Default NULL.

- specimen_ids:

  Integer vector. Delete all links involving these specimen ID(s)
  (\`id_specimen\` from \`specimens\`). Default NULL.

- link_ids:

  Integer vector. Delete specific link record(s) by their primary key
  (\`id_link_specimens\` from \`data_link_specimens\`). Takes precedence
  over the other selectors when provided. Default NULL.

- con:

  Database connection. If NULL, will connect automatically.

- dry_run:

  Logical. If TRUE, shows what would be deleted without deleting.
  Default TRUE for safety.

- force:

  Logical. If TRUE, skips confirmation prompts. Default FALSE. \*\*USE
  WITH EXTREME CAUTION!\*\*

- verbose:

  Logical. Show detailed progress? Default TRUE.

## Value

List with deletion summary (invisible).

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# STEP 1: Always do a dry-run first!

# Remove all links for a given individual
safe_delete_specimen_links(individual_ids = 12345, dry_run = TRUE)

# Remove all links pointing to a given specimen
safe_delete_specimen_links(specimen_ids = 678, dry_run = TRUE)

# Remove specific link records by their PK
safe_delete_specimen_links(link_ids = c(1001, 1002), dry_run = TRUE)

# STEP 2: Review the output, then delete if sure
safe_delete_specimen_links(individual_ids = 12345, dry_run = FALSE)
} # }
```
