# Safely delete taxa trait measurements with all related data

\*\*DANGER: This permanently deletes data from the database!\*\*

This function safely deletes taxa trait measurements and all related
data in the correct order to respect foreign key constraints: 1. Trait
measurement features (\`taxa_traits_measures_feat\`) 2. Trait
measurements (\`taxa_traits_measures\`)

\*\*Taxonomy is preserved\*\* (taxa entries in the taxonomy database are
not touched).

\*\*Safety features:\*\* - Dry-run mode to preview what will be
deleted - Shows counts of all related data - Requires explicit
confirmation (unless force = TRUE) - Uses database transactions
(rollback on error) - Detailed logging of each step

## Usage

``` r
safe_delete_taxa_traits(
  idtax = NULL,
  trait_ids = NULL,
  id_trait_measures = NULL,
  con = NULL,
  dry_run = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- idtax:

  Integer vector. Taxon ID(s) whose trait measurements will be deleted.
  If NULL, \`trait_ids\` or \`id_trait_measures\` must be provided.

- trait_ids:

  Integer vector. Trait ID(s) to filter measurements (id_trait from
  traitlist). Can be combined with \`idtax\` to delete only specific
  traits for specific taxa. Default NULL (all traits).

- id_trait_measures:

  Integer vector. Specific measurement ID(s) to delete
  (id_trait_measures from taxa_traits_measures). Takes precedence over
  \`idtax\`/\`trait_ids\`. Default NULL.

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

List with deletion summary (invisible)

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb()

# STEP 1: Always do a dry-run first!

# Preview deletion of all trait measurements for a taxon
safe_delete_taxa_traits(idtax = 12345, dry_run = TRUE)

# Preview deletion of specific traits for a taxon
safe_delete_taxa_traits(idtax = 12345, trait_ids = c(1, 2), dry_run = TRUE)

# Preview deletion of specific measurement records
safe_delete_taxa_traits(id_trait_measures = c(999, 1000), dry_run = TRUE)

# STEP 2: Review the output, then delete if sure
safe_delete_taxa_traits(idtax = 12345, dry_run = FALSE)
} # }
```
