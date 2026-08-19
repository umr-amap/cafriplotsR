# Safely delete individual feature measurements with all related data

\*\*DANGER: This permanently deletes data from the database!\*\*

This function safely deletes individual feature measurements (records in
\`data_traits_measures\`) and their associated sub-features
(\`data_ind_measures_feat\`) in the correct order to respect foreign key
constraints: 1. Measurement features (\`data_ind_measures_feat\`) 2.
Trait measurements (\`data_traits_measures\`)

\*\*Individuals are preserved\*\* — only the feature measurements are
removed.

\*\*Safety features:\*\* - Dry-run mode to preview what will be
deleted - Shows counts of all related data - Requires explicit
confirmation (unless force = TRUE) - Uses database transactions
(rollback on error) - Detailed logging of each step

## Usage

``` r
safe_delete_individual_features(
  id_trait_measures = NULL,
  individual_ids = NULL,
  trait_ids = NULL,
  con = NULL,
  dry_run = TRUE,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- id_trait_measures:

  Integer vector. Specific measurement ID(s) to delete
  (\`id_trait_measures\` from \`data_traits_measures\`). Takes
  precedence over \`individual_ids\` / \`trait_ids\`. Typically obtained
  from `query_individual_features(..., include_measurement_ids = TRUE)`.
  Default NULL.

- individual_ids:

  Integer vector. Delete all feature measurements belonging to these
  individual ID(s) (\`id_n\` from \`data_individuals\`). Can be combined
  with \`trait_ids\`. Default NULL.

- trait_ids:

  Integer vector. Filter measurements to these trait ID(s) (\`id_trait\`
  from \`traitlist\`). Only used when \`individual_ids\` is provided.
  Default NULL (all traits).

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

# STEP 1: Extract features and pick the ones to remove
feats <- query_individual_features(
  plot_ids = 42,
  include_measurement_ids = TRUE,
  format = "long"
)
ids_to_remove <- feats$id_trait_measures[feats$trait == "dbh" & feats$traitvalue < 0]

# STEP 2: Always do a dry-run first!
safe_delete_individual_features(id_trait_measures = ids_to_remove, dry_run = TRUE)

# STEP 3: Review output, then delete if sure
safe_delete_individual_features(id_trait_measures = ids_to_remove, dry_run = FALSE)

# Alternative: delete all features of specific trait(s) for given individuals
safe_delete_individual_features(
  individual_ids = c(1001, 1002),
  trait_ids = c(5),
  dry_run = TRUE
)
} # }
```
