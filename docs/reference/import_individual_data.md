# Import Individual Data with Transaction Support

Imports individual tree data into the database using transactions for
safety. Inserts into data_individuals table and optionally
data_ind_measures_feat for traits. Supports dry-run mode for preview
without committing.

## Usage

``` r
import_individual_data(
  individuals_data,
  features_data = NULL,
  validation = NULL,
  method = NULL,
  con = NULL,
  dry_run = FALSE,
  progress = TRUE,
  ask_confirmation = TRUE
)
```

## Arguments

- individuals_data:

  Data frame containing individual data (required)

- features_data:

  Data frame containing trait/feature data (optional)

- validation:

  Validation result from validate_individual_data() (optional but
  recommended)

- method:

  Method type (e.g., "1ha-IRD")

- con:

  Database connection (optional, will create if NULL)

- dry_run:

  Logical: If TRUE, preview changes without committing (default FALSE)

- progress:

  Logical: If TRUE, show progress messages (default TRUE)

- ask_confirmation:

  Logical: If TRUE, ask user to confirm before importing (default TRUE)

## Value

List with import results:

- success:

  Logical: TRUE if import succeeded

- n_individuals:

  Number of individuals imported

- n_features:

  Number of feature records imported

- n_stem_grouping:

  Number of stems whose `stem_grouping` was set from the
  `multi_tiges_id` column

- plot_names:

  Unique plots affected

- username:

  Username who performed import

- dry_run:

  Was this a dry-run?

- message:

  Summary message

## Pre-requisites

1\. \*\*Taxonomy must be standardized\*\* - Use
\`match_taxonomic_names()\` or Shiny app 2. \*\*Plots must exist\*\* -
Import plots first if needed 3. \*\*Column mapping\*\* - Use
\`map_individual_columns()\` 4. \*\*Validation passed\*\* - Use
\`validate_individual_data()\`

## Database Tables

\- \*\*data_individuals\*\*: Stores core individual data -
\*\*data_ind_measures_feat\*\*: Stores trait measurements

## Examples

``` r
if (FALSE) { # \dontrun{
# Complete workflow
# 1. Map columns
mapped <- map_individual_columns(individuals, features)

# 2. Validate
validation <- validate_individual_data(
  individuals_data = mapped$individuals,
  features_data = mapped$features,
  method = "1ha-IRD"
)

if (!validation$valid) {
  stop("Fix validation errors first!")
}

# 3. Dry run
preview <- import_individual_data(
  individuals_data = validation$cleaned_data$individuals,
  features_data = validation$cleaned_data$features,
  validation = validation,
  dry_run = TRUE
)

# 4. Actual import
result <- import_individual_data(
  individuals_data = validation$cleaned_data$individuals,
  features_data = validation$cleaned_data$features,
  validation = validation,
  dry_run = FALSE
)
} # }
```
