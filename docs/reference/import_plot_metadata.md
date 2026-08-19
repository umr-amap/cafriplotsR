# Import Plot Metadata with Transaction Support

Imports plot metadata into the database using transactions for safety.
Reuses existing .link_table() and .link_colnam() for interactive
matching. Supports dry-run mode for preview without committing.

## Usage

``` r
import_plot_metadata(
  data,
  column_mappings,
  validation,
  config,
  con = NULL,
  dry_run = FALSE,
  interactive = TRUE,
  progress = TRUE
)
```

## Arguments

- data:

  Data frame containing plot metadata

- column_mappings:

  Named list mapping user columns to schema columns (from
  map_user_columns())

- validation:

  Validation result from validate_plot_metadata()

- config:

  Routing configuration from get_import_column_routing()

- con:

  Database connection (optional, will create if NULL)

- dry_run:

  Logical: If TRUE, preview changes without committing (default FALSE)

- interactive:

  Logical: If TRUE, use interactive prompts for matching (default TRUE)

- progress:

  Logical: If TRUE, show progress messages (default TRUE)

## Value

List with import results:

- success:

  Logical: TRUE if import succeeded

- plot_names:

  Vector of plot_name values imported

- n_plots:

  Number of plots imported

- username:

  Username who performed import

- admin_code:

  R code for admin to grant access

- dry_run:

  Was this a dry-run?

- message:

  Summary message

## Details

\*\*IMPORTANT\*\*: Due to row-level security, you won't have access to
imported plots until an admin grants permission. The function returns R
code that admin needs to run.

## Examples

``` r
if (FALSE) { # \dontrun{
# Complete workflow
config <- get_import_column_routing("plots")
mapping <- map_user_columns(my_data, config)
validation <- validate_plot_metadata(my_data, mapping$mappings, config)

if (!validation$valid) {
  stop("Fix validation errors first!")
}

# Dry run first
preview <- import_plot_metadata(
  data = my_data,
  column_mappings = mapping$mappings,
  validation = validation,
  config = config,
  dry_run = TRUE
)

# Actual import
result <- import_plot_metadata(
  data = my_data,
  column_mappings = mapping$mappings,
  validation = validation,
  config = config,
  dry_run = FALSE
)

# Send admin code to database administrator
cat(result$admin_code)
# Or save to file
writeLines(result$admin_code, "admin_access_request.R")
} # }
```
