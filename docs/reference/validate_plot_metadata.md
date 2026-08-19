# Validate Plot Metadata Before Import

Comprehensive validation of plot metadata using database rules and
lookup tables. Returns structured results with severity levels (error vs
warning). Can interactively fix lookup mismatches using fuzzy matching.
Also checks for potential duplicate plots by matching method, country,
and coordinates.

## Usage

``` r
validate_plot_metadata(
  data,
  column_mappings,
  config,
  con = NULL,
  strict = FALSE,
  interactive = TRUE,
  fix_on_fly = TRUE
)
```

## Arguments

- data:

  Data frame containing plot metadata to validate

- column_mappings:

  Named list mapping user columns to schema columns (from
  map_user_columns())

- config:

  Routing configuration from get_import_column_routing()

- con:

  Database connection (optional, will create if NULL)

- strict:

  Logical: If TRUE, warnings are treated as errors (default FALSE)

- interactive:

  Logical: If TRUE, allow interactive fixing of lookup mismatches
  (default TRUE)

- fix_on_fly:

  Logical: If TRUE, fix issues during validation (default TRUE)

## Value

List with validation results:

- valid:

  Logical: TRUE if no errors (warnings allowed)

- errors:

  Data frame of error messages

- warnings:

  Data frame of warning messages (includes duplicate plot warnings)

- summary:

  Summary statistics

- original_data:

  Original input data (unchanged)

- cleaned_data:

  Data with interactive fixes applied (if any)

- changes_made:

  Data frame documenting what was changed

## Examples

``` r
if (FALSE) { # \dontrun{
# Map columns first
config <- get_import_column_routing("plots")
mapping_result <- map_user_columns(my_data, config)

# Validate data with interactive fixing (default)
validation <- validate_plot_metadata(
  data = my_data,
  column_mappings = mapping_result$mappings,
  config = config,
  interactive = TRUE,  # Allow interactive fixing
  fix_on_fly = TRUE    # Fix during validation
)

# Check results
print_validation_results(validation)

if (!validation$valid) {
  stop("Data validation failed!")
}

# Use cleaned data for import
result <- import_plot_metadata(
  data = validation$cleaned_data,  # Use cleaned version!
  column_mappings = mapping_result$mappings,
  validation = validation,
  config = config
)
} # }
```
