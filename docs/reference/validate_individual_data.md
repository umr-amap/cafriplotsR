# Validate Individual Data Before Import

Comprehensive validation of individual tree data using database rules
and checks. Returns structured results with severity levels (error vs
warning). Can interactively fix issues using fuzzy matching.

## Usage

``` r
validate_individual_data(
  individuals_data,
  features_data = NULL,
  method = NULL,
  con = NULL,
  strict = FALSE,
  interactive = TRUE,
  fix_on_fly = TRUE
)
```

## Arguments

- individuals_data:

  Data frame with individual data (required)

- features_data:

  Data frame with feature/trait data (optional)

- method:

  Method type (e.g., "1ha-IRD", "Large"). Used for method-specific
  validation.

- con:

  Database connection (optional, will create if NULL)

- strict:

  Logical: If TRUE, warnings are treated as errors (default FALSE)

- interactive:

  Logical: If TRUE, allow interactive fixing (default TRUE)

- fix_on_fly:

  Logical: If TRUE, fix issues during validation (default TRUE)

## Value

List with validation results:

- valid:

  Logical: TRUE if no errors (warnings allowed)

- errors:

  Data frame of error messages

- warnings:

  Data frame of warning messages

- summary:

  Summary statistics

- original_data:

  Original input data

- cleaned_data:

  List with individuals and features (fixes applied)

- changes_made:

  Data frame documenting changes

## Validation Checks

\*\*Individuals Sheet\*\*: - Required fields: plot_name, tag, idtax_n,
original_tax_name - Plot existence and access - Taxonomy ID existence in
taxa database - Tag uniqueness within plot - Tag numeric and valid - No
duplicate tags with existing database records - Method-specific
requirements

\*\*Features Sheet\*\* (if provided): - Linking columns present
(plot_name, tag) - Match to individuals in import - Trait value types
(numeric vs character) - Min/max value ranges per trait - Expected units

## Examples

``` r
if (FALSE) { # \dontrun{
# After column mapping
mapped <- map_individual_columns(individuals, features)

# Validate data
validation <- validate_individual_data(
  individuals_data = mapped$individuals,
  features_data = mapped$features,
  method = "1ha-IRD",
  interactive = TRUE
)

# Check results
print_validation_results(validation)

if (!validation$valid) {
  stop("Validation failed!")
}

# Use cleaned data for import
import_individual_data(
  data = validation$cleaned_data,
  validation = validation
)
} # }
```
