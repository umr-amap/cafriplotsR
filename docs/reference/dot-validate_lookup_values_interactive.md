# Validate Lookup Values with Interactive Fixing

Extended version of .validate_lookup_values() that can interactively fix
mismatches using fuzzy matching via resolve_multiple_values().

## Usage

``` r
.validate_lookup_values_interactive(
  data,
  config,
  con,
  interactive = TRUE,
  fix_on_fly = TRUE
)
```

## Arguments

- data:

  Data frame with schema column names

- config:

  Routing configuration

- con:

  Database connection

- interactive:

  Logical: Allow interactive fixing

- fix_on_fly:

  Logical: Apply fixes during validation

## Value

List with:

- errors:

  List of error objects

- warnings:

  List of warning objects

- cleaned_data:

  Data with fixes applied

- changes_made:

  Data frame documenting changes
