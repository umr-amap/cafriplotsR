# Get Import Column Routing Configuration

Extends the existing get_column_routing() system with import-specific
configuration including synonym mappings and validation rules.

## Usage

``` r
get_import_column_routing(table_type = "plots", con = NULL)
```

## Arguments

- table_type:

  Character: Type of table ("plots", "individuals", etc.)

- con:

  Database connection (optional)

## Value

List with routing configuration including synonyms

## Examples

``` r
if (FALSE) { # \dontrun{
config <- get_import_column_routing("plots")
# Returns: direct_columns, subplot_features, synonyms, validation_rules
} # }
```
