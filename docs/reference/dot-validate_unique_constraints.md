# Validate Unique Constraints

Checks for duplicate values in columns that should be unique. For
plot_name, checks both within uploaded data AND against existing
database plots (respects row-level security - only user's accessible
plots).

## Usage

``` r
.validate_unique_constraints(data, config, con = NULL)
```

## Arguments

- data:

  Data frame with schema column names

- config:

  Routing configuration

- con:

  Database connection (optional, but needed for database uniqueness
  checks)

## Value

List of error objects
