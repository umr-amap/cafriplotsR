# Fix Hierarchy Inconsistencies

Updates flat columns to match the hierarchy defined by id_parent.

## Usage

``` r
fix_hierarchy_inconsistencies(con, inconsistencies)
```

## Arguments

- con:

  Database connection

- inconsistencies:

  List of inconsistencies from check_hierarchy_consistency

## Value

Summary of fixes applied
