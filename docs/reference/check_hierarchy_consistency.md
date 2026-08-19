# Check Hierarchy Consistency

Validates that flat taxonomic columns match the hierarchy defined by
id_parent. Returns taxa where the flat columns don't match their parent
entries.

## Usage

``` r
check_hierarchy_consistency(con = NULL, fix = FALSE, limit = 100)
```

## Arguments

- con:

  Database connection to taxa database

- fix:

  Logical, if TRUE attempts to fix inconsistencies (default FALSE)

- limit:

  Integer, max number of inconsistencies to return (default 100)

## Value

Data frame with inconsistent taxa, or NULL if all consistent

## Examples

``` r
if (FALSE) { # \dontrun{
con <- call.mydb.taxa()

# Check for inconsistencies
issues <- check_hierarchy_consistency(con)

# Fix inconsistencies automatically
check_hierarchy_consistency(con, fix = TRUE)
} # }
```
