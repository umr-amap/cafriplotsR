# Get Primary Specimen for Individuals

Returns the primary (highest priority) specimen link for each
individual. Uses link type priority: type_individual (100) \>
referenced_individual (50). If same priority, uses most recent
determination date.

## Usage

``` r
get_primary_specimen_link(id_ind = NULL, con = NULL)
```

## Arguments

- id_ind:

  Integer vector of individual IDs

- con:

  Database connection. If NULL, calls call.mydb()

## Value

Tibble with one row per individual (primary specimen only)
