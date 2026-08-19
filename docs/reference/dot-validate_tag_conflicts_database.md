# Validate Tag Conflicts with Database (Internal)

Check if any plot+tag combinations already exist in database.

## Usage

``` r
.validate_tag_conflicts_database(data, con)
```

## Arguments

- data:

  Data frame with plot_name and tag columns

- con:

  Database connection

## Value

List of warning messages
