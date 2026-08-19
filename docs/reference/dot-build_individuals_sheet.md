# Build Individuals Sheet Data

Creates the data structure for the individuals sheet with headers,
descriptions, and example data.

## Usage

``` r
.build_individuals_sheet(column_defs, method = NULL)
```

## Arguments

- column_defs:

  Column definitions from .get_individual_columns_from_db()

- method:

  Method type (optional)

## Value

Tibble formatted for Excel export
