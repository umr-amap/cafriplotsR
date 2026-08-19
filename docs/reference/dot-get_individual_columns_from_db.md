# Get Individual Column Definitions from Database

Retrieves column definitions for the data_individuals table, including
data types, constraints, and method-specific requirements.

## Usage

``` r
.get_individual_columns_from_db(con, method = NULL)
```

## Arguments

- con:

  Database connection

- method:

  Optional method filter

## Value

Tibble with column definitions
