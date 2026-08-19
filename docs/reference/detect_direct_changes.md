# Detect changes in direct columns with visual display

Detect changes in direct columns with visual display

## Usage

``` r
detect_direct_changes(data, columns, config, con, max_display = 10)
```

## Arguments

- data:

  Tibble with new data

- columns:

  Vector of column names to check

- config:

  Configuration list from get_column_routing()

- con:

  Database connection

- max_display:

  Maximum number of changes to display per column (default: 10)

## Value

Tibble with changes or NULL if no changes
