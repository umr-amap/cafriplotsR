# Validate Lookup Table Values

Checks that foreign key references exist in lookup tables.

## Usage

``` r
.validate_lookup_values(data, config, con)
```

## Arguments

- data:

  Data frame with schema column names

- config:

  Routing configuration

- con:

  Database connection

## Value

List with errors and warnings
