# Get Column Descriptions

Returns descriptions for database columns to help users understand what
data is expected. Includes both flat table columns (hard-coded) and
feature columns (from database).

## Usage

``` r
.get_column_descriptions(con, table_type = "plots")
```

## Arguments

- con:

  Database connection

- table_type:

  Character: "plots" or "individuals"

## Value

Named list of column descriptions (and additional info for traits)
