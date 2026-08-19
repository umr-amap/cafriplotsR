# Parse Herbarium Columns (Internal Helper)

Extracts collector names and specimen numbers from herbarium_nbe_char
and herbarium_nbe_type columns. Determines link type based on column
content.

## Usage

``` r
.parse_herbarium_columns(individuals)
```

## Arguments

- individuals:

  Data frame with columns: id_n, herbarium_nbe_char, herbarium_nbe_type

## Value

Data frame with parsed information
