# Get Link Types from Lookup Table

Returns the available link types from the linktypelist lookup table.

## Usage

``` r
get_linktypes(con = NULL)
```

## Arguments

- con:

  Database connection. If NULL, calls call.mydb()

## Value

Tibble with id_linktype, linktype, description, priority columns
