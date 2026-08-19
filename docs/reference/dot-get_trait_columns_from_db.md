# Get Trait Column Definitions from Database

Retrieves trait definitions from traits_list() for the features
template.

## Usage

``` r
.get_trait_columns_from_db(con, common_traits_only = TRUE)
```

## Arguments

- con:

  Database connection

- common_traits_only:

  Logical. If TRUE, only includes most common traits. If FALSE, includes
  all available traits.

## Value

Tibble with trait definitions
