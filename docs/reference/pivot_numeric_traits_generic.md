# Pivot numeric trait data to wide format with statistics

Generic function to pivot numeric traits and calculate statistics (mean,
sd, n). Uses data.table for performance.

## Usage

``` r
pivot_numeric_traits_generic(
  data,
  id_col,
  include_stats = TRUE,
  include_id_measures = TRUE,
  name_prefix = ""
)
```

## Arguments

- data:

  Data frame with columns: id_col, trait, traitvalue

- id_col:

  Name of ID column (e.g., "idtax", "id_data_individuals")

- include_stats:

  If TRUE, calculates mean, sd, and n for each trait

- include_id_measures:

  If TRUE, concatenates id_trait_measures

- name_prefix:

  Prefix to add to trait names

## Value

Tibble in wide format
