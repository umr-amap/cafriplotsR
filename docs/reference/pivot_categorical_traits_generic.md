# Pivot categorical trait data to wide format

Generic function to pivot categorical/character traits. Uses data.table
for performance.

## Usage

``` r
pivot_categorical_traits_generic(
  data,
  id_col,
  aggregation_mode = c("mode", "concat"),
  include_id_measures = TRUE,
  name_prefix = ""
)
```

## Arguments

- data:

  Data frame with columns: id_col, trait, traitvalue_char

- id_col:

  Name of ID column

- aggregation_mode:

  "mode" (most frequent) or "concat" (all unique values)

- include_id_measures:

  If TRUE, concatenates id_trait_measures

- name_prefix:

  Prefix to add to trait names

## Value

Tibble in wide format
