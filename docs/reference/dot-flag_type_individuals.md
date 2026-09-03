# Flag the single individual each herbarium specimen was collected on

The OpenForis forms write the voucher number on every stem
field-identified as the species of that voucher, so one specimen number
can appear on many individuals. Only one of them is the tree the
specimen was actually taken from, and that is what `herbarium_nbe_type`
records: one row per specimen. Among the individuals sharing a voucher,
the one flagged as collected wins, then the one carrying a collection
number, ties going to the first row — the caller sorts by plot name then
tag beforehand.

## Usage

``` r
.flag_type_individuals(voucher, specimen_number = NULL, collected_flag = NULL)
```

## Arguments

- voucher:

  Character vector of voucher numbers, prefix already applied.

- specimen_number:

  Collection numbers in the same order, or NULL.

- collected_flag:

  The `any_voucher` column in the same order (1 = this stem was
  collected), or NULL when the form has none.

## Value

Character vector as long as `voucher`, holding the voucher on the
collected individual and `NA` on every other individual.
