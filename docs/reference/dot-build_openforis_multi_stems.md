# Build candidate multi-stem groupings from OpenForis data

In OpenForis, multi-stem individuals are indicated by
`multi_stem == "yes"` on the first stem row, with `number_multi_stem`
giving the total number of stems. The subsequent N-1 rows (sorted by tag
within the same plot) are assumed to be the additional stems of that
individual.

## Usage

``` r
.build_openforis_multi_stems(trees)
```

## Arguments

- trees:

  Full tree data frame with columns: plot_name, tag, multi_stem,
  number_multi_stem, and optionally species_scientific_name and
  species_code.

## Value

Data frame with columns: plot_name, tag, group_tag, stem_order,
original_tax_name, idtax, flag. NULL if no multi-stem detected.

## Details

This function reconstructs those groups and flags potential problems:

- `different_idtax`: stems in the group have different taxonomy

- `tag_gap`: tags are not consecutive

- `overlapping_group`: a stem belongs to more than one group
