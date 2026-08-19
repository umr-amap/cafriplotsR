# Apply WCVP backbone to a taxonomic result data frame

Replaces the standard internal taxonomy columns (`tax_fam`, `tax_gen`,
`tax_esp`, `tax_sp_level`, `tax_infra_level`, `tax_infra_level_auth`)
with WCVP values where a WCVP match exists. The original internal name
is preserved in `alt_taxon_name` and a `name_source` column (`"wcvp"` /
`"internal"`) is added. Taxa without a WCVP match keep their internal
values with `name_source = "internal"`.

## Usage

``` r
.apply_wcvp_backbone(data, wcvp_info, id_col = "idtax_n")
```

## Arguments

- data:

  Data frame with internal taxonomy columns.

- wcvp_info:

  Tibble returned by
  [`get_wcvp_names()`](https://umr-amap.github.io/cafriplotsR/reference/get_wcvp_names.md).

- id_col:

  Character. Name of the column in `data` that matches
  `wcvp_info$idtax_n`. Default `"idtax_n"`.

## Value

`data` with standard columns overwritten by WCVP values where available,
plus `name_source` and `alt_taxon_name`.
