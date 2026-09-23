# Replace internal name columns with a backbone's names

Overwrites `tax_fam`, `tax_gen`, `tax_esp`, `tax_sp_level`,
`tax_infra_level` and `tax_infra_level_auth` (those present in `data`)
for rows linked to `backbone`. The internal name is kept in
`alt_taxon_name`. Adds `backbone_name_id`, `backbone_accepted_id` and
`name_source`; for `"wcvp"`, also `wcvp_plant_name_id` and
`wcvp_accepted_plant_name_id`, the column names used before any other
backbone existed.

## Usage

``` r
.apply_backbone(data, info, backbone, id_col = "idtax_n")
```

## Arguments

- data:

  Data frame with internal taxonomy columns.

- info:

  Tibble returned by \[get_backbone_names()\].

- backbone:

  Backbone code.

- id_col:

  Name of the column of `data` holding `idtax_n`.

## Value

`data`, same number of rows.
