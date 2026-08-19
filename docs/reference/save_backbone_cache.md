# Save backbone data to cache

Saves the processed taxonomic backbone data to cache along with
metadata. Compresses data for efficient storage.

## Usage

``` r
save_backbone_cache(backbone_data)
```

## Arguments

- backbone_data:

  Data frame or tibble with processed backbone data. Must include
  required columns: idtax_n, idtax_good_n, tax_fam, tax_gen, tax_esp,
  tax_sp_level, tax_gen_level, tax_fam_level, tax_class_level

## Value

Logical, TRUE on successful save, FALSE on error
