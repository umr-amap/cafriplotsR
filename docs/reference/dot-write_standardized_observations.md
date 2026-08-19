# Upsert standardized values into data_traits_measures

Skips dawkins rows flagged as `skip_existing`. Skips mortality rows that
already exist (same id_n + id_sub_plots + traitvalue_char). Inserts the
remainder.

## Usage

``` r
.write_standardized_observations(
  out,
  mortality_trait_id,
  dawkins_trait_id,
  obs_trait_id,
  dry_run,
  con
)
```
