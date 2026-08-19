# Execute standardize_observations import

Re-runs
[`standardize_observations`](https://umr-amap.github.io/cafriplotsR/reference/standardize_observations.md)
with `add_data = TRUE` for the confirmed individuals; dawkins rows
flagged `skip_existing` are dropped, mortality_risk_flag duplicates are
de-duped on (id_n, id_sub_plots, std_value).

## Usage

``` r
.execute_standardize_observations_import(
  data,
  config,
  con,
  dry_run = TRUE,
  i18n = NULL
)
```
