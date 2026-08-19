# Prepare Subplot Features for Import (Internal Helper)

Prepares data for each feature type, handling: - Plot name → plot ID
linking - People name → id_table_colnam linking (for people features) -
Data formatting for add_subplot_features()

## Usage

``` r
.prepare_subplot_features(
  data,
  plot_id_column,
  plot_id_type,
  column_mappings,
  con,
  interactive,
  dry_run,
  verbose
)
```
