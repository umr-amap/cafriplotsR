# Build the plot-level table from a raw OpenForis plot export

The `forest_type[1..n]` columns are decoded and collapsed into a single
comma-separated `forest_type` column, alongside `forest_state`.

## Usage

``` r
.prepare_openforis_new_plots(
  plots_raw,
  country_codes = NULL,
  forest_state_codes = NULL,
  forest_type_codes = NULL,
  team_leader_codes = NULL,
  country = NULL,
  province = NULL,
  method = NULL,
  data_provider = NULL,
  locality_name = NULL,
  plot_area = NULL,
  principal_investigator = NULL,
  data_manager = NULL,
  additional_people = NULL,
  census = NULL
)
```

## Arguments

- plots_raw:

  Data frame read from `plot.xlsx`.

- country_codes, forest_state_codes, forest_type_codes,
  team_leader_codes:

  Parsed code lists, or NULL to skip that decoding.

- country, province, method, data_provider, locality_name, plot_area,
  principal_investigator, data_manager:

  Constants not present in the export. NULL to omit.

- additional_people:

  Overrides the exported `add_people` column.

- census:

  Census number written to a `census` column. NULL to omit.

## Value

Data frame with one row per plot.
