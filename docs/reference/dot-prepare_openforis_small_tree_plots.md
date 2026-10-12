# Build the plot-level table, one row per small-tree plot

Reuses
[`.prepare_openforis_new_plots()`](https://umr-amap.github.io/cafriplotsR/reference/dot-prepare_openforis_new_plots.md)
for everything the two forms share — country, team, forest state and
type, dates — one row per quadrat, then pools the quadrats of each
parent plot into its small-tree plot with
[`.collapse_small_tree_plots()`](https://umr-amap.github.io/cafriplotsR/reference/dot-collapse_small_tree_plots.md).

## Usage

``` r
.prepare_openforis_small_tree_plots(
  quadrats,
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

- quadrats:

  Quadrat units from
  [`.prepare_openforis_small_tree_quadrats()`](https://umr-amap.github.io/cafriplotsR/reference/dot-prepare_openforis_small_tree_quadrats.md).

- plots_raw:

  Data frame read from `plot.xlsx`.

- forest_state_codes, forest_type_codes, team_leader_codes,
  country_codes:

  Parsed code lists, or NULL to skip that decoding.

- country, province, method, data_provider, locality_name,
  principal_investigator, data_manager:

  Constants not present in the export. NULL to omit.

- plot_area:

  Area of one quadrat in hectares, multiplied by the number of quadrats
  of each plot. NULL to omit.

- additional_people:

  Overrides the exported `add_people` column.

- census:

  Census number written to a `census` column. NULL to omit.

## Value

Data frame with one row per small-tree plot.

## Details

The quadrat units carry `row_id`, the row of `plots_raw` they came from,
so the join back is positional and unaffected by the repeated plot names
that make a quadrat export what it is.
