# Process an OpenForis new-plot export into clean data frames

Reads the raw `plot.xlsx` and `tree_list.xlsx` files exported from
OpenForis Collect for a newly established plot, decodes the coded
columns using the code-list CSVs, and returns a list of clean tables
ready for import. Unlike
[`process_openforis_census`](https://umr-amap.github.io/cafriplotsR/reference/process_openforis_census.md),
every stem is a new individual — there is no recruit/existing split.

## Usage

``` r
process_openforis_new_plot(
  data_dir = NULL,
  codes_dir = NULL,
  tree_file = NULL,
  plot_file = NULL,
  tree_file_pattern = "tree_list*",
  plot_file_pattern = "plot*",
  observation_codes = NULL,
  pom_codes = NULL,
  light_codes = NULL,
  pheno_codes = NULL,
  quadrat_codes = NULL,
  morpho_codes = NULL,
  forest_state_codes = NULL,
  forest_type_codes = NULL,
  country_codes = NULL,
  team_leader_codes = NULL,
  method = NULL,
  province = NULL,
  locality_name = NULL,
  plot_area = NULL,
  country = NULL,
  data_provider = NULL,
  principal_investigator = NULL,
  data_manager = NULL,
  additional_people = NULL,
  census = 1,
  specimen_prefix = NULL,
  specimen_locality = NULL,
  specimen_country = NULL,
  specimen_col_month = NULL,
  specimen_col_year = NULL,
  specimen_collector = NULL,
  specimen_description_col = "stem_diameter",
  plot_name_col = "plot_plot_name",
  tag_col = "tag"
)
```

## Arguments

- data_dir:

  Path to the directory containing the OpenForis xlsx exports. The
  function looks for a tree file matching `tree_file_pattern` and a plot
  file matching `plot_file_pattern`. Ignored if `tree_file` is provided
  explicitly.

- codes_dir:

  Path to the directory containing the OpenForis code-list CSVs. Files
  are auto-detected by pattern: `code_observations*` (or
  `code_list_observation*`), `code_pom*`, `code_light*`, `code_pheno*`,
  `code_quadrat*`, `code_morpho*`, `code_forest_state*`,
  `code_forest_type*`, `code_country*`, `code_list_team_leader*`.
  Ignored for any code file given explicitly.

- tree_file:

  Path to `tree_list.xlsx`. If NULL, auto-detected from `data_dir`.

- plot_file:

  Path to `plot.xlsx`. If NULL, auto-detected from `data_dir`. Set to
  FALSE to skip (no `plots` output).

- tree_file_pattern:

  Glob pattern for the tree xlsx (default `"tree_list*"`).

- plot_file_pattern:

  Glob pattern for the plot xlsx (default `"plot*"`).

- observation_codes, pom_codes, light_codes, pheno_codes, quadrat_codes,
  morpho_codes:

  Paths to the individual-level code-list CSVs. NULL to auto-detect from
  `codes_dir`, FALSE to skip that decoding.

- forest_state_codes, forest_type_codes, country_codes,
  team_leader_codes:

  Paths to the plot-level code-list CSVs. NULL to auto-detect from
  `codes_dir`, FALSE to skip that decoding.

- method:

  Character. Plot method (e.g. `"1ha-IRD"`). Not present in the
  OpenForis export — supply it here. Required by the Import Wizard, so a
  warning is raised when it is missing. NULL to omit.

- province:

  Character. Province/region. NULL to omit.

- locality_name:

  Character. Locality or site name. Not present in the OpenForis export
  — supply it here. NULL to omit.

- plot_area:

  Numeric. Plot area in hectares. NULL to omit.

- country:

  Character. Overrides the country decoded from the export. NULL keeps
  the decoded value.

- data_provider:

  Character. Data provider (e.g. `"IRD"`).

- principal_investigator:

  Character. PI name(s).

- data_manager:

  Character. Data manager name(s).

- additional_people:

  Character. Overrides the `add_people` column from the export. NULL
  keeps the exported value. When given it is also written to the
  `additional_people` column of the specimen table, and feeds
  `additional_collector` wherever the plot file cannot supply a team (no
  plot file read, or a plot name missing from it).

- census:

  Integer. Census number, written to `plots$census` and to
  `individuals_wide$census_id` (default `1` — a newly established plot).

- specimen_prefix:

  Character prefix for specimen numbers (e.g. `"PIRD"`). NULL leaves the
  numbers as-is.

- specimen_locality:

  Character. Locality string for specimens.

- specimen_country:

  Character. Country for specimens. If NULL, falls back to the plot
  country.

- specimen_col_month, specimen_col_year:

  Integer. Collection month/year. If NULL, taken from the plot file
  dates when these are unambiguous.

- specimen_collector:

  Character. Collector code stored as `colnam`. If NULL, taken from the
  `colnam` column of the plot file when it holds a single value.

- specimen_description_col:

  Column used to build the specimen description (default
  `"stem_diameter"`). NULL to skip.

- plot_name_col:

  Column name for plot name in the tree file (default
  `"plot_plot_name"`).

- tag_col:

  Column name for the tree tag (default `"tag"`).

## Value

A list with components:

- plots:

  Tibble, one row per plot, ready to upload to the Import Wizard as
  inventory metadata: plot_name, country, province, method,
  locality_name, plot_area, data_provider, team_leader,
  principal_investigator, data_manager, additional_people,
  identified_by, forest_state, forest_type, date_y, date_m, date_d,
  census. A plot can carry several forest types; they are decoded and
  collapsed into a single comma-separated `forest_type` value. NULL if
  no plot file.

- individuals:

  Tibble of every stem: plot_name, tag, quadrat, original_tax_name,
  idtax_n, tax_appendix, herbarium_nbe_char, herbarium_nbe_type,
  position_x, position_y, multi_stem, number_multi_stem, multi_tiges_id
  (tag of the main stem, NA for the main stem itself). `idtax_n` is
  copied from the OpenForis `species_code` without being checked against
  the taxonomic backbone — see the note below.

- individuals_wide:

  The same stems with one column per trait and a `census_id` column —
  the flat table the Import Wizard expects for individual trees. Traits
  recorded several times for one stem (`observation`,
  `observation_flag`) are joined with "; ".

- measurements:

  Tibble in long format (plot_name, tag, trait_name, traitvalue,
  traitvalue_char) covering stem_diameter, height_of_stem_diameter,
  tree_height, position_x, position_y, light, observation (observations,
  phenology and free-text comments) and pom_observation. Stems with no
  value for a given trait produce no row.

- specimens:

  Tibble ready for
  [`add_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/add_specimens.md),
  one row per unique voucher (not one per individual), or NULL. The
  `additional_collector` column lists the collecting team of the plot —
  its team leader plus the additional people recorded in `plot.xlsx`;
  `additional_people` holds the argument of the same name when one was
  given.

- multi_stems:

  Tibble of candidate multi-stem groupings with a `flag` column to
  review, or NULL.

- duplicated_stems:

  Tibble of rows sharing a plot_name + tag pair, or NULL. A
  [`warning()`](https://rdrr.io/r/base/warning.html) is also raised.

- summary:

  List of counts.

## Details

Plot coordinates are \*\*not\*\* produced: they are not part of the
OpenForis raw export and must be added separately (e.g. with
[`add_plot_coordinates()`](https://umr-amap.github.io/cafriplotsR/reference/add_plot_coordinates.md)).

## Taxonomy

The OpenForis `species_code` is written to `idtax_n` as-is, on the
assumption that the form was built against the same taxonomic backbone
as the database. Nothing in this function verifies that, while the
Import Wizard requires a valid, non-empty `idtax_n` for every individual
— so run
[`launch_taxonomic_match_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md)
on the result before importing. A warning is raised as a reminder.

## See also

[`process_openforis_census`](https://umr-amap.github.io/cafriplotsR/reference/process_openforis_census.md)
for re-measurement exports;
[`export_openforis_for_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/export_openforis_for_wizard.md)
to write the upload files.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- process_openforis_new_plot(
  data_dir  = "path/to/mission/plots/",
  codes_dir = "path/to/openforis/",
  method = "1ha-IRD",
  province = "Centre",
  locality_name = "Mbalmayo",
  plot_area = 1,
  data_provider = "IRD",
  principal_investigator = "Jane Doe",
  data_manager = "John Smith",
  specimen_prefix = "PIRD"
)

# One file per Import Wizard upload
export_openforis_for_wizard(result, dir = "to_import")

launch_import_wizard()
} # }
```
