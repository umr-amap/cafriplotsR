# Process an OpenForis census export into clean data frames

Reads the raw tree and plot xlsx files exported from OpenForis Collect,
decodes coded columns using the provided code-list CSVs, and returns a
list with three components ready for import:

- `census_metadata`: plot-level census info for
  [`add_subplot_features()`](https://umr-amap.github.io/cafriplotsR/reference/add_subplot_features.md)

- `recruits`: new individuals ready for the Import Wizard

- `measurements`: long-format trait table ready for the Feature Wizard

## Usage

``` r
process_openforis_census(
  data_dir = NULL,
  codes_dir = NULL,
  tree_file = NULL,
  plot_file = NULL,
  observation_codes = NULL,
  pom_codes = NULL,
  light_codes = NULL,
  status_codes = NULL,
  tree_file_pattern = "arbre*",
  plot_file_pattern = "plot*",
  team_leader = NULL,
  principal_investigator = NULL,
  data_manager = NULL,
  additional_people = NULL,
  census = NULL,
  specimen_prefix = NULL,
  specimen_remap_file = NULL,
  specimen_locality = NULL,
  specimen_country = NULL,
  specimen_col_month = NULL,
  specimen_col_year = NULL,
  specimen_collector = NULL,
  specimen_description_col = "stem_diameter",
  specimen_branch_position_col = "branch_position",
  recruit_state = "recruted",
  plot_name_col = "plot_plot_name",
  tag_col = "arbre",
  recruit_tag_col = "label_recrut",
  herbarium_col = "herbarium_nbe_char"
)
```

## Arguments

- data_dir:

  Path to the directory containing the OpenForis data exports (xlsx
  files). The function looks for a tree file matching
  `tree_file_pattern` and a plot file matching `plot_file_pattern`.
  Ignored if `tree_file` is provided explicitly.

- codes_dir:

  Path to the directory containing the OpenForis code-list CSVs. The
  function auto-detects files by pattern: `code_list_observation*`,
  `code_list_pom*`, `code_light*`, `code_*status*`. Ignored for any code
  file provided explicitly.

- tree_file:

  Path to the tree-level xlsx. If NULL, auto-detected from `data_dir`
  using `tree_file_pattern`.

- plot_file:

  Path to the plot-level xlsx. If NULL, auto-detected from `data_dir`
  using `plot_file_pattern`. Set to FALSE to skip.

- observation_codes:

  Path to the observation code-list CSV. If NULL, auto-detected from
  `codes_dir`. Set to FALSE to skip.

- pom_codes:

  Path to the POM observation code-list CSV. If NULL, auto-detected from
  `codes_dir`. Set to FALSE to skip.

- light_codes:

  Path to the light code-list CSV. If NULL, auto-detected from
  `codes_dir`. Set to FALSE to skip.

- status_codes:

  Path to the stem-status code-list CSV. If NULL, auto-detected from
  `codes_dir`. Set to FALSE to skip.

- tree_file_pattern:

  Glob pattern to find the tree xlsx in `data_dir` (default `"arbre*"`).

- plot_file_pattern:

  Glob pattern to find the plot xlsx in `data_dir` (default `"plot*"`).

- team_leader:

  Character. Team leader name(s).

- principal_investigator:

  Character. PI name(s).

- data_manager:

  Character. Data manager name(s).

- additional_people:

  Character. Comma-separated additional people.

- census:

  Integer. Census number. If NULL, must be provided later.

- specimen_prefix:

  Character prefix for specimen numbers (e.g. "PIRD"). If NULL, specimen
  columns are left as-is.

- specimen_remap_file:

  Path or filename of an xlsx file with two columns: the first contains
  the original specimen number (as in the tree file), the second
  contains the replacement number. If just a filename (no directory), it
  is looked up in `data_dir`. The original values are kept in a
  `specimen_number_original` column. The function stops if the new
  numbers contain duplicates. NULL (default) skips remapping.

- specimen_locality:

  Character. Locality string for specimens (e.g. "Mbalmayo, Centre").
  NULL to omit.

- specimen_country:

  Character. Country name for specimens. NULL to omit.

- specimen_col_month:

  Integer. Collection month for specimens. NULL to omit.

- specimen_col_year:

  Integer. Collection year for specimens. NULL to omit.

- specimen_collector:

  Character. Collector code for specimens (e.g. "PIRD"). Stored as
  `colnam`. NULL to omit.

- specimen_description_col:

  Column name from the tree file used to build a description string
  (default `"stem_diameter"`). Set to NULL to skip.

- specimen_branch_position_col:

  Column name from the tree file giving the origin of the collected
  branch (default `"branch_position"`). Rows with the value `"rejet"`
  get "Echantillon collecté sur un rejet" appended to the description;
  `"shade_branch"` and `"light_branch"` are ignored.

- recruit_state:

  Character. Value in the `state` column that marks recruits (default
  `"recruted"` — the OpenForis spelling).

- plot_name_col:

  Column name for plot name in tree file (default `"plot_plot_name"`).

- tag_col:

  Column name for existing-tree tag (default `"arbre"`).

- recruit_tag_col:

  Column name for recruit tag (default `"label_recrut"`).

- herbarium_col:

  Column name for the herbarium/collection number in the tree file
  (default `"herbarium_nbe_char"`). This column is used to identify
  trees with specimens. If the column is absent and no alternative name
  is provided, a warning is issued and the specimen list will be empty.

## Value

A list with components:

- census_metadata:

  Tibble with plot_name, date_year, date_month, date_day, census, and
  people columns. NULL if no plot file.

- recruits:

  Tibble of recruited individuals with wide-format measurements. NULL if
  no recruits found.

- measurements:

  Tibble in long format with columns: plot_name, tag, trait_name,
  traitvalue (numeric), traitvalue_char (character). Numeric traits are
  stem_diameter, height_of_stem_diameter and, when the tree file records
  them, position_x and position_y; columns that are absent or entirely
  empty are skipped. Ready for the Feature Wizard measurements step.

- specimens:

  Tibble of specimens ready for
  [`add_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/add_specimens.md),
  with columns: plot_name, tag, specimen_number, herbarium_nbe_char,
  colnbr, idtax_n, description, locality, country, colnam, colm, coly,
  additional_people. NULL if no specimens found.

- multi_stems:

  Tibble of candidate multi-stem groupings with columns: plot_name, tag,
  group_tag (parent tag), stem_order (position within group),
  original_tax_name, idtax, flag (validation issues). NULL if no
  multi-stem individuals detected. Review flags before uploading.

- all_stems:

  Tibble in the same wide format as `recruits` but covering every stem
  in the dataset (recruits and existing individuals alike). Useful for
  bulk imports or cross-census checks where you need all stems in one
  table.

- duplicated_stems:

  Tibble of all rows involved in duplicated `plot_name + tag`
  combinations (i.e. every row that shares a plot_name/tag pair with at
  least one other row), with columns plot_name, tag, state,
  species_scientific_name, species_code, stem_diameter, quadrat
  (whichever are present). NULL if no duplicates. A
  [`warning()`](https://rdrr.io/r/base/warning.html) is also raised when
  duplicates are found.

- summary:

  List with counts: n_plots, n_recruits, n_existing, n_measurements,
  n_specimens, n_multi_stem_groups, n_all_stems, n_duplicated_stems,
  trait_names.

## Examples

``` r
if (FALSE) { # \dontrun{
# Simplest usage — just point to the two directories:
result <- process_openforis_census(
  data_dir = "path/to/mission/plot/",
  codes_dir = "path/to/openforis/",
  team_leader = "Jane Doe",
  additional_people = "John Smith, Alice Brown",
  census = 2,
  specimen_prefix = "PIRD"
)

# Or specify files individually:
result <- process_openforis_census(
  tree_file = "path/to/arbre.xlsx",
  plot_file = "path/to/plot.xlsx",
  observation_codes = "path/to/code_list_observations.csv",
  status_codes = "path/to/code_recensus_status.csv",
  light_codes = FALSE,   # skip light decoding
  census = 2
)

# Long-format measurements — upload to Feature Wizard
head(result$measurements)

# Write to xlsx for the app
writexl::write_xlsx(result$measurements, "measurements_long.xlsx")
writexl::write_xlsx(result$recruits, "recruits.xlsx")
} # }
```
