# Process an OpenForis small-tree export into clean data frames

Reads the raw `plot.xlsx` and `tree_list.xlsx` files exported from
OpenForis Collect for a small-tree inventory — stems below the 10 cm
diameter threshold, censused inside a handful of 20 x 20 m quadrats of
an already established 1-ha plot — and returns clean tables ready for
import.

## Usage

``` r
process_openforis_small_trees(
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
  subquadrat_codes = NULL,
  morpho_codes = NULL,
  forest_state_codes = NULL,
  forest_type_codes = NULL,
  country_codes = NULL,
  team_leader_codes = NULL,
  method = NULL,
  province = NULL,
  locality_name = NULL,
  quadrat_area = 0.04,
  country = NULL,
  data_provider = NULL,
  principal_investigator = NULL,
  data_manager = NULL,
  additional_people = NULL,
  census = 1,
  plot_name_digits = 3L,
  plot_name_map = NULL,
  quadrat_sep = "_",
  dbh_max = 10,
  specimen_prefix = NULL,
  specimen_remap_file = NULL,
  specimen_locality = NULL,
  specimen_country = NULL,
  specimen_col_month = NULL,
  specimen_col_year = NULL,
  specimen_collector = NULL,
  specimen_description_col = "stem_diameter",
  plot_name_col = "plot_plot_name_old",
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
  `code_quadrat*`, `code_subquadrat*` (or `code_sub_quadrat*`),
  `code_morpho*`, `code_forest_state*`, `code_forest_type*`,
  `code_country*`, `code_list_team_leader*`. Ignored for any code file
  given explicitly.

- tree_file:

  Path to `tree_list.xlsx`. If NULL, auto-detected from `data_dir`.

- plot_file:

  Path to `plot.xlsx`. If NULL, auto-detected from `data_dir`. Unlike
  the other pre-processors this file is \*\*required\*\* — it carries
  the quadrat of each unit and the `firsttag` that assigns stems to it.

- tree_file_pattern:

  Glob pattern for the tree xlsx (default `"tree_list*"`).

- plot_file_pattern:

  Glob pattern for the plot xlsx (default `"plot*"`).

- observation_codes, pom_codes, light_codes, pheno_codes, morpho_codes:

  Paths to the individual-level code-list CSVs. NULL to auto-detect from
  `codes_dir`, FALSE to skip that decoding.

- quadrat_codes:

  Path to the code list of the 20 x 20 m quadrats of the parent plot
  (`code_quadrat.csv`: codes 1-25, labels `"0_0"`, `"20_20"`, ...).
  Decodes the `quadrat` column of `plot.xlsx` and so names the plots
  this function creates. NULL to auto-detect, FALSE to keep the raw
  codes.

- subquadrat_codes:

  Path to the code list of the 10 x 10 m sub-quadrats
  (`code_subquadrat_smalltree.csv`: codes 1-4, labels A-D). Decodes the
  `quadrat` column of `tree_list.xlsx` into the `quadrat` feature of
  each individual. NULL to auto-detect, FALSE to keep the raw codes.

- forest_state_codes, forest_type_codes, country_codes,
  team_leader_codes:

  Paths to the plot-level code-list CSVs. NULL to auto-detect from
  `codes_dir`, FALSE to skip that decoding.

- method:

  Character. Plot method for the quadrats. Not present in the OpenForis
  export — supply it here. Required by the Import Wizard, so a warning
  is raised when it is missing. NULL to omit.

- province:

  Character. Province/region. NULL to omit.

- locality_name:

  Character. Locality or site name. Not present in the OpenForis export
  — supply it here. NULL to omit.

- quadrat_area:

  Numeric. Area of one quadrat in hectares, written to
  `plots$plot_area`. Default `0.04` (a 20 x 20 m quadrat). NULL to omit.

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
  keeps the exported value.

- census:

  Integer. Census number, written to `plots$census` and to
  `individuals_wide$census_id` (default `1` — a small-tree inventory is
  a first census even though the parent plot is older).

- plot_name_digits:

  Integer. Number of digits the numeric suffix of a plot name is
  zero-padded to, so that the two- and three-digit spellings found in
  the raw files (`"mbalmayo01"`, `"mbalmayo010"`) resolve to the one the
  database uses (`"mbalmayo001"`, `"mbalmayo010"`). Default `3`, which
  is the convention the OpenForis form itself states for this field —
  *"using the following format: Mbalmay005 or Somalomo010"* — and not
  merely what the exports happen to contain. NULL disables padding.

- plot_name_map:

  Named character vector of explicit plot-name replacements, applied to
  the raw value before padding and winning over it (e.g.
  `c("Mbalmayo-09" = "mbalmayo009")`). NULL for none.

- quadrat_sep:

  Character inserted between the parent plot name and the quadrat label
  (default `"_"`).

- dbh_max:

  Numeric. Diameter above which a stem is reported as outside the
  small-tree protocol (default `10`). NULL to skip the check.

- specimen_prefix:

  Character prefix for specimen numbers (e.g. `"PIRD"`). NULL leaves the
  numbers as-is. A prefix the field team already typed into the form is
  not repeated — the OpenForis `specimen_name` is calculated as `colnam`
  plus the number, so it usually arrives prefixed already, and
  inconsistently cased.

- specimen_remap_file:

  Path or filename of an xlsx whose first column holds the specimen
  number as recorded and second column its replacement; further columns
  are ignored. A bare filename is looked up in `data_dir`. Both
  `specimen_number` and `herbarium_nbe_char` are substituted and the
  values as recorded kept in `specimen_number_original` and
  `herbarium_nbe_char_original`. Numbers are matched as written and
  then, for whatever is left, on their digits alone with the prefix kept
  — so a table keyed on bare numbers reaches `"Pird 1"` as well as `1`.
  The function stops if either column of the table repeats a number.
  NULL (default) skips remapping.

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

  Column of the tree file naming the parent plot (default
  `"plot_plot_name_old"`). It is the raw field entry and, once
  normalised, resolves the parent plot more reliably than the tidied
  `plot_plot_name`, which mixes two- and three-digit spellings.

- tag_col:

  Column name for the tree tag (default `"tag"`).

## Value

A list with components:

- plots:

  Tibble, one row per sampled quadrat, ready to upload to the Import
  Wizard as inventory metadata: plot_name (`<parent>_<quadrat>`),
  parent_plot_name, quadrat, country, province, method, locality_name,
  plot_area, data_provider, team_leader, principal_investigator,
  data_manager, additional_people, identified_by, forest_state,
  forest_type, date_y, date_m, date_d, census.

- quadrats:

  Tibble, the same units with the numbers used to assign stems to them —
  firsttag, the tag range actually observed, the stem count, and the raw
  `plot_plot_name_old` spellings that fell in the range. A review table,
  not an upload.

- individuals:

  Tibble of every stem: plot_name (the quadrat), parent_plot_name, tag,
  quadrat (the 10 x 10 m sub-quadrat A-D), original_tax_name, idtax_n,
  tax_appendix, herbarium_nbe_char, herbarium_nbe_type, position_x,
  position_y, multi_stem, number_multi_stem, multi_tiges_id (tag of the
  main stem, NA for the main stem itself). The two position columns are
  part of the form but the small-tree protocol does not map stems, so in
  practice they arrive empty. `herbarium_nbe_char` is repeated on every
  individual identified as the species of a voucher, while
  `herbarium_nbe_type` names each specimen once, on the individual it
  was collected from. `idtax_n` is copied from the OpenForis
  `species_code` without being checked against the taxonomic backbone —
  see the note below.

- individuals_wide:

  The same stems with one column per trait and a `census_id` column —
  the flat table the Import Wizard expects.

- measurements:

  Tibble in long format (plot_name, tag, trait_name, traitvalue,
  traitvalue_char) covering stem_diameter, height_of_stem_diameter,
  light, observation (observations, phenology and free-text comments)
  and pom_observation. Stems with no value for a given trait produce no
  row.

- specimens:

  Tibble ready for
  [`add_specimens()`](https://umr-amap.github.io/cafriplotsR/reference/add_specimens.md),
  one row per unique voucher, or NULL.

- multi_stems:

  Tibble of candidate multi-stem groupings with a `flag` column to
  review, or NULL.

- name_mismatches:

  Tibble of stems whose `plot_plot_name_old` spelling puts them in a
  different quadrat than their tag does, or NULL. The tag wins; these
  rows are for review.

- unassigned_stems:

  Tibble of stems that could not be placed in any quadrat — an unknown
  parent plot, a missing tag, or a tag below every `firsttag` of its
  plot — or NULL. A [`warning()`](https://rdrr.io/r/base/warning.html)
  is also raised. These stems are excluded from every other table.

- duplicated_stems:

  Tibble of rows sharing a quadrat + tag pair, or NULL. A
  [`warning()`](https://rdrr.io/r/base/warning.html) is also raised.

- oversized_stems:

  Tibble of stems at or above `dbh_max`, or NULL. They belong to the
  large-stem census of the parent plot, not here.

- summary:

  List of counts.

## Details

Each sampled quadrat becomes \*\*its own plot\*\*, named
`<parent plot>_<quadrat label>` (e.g. `"mbalmayo001_20_20"`) with an
area of `quadrat_area` hectares. See the section below for why.

Plot coordinates are \*\*not\*\* produced: they are not part of the
OpenForis raw export and must be added separately (e.g. with
[`add_plot_coordinates()`](https://umr-amap.github.io/cafriplotsR/reference/add_plot_coordinates.md)).

## Why one plot per quadrat

The small trees are tagged 1..N within their parent plot, and those
numbers collide head-on with the tags the parent plot already uses for
its large stems — `mbalmayo001` holds tags 1-445 for D \>= 10 cm and the
small trees restart at 1. A repeated plot + tag pair is treated as a
data defect everywhere else in this package (see
[`split_census_table`](https://umr-amap.github.io/cafriplotsR/reference/split_census_table.md)),
and once recorded it cannot be undone, so the small trees cannot be
filed under the parent plot without mangling their field tags.

Registering each quadrat as a plot of its own avoids that: the
`firsttag` column makes the tag ranges of the quadrats disjoint, so
every tag is kept exactly as written in the field. It also stores the
right area for a density per hectare, and keeps the date, forest type
and field team that `plot.xlsx` records \*\*per quadrat\*\* rather than
collapsing them. The link back is kept in the `parent_plot_name` column.

## How stems are assigned to quadrats

`tree_list.xlsx` does not name the quadrat a stem sits in. Its
`plot_plot_name` column collapses the quadrats of a plot into one value,
and `plot_plot_name_old` distinguishes them only by a trailing
underscore typed by hand — a convention the field teams do not always
follow.

The reliable key is `firsttag`: `plot.xlsx` records, for every quadrat,
the tag its numbering started at. Stems are therefore assigned by tag
range — a stem belongs to the quadrat with the largest `firsttag` not
above its tag. The `plot_plot_name_old` grouping is used only as a
cross-check, and any disagreement is reported in `name_mismatches`
rather than acted on.

## Taxonomy

The OpenForis `species_code` is written to `idtax_n` as-is, on the
assumption that the form was built against the same taxonomic backbone
as the database. Nothing in this function verifies that, while the
Import Wizard requires a valid, non-empty `idtax_n` for every individual
— so run
[`launch_taxonomic_match_app`](https://umr-amap.github.io/cafriplotsR/reference/launch_taxonomic_match_app.md)
on the result before importing. A warning is raised as a reminder.

## See also

[`process_openforis_new_plot`](https://umr-amap.github.io/cafriplotsR/reference/process_openforis_new_plot.md)
for a newly established 1-ha plot,
[`process_openforis_census`](https://umr-amap.github.io/cafriplotsR/reference/process_openforis_census.md)
for re-measurement exports;
[`export_openforis_for_wizard()`](https://umr-amap.github.io/cafriplotsR/reference/export_openforis_for_wizard.md)
to write the upload files.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- process_openforis_small_trees(
  data_dir  = "path/to/mission/plot/small_tree/",
  codes_dir = "path/to/openforis/",
  method = "small-tree quadrat",
  province = "Centre",
  locality_name = "Mbalmayo",
  data_provider = "IRD",
  principal_investigator = "Jane Doe",
  data_manager = "John Smith",
  specimen_prefix = "PIRD"
)

# Always review these before importing
result$quadrats
result$name_mismatches
result$unassigned_stems

# One file per Import Wizard upload
export_openforis_for_wizard(result, dir = "to_import")

launch_import_wizard()
} # }
```
