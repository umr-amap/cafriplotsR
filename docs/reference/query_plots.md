# Query plots from database

This function queries a PostgreSQL inventory database to return a list
of forest plots or individuals, with options to include associated
traits and metadata, and to generate interactive maps.

## Usage

``` r
query_plots(
  plot_name = NULL,
  tag = NULL,
  country = NULL,
  locality_name = NULL,
  method = NULL,
  extract_individuals = FALSE,
  map = FALSE,
  id_individual = NULL,
  id_plot = NULL,
  id_tax = NULL,
  id_specimen = NULL,
  interactive = TRUE,
  show_multiple_census = FALSE,
  extract_coordinates = FALSE,
  show_all_coordinates = lifecycle::deprecated(),
  remove_ids = TRUE,
  extract_traits = TRUE,
  extract_individual_features = TRUE,
  traits_to_genera = FALSE,
  wd_fam_level = FALSE,
  include_liana = FALSE,
  extract_subplot_features = TRUE,
  concatenate_stem = FALSE,
  issues = c("remove", "include", "ignore"),
  include_measurement_ids = FALSE,
  exact_match = FALSE,
  census_strategy = c("last", "first", "mean"),
  individual_features_format = c("wide", "long", "census_pairs"),
  output_style = "auto",
  backbone = c("internal", "wcvp"),
  con = NULL,
  con.taxa = NULL
)
```

## Arguments

- plot_name:

  Optional. A single string specifying plot name.

- tag:

  Optional. Tag identifier.

- country:

  Optional. A single string specifying country.

- locality_name:

  Optional. A single string specifying locality name.

- method:

  Optional. Method identifier.

- extract_individuals:

  Logical. Whether to extract individuals. Optional.

- map:

  Logical. Whether to generate map. Optional.

- id_individual:

  Optional. Individual identifiers.

- id_plot:

  Optional. Plot identifiers.

- id_tax:

  Optional. Taxonomic identifiers.

- id_specimen:

  Optional. Specimen identifiers.

- interactive:

  Logical. Whether the query should be interactive. TRUE by default.

- show_multiple_census:

  Logical. Whether to show multiple census data. Optional.

- extract_coordinates:

  Logical. Whether to extract subplot coordinates as separate tables.
  When TRUE, returns \`coordinates\` (raw coordinate data) and
  \`coordinates_sf\` (spatial features) in the output list. Default is
  FALSE.

- show_all_coordinates:

  \`r lifecycle::badge("deprecated")\` Use \`extract_coordinates\`
  instead.

- remove_ids:

  Logical. Whether to remove ID columns from output. Optional.

- extract_traits:

  Logical. Whether to extract taxonomic traits. Optional.

- extract_individual_features:

  Logical. Whether to extract individual-level features. Optional.

- traits_to_genera:

  Logical. Whether to aggregate traits to genus level. Optional.

- wd_fam_level:

  Logical. Whether to use family-level wood density. Optional.

- include_liana:

  Logical. Whether to include lianas. Optional.

- extract_subplot_features:

  Logical. Whether to extract subplot features. Optional.

- concatenate_stem:

  Logical. Whether to concatenate multiple stems. Optional.

- issues:

  Character. How to handle flagged measurements. Options:

  - `"remove"` (default): Drop flagged measurements before aggregation.

  - `"include"`: Keep flagged measurements and add an issue column in
    output.

  - `"ignore"`: Keep flagged measurements but do not show the issue
    column.

- include_measurement_ids:

  Logical. Whether to include measurement IDs in aggregated output.
  Optional.

- census_strategy:

  Character. Strategy for selecting census when \`show_multiple_census =
  FALSE\`. Options: "last" (default, most recent census), "first"
  (earliest census), or "mean" (average across all censuses). When
  "first" or "last" is selected, individuals recruited after the first
  census or dead before the last census will have NA values, reflecting
  biological reality.

- individual_features_format:

  Character. Format for individual-level feature measurements.
  \`"wide"\` (default) returns one row per individual with one column
  per trait (aggregated). \`"long"\` returns one row per individual per
  measurement: if an individual has two diameter measurements, it will
  appear in two rows. Adds columns \`trait\`, \`traitvalue\`,
  \`traitvalue_char\`, \`valuetype\`, \`census_name\`, and
  \`census_date\` (NA when not census-linked). Census filtering via
  \`show_multiple_census\` / \`census_strategy\` is still applied.
  Incompatible with \`concatenate_stem = TRUE\`. \`"census_pairs"\`
  returns one row per consecutive pair of censuses per individual.
  Census pairing is plot-level: every individual is crossed against all
  consecutive census pairs of its plot, receiving \`NA\` at any census
  where it has no measurement (e.g. recruited at census_2 →
  \`stem_diameter_0 = NA\`; dead at census_2 → \`stem_diameter_1 =
  NA\`). All individual-level traits appear as \`\<trait\>\_0\` and
  \`\<trait\>\_1\` columns. Additional columns: \`census_name_0\`,
  \`census_name_1\`, \`date_census0\`, \`date_census1\`, \`time\` (days
  between the two censuses). All available censuses are used regardless
  of \`show_multiple_census\`. Incompatible with \`concatenate_stem =
  TRUE\`.

- output_style:

  Either a character scalar – one of \`"auto"\`, \`"minimal"\`,
  \`"standard"\`, \`"permanent_plot"\`,
  \`"permanent_plot_multi_census"\`, \`"transect"\`, \`"full"\`,
  \`"census_pairs"\` – or a \`plot_output_style\` object built with
  \[output_style()\]. Defaults to \`"auto"\`, which picks a style from
  the \`method\` field of the queried plots. When
  \`individual_features_format = "census_pairs"\`, character values
  (other than \`"full"\`) are overridden to \`"census_pairs"\`; a custom
  \`plot_output_style\` object is respected as-is. Use
  \[list_output_styles()\] for an overview of available built-in styles,
  \[get_output_style()\] to inspect what a style does, and
  \[output_style()\] to build a custom one (typically with \`based_on\`
  to inherit from a built-in and override a few fields). The
  configuration schema is documented in \[output_style()\].

- con:

  Optional database connection to main database. If NULL, will call
  call.mydb() to establish connection. If you've already connected with
  \`mydb \<- call.mydb()\`, pass \`con = mydb\` to avoid re-prompting.

- con.taxa:

  Optional database connection to taxa database. If NULL, will check for
  \`mydb.taxa\` in calling environment, otherwise will call
  call.mydb.taxa() to establish connection. Pass explicitly to avoid
  credential prompts.

## Value

A list or data frame containing plot data and associated information.
When multiple components are requested, returns a list with elements
like \`extract\`, \`census_features\`, \`coordinates\`, and
\`coordinates_sf\`. If only one component is available, returns that
component directly. Returns \`NA\` if no plots are found matching the
criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
  query_plots(country = "Gabon", extract_individuals = FALSE)
  
  query_plots(country = "Cameroon")
  
  query_plots(plot_name = "mbalmayo001")
} # }

```
