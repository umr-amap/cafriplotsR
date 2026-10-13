# Get Plot Metadata Template

Generate a template data frame with standard column names and example
data. Users can download this template, fill it with their data, and
import it using the plot metadata import functions.

## Usage

``` r
get_plot_metadata_template(
  template_type = c("permanent_plot", "transect", "nested_plot", "minimal", "full"),
  with_examples = TRUE,
  include_optional = TRUE,
  n_examples = 3,
  con = NULL
)
```

## Arguments

- template_type:

  Character: Type of template to generate - \`"permanent_plot"\`:
  Standard permanent forest plot (default) - \`"transect"\`: Linear
  transect survey - \`"nested_plot"\`: Plot nested in, or tiling part
  of, a plot already in the database (adds \`parent_plot\` and
  \`parent_relation\`) - \`"minimal"\`: Minimum required fields only -
  \`"full"\`: All available optional fields included

- with_examples:

  Logical: Include example rows with sample data? Default: TRUE

- include_optional:

  Logical: Include optional columns? Default: TRUE (ignored if
  template_type = "minimal")

- n_examples:

  Integer: Number of example rows to include (1 to 3). Default: 3

- con:

  Optional database connection (or pool). When supplied, the template
  follows the live database: the people columns are the
  \`subplotype_list\` features of valuetype \`table_colnam\`, plot
  characteristics are offered only if the database defines them, the
  hierarchy columns only if the plot hierarchy migration has been
  applied, and the example \`method\` / \`country\` values are real
  entries of \`methodslist\` / \`table_countries\`. Without it (or if
  the database cannot be queried) a static fallback is used.

## Value

A tibble with template structure. If \`with_examples = TRUE\`, includes
example rows with realistic sample data.

## Details

Columns follow the three storage kinds of the import (see
\[get_import_column_routing()\]):

\*\*Required columns (lookups to \`methodslist\` /
\`table_countries\`):\*\* - \`plot_name\`: Unique identifier for the
plot (character) - \`method\`: Survey method (must match
\[method_list()\]) - \`country\`: Country name (must match
\[country_list()\])

\*\*Flat columns (stored in \`data_liste_plots\`):\*\* - \`ddlat\`,
\`ddlon\`: Decimal degrees - \`elevation\`: Metres above sea level -
\`locality_name\`, \`province\`: Place names - \`date_y\`, \`date_m\`,
\`date_d\`: Survey date - \`subplot_shape\` (full): Shape of the
subplots - \`id_citation\` (full): Numeric id of the source, from
\`query_citations()\` - \`parent_plot\`, \`parent_relation\`
(nested_plot, full): the plot this one sits inside, by \`plot_name\`,
and how (\`"nested_subsample"\` when the two overlap with different
protocols - never sum them; \`"block_member"\` when this plot tiles part
of the parent). The parent must already be in the database; import
parents before children.

\*\*Features (stored in \`data_liste_sub_plots\`):\*\* - People
(\`valuetype = "table_colnam"\`, e.g. \`team_leader\`,
\`principal_investigator\`, \`data_manager\`, \`additional_people\`):
names as they appear in \`table_colnam\`, comma-separated for several. -
\`data_provider\` (full): free text. - \`plot_area\`,
\`vegetation_type\`: when defined in \`subplotype_list\`.

## See also

\[export_plot_template()\] to save template as Excel file
\[method_list()\] to see available methods \[country_list()\] to see
valid countries \[subplot_list()\] to see available subplot features
with validation rules

## Examples

``` r
if (FALSE) { # \dontrun{
# Get standard template with examples
template <- get_plot_metadata_template()
View(template)

# Follow the live database (real methods, countries and feature types)
template <- get_plot_metadata_template(con = call.mydb())

# Get minimal template (required fields only)
minimal <- get_plot_metadata_template(template_type = "minimal")

# Template for plots nested in existing plots
nested <- get_plot_metadata_template(template_type = "nested_plot")

# Export to Excel for filling
export_plot_template("my_plots_template.xlsx", template_type = "permanent_plot")
} # }
```
