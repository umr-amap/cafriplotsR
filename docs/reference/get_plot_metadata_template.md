# Get Plot Metadata Template

Generate a template data frame with standard column names and example
data. Users can download this template, fill it with their data, and
import it using the plot metadata import functions.

## Usage

``` r
get_plot_metadata_template(
  template_type = c("permanent_plot", "transect", "minimal", "full"),
  with_examples = TRUE,
  include_optional = TRUE,
  n_examples = 3
)
```

## Arguments

- template_type:

  Character: Type of template to generate - \`"permanent_plot"\`:
  Standard permanent forest plot (default) - \`"transect"\`: Linear
  transect survey - \`"minimal"\`: Minimum required fields only -
  \`"full"\`: All available optional fields included

- with_examples:

  Logical: Include example rows with sample data? Default: TRUE

- include_optional:

  Logical: Include optional columns? Default: TRUE (ignored if
  template_type = "minimal")

- n_examples:

  Integer: Number of example rows to include. Default: 3

## Value

A tibble with template structure. If \`with_examples = TRUE\`, includes
example rows with realistic sample data.

## Details

The template includes standard column names that match the database
schema:

\*\*Required columns:\*\* - \`plot_name\`: Unique identifier for the
plot (character) - \`method\`: Survey method (must match
\[method_list()\]) - \`country\`: Country name (must match
\[country_list()\])

\*\*Location columns:\*\* - \`ddlat\`: Latitude in decimal degrees (-90
to 90) - \`ddlon\`: Longitude in decimal degrees (-180 to 180) -
\`elevation\`: Elevation in meters above sea level - \`locality_name\`:
Name of the locality or site - \`province\`: Province or state name

\*\*Date columns:\*\* - \`date_y\`: Year of survey (YYYY) - \`date_m\`:
Month of survey (1-12) - \`date_d\`: Day of survey (1-31)

\*\*People columns (subplot features with
valuetype='table_colnam'):\*\* - Dynamically determined from database
via \[subplot_list()\] - Common examples: team_leader,
principal_investigator, data_manager, etc. - Multiple names can be
comma-separated

\*\*Plot characteristics:\*\* - \`plot_area\`: Plot area in hectares -
Other characteristics as defined in database

Column descriptions and validation rules are pulled from the database
via \[subplot_list()\] for subplot features.

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

# Get minimal template (required fields only)
minimal <- get_plot_metadata_template(template_type = "minimal")

# Get full template without examples (for large imports)
empty_full <- get_plot_metadata_template(
  template_type = "full",
  with_examples = FALSE
)

# Export to Excel for filling
export_plot_template("my_plots_template.xlsx", template_type = "permanent_plot")
} # }
```
