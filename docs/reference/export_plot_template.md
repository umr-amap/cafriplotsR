# Export Plot Metadata Template to Excel

Export a plot metadata template as an Excel file that users can fill and
import. The Excel file includes formatted columns and example data to
guide users.

## Usage

``` r
export_plot_template(
  file_path,
  template_type = c("permanent_plot", "transect", "minimal", "full"),
  with_examples = TRUE,
  include_optional = TRUE,
  n_examples = 3,
  open_file = TRUE
)
```

## Arguments

- file_path:

  Character: Path where the Excel file should be saved. Should end with
  .xlsx

- template_type:

  Character: Type of template (see \[get_plot_metadata_template()\])

- with_examples:

  Logical: Include example rows? Default: TRUE

- include_optional:

  Logical: Include optional columns? Default: TRUE

- n_examples:

  Integer: Number of example rows. Default: 3

- open_file:

  Logical: Open the file after creating it? Default: TRUE (Windows only)

## Value

Invisibly returns the template data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Export standard template
export_plot_template("my_plots.xlsx")

# Export minimal template without examples
export_plot_template(
  "plots_minimal.xlsx",
  template_type = "minimal",
  with_examples = FALSE
)

# Export full template for large import
export_plot_template(
  "plots_full.xlsx",
  template_type = "full",
  n_examples = 1
)
} # }
```
