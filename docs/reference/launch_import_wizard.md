# Launch Import Wizard Shiny App

Opens an interactive Shiny app that guides users through the complete
import workflow: data upload, column mapping, validation, preview, and
execution. This wizard wraps the existing import functions in a
user-friendly graphical interface.

## Usage

``` r
launch_import_wizard(launch_browser = TRUE, language = "fr")
```

## Arguments

- launch_browser:

  Logical: Open in external browser? (default TRUE)

- language:

  Character, initial language ("en" or "fr"), default: "fr"

## Value

Invisibly returns the Shiny app object

## Details

The wizard consists of 6 steps:

1.  Choose import type (plots or individuals)

2.  Upload data or download template

3.  Map columns to database schema

4.  Validate data quality

5.  Preview import (dry run)

6.  Execute import

All import logic reuses existing CafriplotsR functions:

- [`get_plot_metadata_template`](https://umr-amap.github.io/cafriplotsR/reference/get_plot_metadata_template.md) -
  Template generation

- [`map_user_columns`](https://umr-amap.github.io/cafriplotsR/reference/map_user_columns.md) -
  Column mapping

- [`validate_plot_metadata`](https://umr-amap.github.io/cafriplotsR/reference/validate_plot_metadata.md) -
  Validation

- [`import_plot_metadata`](https://umr-amap.github.io/cafriplotsR/reference/import_plot_metadata.md) -
  Import execution

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the import wizard
launch_import_wizard()

# Launch in RStudio Viewer pane
launch_import_wizard(launch_browser = FALSE)
} # }
```
