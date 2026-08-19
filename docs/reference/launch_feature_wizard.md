# Launch Feature Wizard Shiny App

Opens an interactive Shiny app that guides users through adding features
to existing plots. Supports two modes: adding a new census (with dates
and people) or adding arbitrary plot-level features.

## Usage

``` r
launch_feature_wizard(launch_browser = TRUE, language = "fr")
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

1.  Login & select existing plots

2.  Choose operation mode (New Census or Add Plot Features)

3.  Enter or upload plot features

4.  Match lookup values (people names) to database

5.  Validate and preview data

6.  Execute import (with dry-run support)

Phase 1 covers plot-level features and census metadata. Phase 2 (future)
will add individual measurements and recruit handling.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the feature wizard
launch_feature_wizard()

# Launch in RStudio Viewer pane
launch_feature_wizard(launch_browser = FALSE)
} # }
```
