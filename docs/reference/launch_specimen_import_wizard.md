# Launch Specimen Import Wizard

Launches an interactive wizard-style Shiny application for importing new
herbarium specimens to the database from Excel/CSV files.

## Usage

``` r
launch_specimen_import_wizard(lang = "fr")
```

## Arguments

- lang:

  Character, initial language ("en" or "fr"). Default "en".

## Value

Launches Shiny app (does not return until app closes)

## Details

\*\*Wizard Steps:\*\* - Step 1: Upload your data file - Step 2: Map your
columns to database fields - Step 3: Match collector names and taxa to
the database - Step 4: Preview and import to the specimens table

The wizard guides you through the entire import process with validation
at each step. Collector names are matched to the \`table_colnam\` lookup
table, and taxonomic identifications are validated against the taxa
database.

For creating links between specimens and individuals, use the separate
\`launch_individual_specimen_linking_app()\` function.

## Examples

``` r
if (FALSE) { # \dontrun{
launch_specimen_import_wizard()
launch_specimen_import_wizard(lang = "fr")
} # }
```
