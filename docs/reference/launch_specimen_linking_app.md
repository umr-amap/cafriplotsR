# Launch Specimen Linking App

Launches the interactive Shiny application for managing herbarium
specimens and their links to individual trees. The app has two main
sections:

## Usage

``` r
launch_specimen_linking_app(lang = "en")
```

## Arguments

- lang:

  Character, initial language ("en" or "fr"). Default "en".

## Value

Launches Shiny app (does not return until app closes)

## Details

\*\*Section 1: Import Specimens\*\* A wizard-style interface to import
new specimens from Excel/CSV files: - Step 1: Upload your data file -
Step 2: Map your columns to database fields - Step 3: Match collector
names and taxa to the database - Step 4: Preview and import to the
specimens table

\*\*Section 2: Link Specimens to Individuals\*\* - Search existing
specimens by collector, number, or taxonomy - Search individuals by
plot, tag, or species - Create links with taxonomic validation (same
genus check)

When creating links, the app validates that specimens and individuals
have compatible taxonomic identifications. Links between specimens and
individuals with different genus identifications are flagged for manual
review.

Link types: - type_individual: Specimen collected from this specific
individual - referenced_individual: Specimen represents same species but
from different individual

## Examples

``` r
if (FALSE) { # \dontrun{
launch_specimen_linking_app()
launch_specimen_linking_app(lang = "fr")
} # }
```
