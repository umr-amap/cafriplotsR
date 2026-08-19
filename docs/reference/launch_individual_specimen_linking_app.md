# Launch Individual-Specimen Linking App

Launches interactive Shiny application for creating links between
individual trees and herbarium specimens based on herbarium information
in the individuals dataset.

## Usage

``` r
launch_individual_specimen_linking_app(lang = "fr")
```

## Arguments

- lang:

  Character, initial language ("en" or "fr"). Default "en".

## Value

Launches Shiny app (does not return until app closes)

## Details

\*\*Workflow:\*\*

1\. \*\*Select Individuals\*\*: Filter individuals by plot, country,
tag, etc. 2. \*\*Parse Herbarium Info\*\*: Extract collector names and
specimen numbers from \`herbarium_nbe_char\` and \`herbarium_nbe_type\`
columns 3. \*\*Match Collectors\*\*: Interactively match extracted
collector names to entries in \`table_colnam\` 4. \*\*Retrieve
Specimens\*\*: Find matching specimens in database by collector + number
5. \*\*Validate Taxonomy\*\*: Review taxonomic matches between
individuals and specimens 6. \*\*Create Links\*\*: Execute link creation
for validated matches

\*\*Understanding the Two Column Types:\*\*

The system uses two columns to track different specimen-individual
relationships:

\- \*\*\`herbarium_nbe_type\`\*\*: The ACTUAL tree where the specimen
was physically collected - Direct evidence (high confidence) - Creates
\`type_individual\` link

\- \*\*\`herbarium_nbe_char\`\*\*: Trees field-identified as the SAME
SPECIES as the specimen tree - Indirect evidence based on field
identification (lower confidence) - Creates \`referenced_individual\`
link - Extends specimen utility to more trees without collecting
additional specimens

\*\*Link Type Logic:\*\* - If \`herbarium_nbe_type\` is non-empty →
\`type_individual\` (specimen from THIS tree) - If only
\`herbarium_nbe_char\` is non-empty → \`referenced_individual\` (tree
believed to be same species) - If both columns have the SAME value →
\`type_individual\` (confirms this is the specimen tree)

## Examples

``` r
if (FALSE) { # \dontrun{
launch_individual_specimen_linking_app()
launch_individual_specimen_linking_app(lang = "fr")
} # }
```
