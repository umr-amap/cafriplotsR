# Map Individual Data Columns

Automatically maps user column names from individual data import files
to database schema. Handles both the individuals sheet (flat columns)
and the features sheet (trait measurements).

## Usage

``` r
map_individual_columns(
  data = NULL,
  individuals_data = NULL,
  features_data = NULL,
  method = NULL,
  similarity_threshold = 0.6,
  interactive = TRUE,
  con = NULL
)
```

## Arguments

- data:

  Data frame with all columns in a single flat table (RECOMMENDED). The
  simplest approach - provide your complete dataset and the function
  will interactively guide you through column classification and
  mapping.

- individuals_data:

  Data frame from individuals sheet (OLD APPROACH). Must have columns
  that map to: plot_name, tag, idtax_n, original_tax_name. For backward
  compatibility - use \`data\` parameter instead for easier workflow.

- features_data:

  Data frame from features sheet (OLD APPROACH). Should have linking
  columns (plot_name, tag) plus trait measurements. For backward
  compatibility - use \`data\` parameter instead for easier workflow.

- method:

  Method type (e.g., "1ha-IRD", "Large"). Used for validation.

- similarity_threshold:

  Numeric: minimum similarity for fuzzy matching (0-1). Default: 0.6

- interactive:

  Logical: enable interactive column classification and mapping.
  Default: TRUE (highly recommended for new single-table workflow)

- con:

  Database connection. If NULL, creates temporary connection.

## Value

List with mapped data: - individuals: Data frame with standardized
individual column names - features: Data frame with standardized
trait/feature column names (if any features found) - mapping_info:
Details about how columns were mapped

## Details

Uses multiple strategies: 1. Exact matching 2. Synonym dictionary
(including domain-specific like dbh = stem_diameter) 3. Fuzzy string
matching

## Two Workflows

\*\*NEW RECOMMENDED WORKFLOW (single flat table):\*\*

Simply provide all your data in one table. The function will
interactively guide you to classify each column as either an individual
column (plot_name, tag, idtax_n, etc.) or a feature/trait measurement
(stem_diameter, height, etc.).

\*\*OLD WORKFLOW (two separate tables):\*\*

Manually separate data into individuals and features tables before
calling. Still supported for backward compatibility.

## Typical Usage

This function is typically called after: 1. Taxonomy standardization
(separate step using taxonomic matching tools!) 2. Data
collection/template filling

And before: 1. Data validation (validate_individual_data()) 2. Database
import (import_individual_data())

## See also

\[validate_individual_data()\] for data validation
\[import_individual_data()\] for database import

## Examples

``` r
if (FALSE) { # \dontrun{
# NEW RECOMMENDED APPROACH: Single flat table
my_trees <- readxl::read_excel("field_data.xlsx")
# Columns: Plot, TreeID, Species, idtax, DBH, Height, WoodDensity, etc.

mapped <- map_individual_columns(data = my_trees, interactive = TRUE)
# Interactive prompts guide you through:
# - Automatic matching where possible
# - For unmapped columns: "Is this a feature?" (yes/no/skip)
# - If NO: Select from list of individual columns
# - If YES: Select from list of available traits

# OLD APPROACH: Two separate tables (still works)
individuals <- readxl::read_excel("file.xlsx", sheet = "individuals")
features <- readxl::read_excel("file.xlsx", sheet = "features")
mapped <- map_individual_columns(
  individuals_data = individuals,
  features_data = features
)

# Access results
mapped$individuals  # Standardized individual data
mapped$features     # Standardized trait data (if any)
mapped$mapping_info # Details about mappings
} # }
```
