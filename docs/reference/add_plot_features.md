# Add Plot Features to Existing Plots

User-friendly function to add subplot features (census dates, team
members, plot characteristics, etc.) to existing plots in the database.
Automatically maps column names to database feature types and validates
data before import.

## Usage

``` r
add_plot_features(
  data,
  plot_id_column = NULL,
  column_mapping = NULL,
  interactive = TRUE,
  dry_run = TRUE,
  con = NULL,
  similarity_threshold = 0.6,
  ask_before_update = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing plot features to add. Must include: - A plot
  identifier column (either \`plot_name\` or \`id_liste_plots\`) - One
  or more feature columns (e.g., \`team_leader\`, \`census_date\`, etc.)

- plot_id_column:

  Character: Name of the column containing plot identifiers. Options: -
  \`"plot_name"\` (default): Plot names as strings -
  \`"id_liste_plots"\`: Plot IDs as integers If \`NULL\`, the function
  will try to detect it automatically.

- column_mapping:

  Named list: Optional pre-defined mapping of user column names to
  subplot feature types. If \`NULL\`, interactive mapping will be used.
  Example: \`list(PI = "principal_investigator", TeamLead =
  "team_leader")\`

- interactive:

  Logical: If \`TRUE\` (default), use interactive prompts for column
  mapping. Set to \`FALSE\` for non-interactive/scripted usage.

- dry_run:

  Logical: If \`TRUE\`, preview changes without committing to database.
  Always recommended to run with \`dry_run = TRUE\` first! (Default:
  \`TRUE\`)

- con:

  Database connection. If \`NULL\`, will create a new connection.

- similarity_threshold:

  Numeric: Threshold for fuzzy column name matching (0 to 1). Default:
  0.6. Higher values require closer matches.

- ask_before_update:

  Logical: If \`TRUE\`, ask for confirmation before updating existing
  records. Default: \`TRUE\`.

- verbose:

  Logical: If \`TRUE\`, show detailed progress messages. Default:
  \`TRUE\`.

## Value

List with import results:

- success:

  Logical: TRUE if import succeeded

- n_rows:

  Number of feature records added

- n_plots:

  Number of plots affected

- plot_names:

  Vector of plot names affected

- feature_types:

  Vector of feature types added

- mapping:

  Column mapping used

- dry_run:

  Was this a dry-run?

- message:

  Summary message

## Details

\*\*What are subplot features?\*\*

Subplot features are attributes that describe plots or census events: -
\*\*People\*\*: \`team_leader\`, \`principal_investigator\`,
\`data_manager\`, etc. - \*\*Dates\*\*: \`census_date\` (year/month/day
columns) - \*\*Plot characteristics\*\*: Various plot-specific
measurements

Use \`subplot_list()\` to see all available subplot feature types and
their definitions.

\*\*Data Structure:\*\*

Your data should have one row per feature instance: “\` plot_name \|
team_leader \| principal_investigator \| census_year
———-\|————-\|————————\|———— Plot-A \| John Doe \| Dr. Smith \| 2020
Plot-B \| Jane Smith \| Dr. Smith \| 2020 “\`

\*\*Column Mapping:\*\*

The function intelligently maps your column names to database feature
types: 1. Exact match (e.g., \`team_leader\` → \`team_leader\`) 2.
Synonym match (e.g., \`PI\` → \`principal_investigator\`) 3. Fuzzy
string match (e.g., \`TeamLeader\` → \`team_leader\`) 4. Interactive
selection (if \`interactive = TRUE\`)

\*\*People Fields:\*\*

For people-related features (team_leader, PI, etc.), names will be: -
Matched against existing people in \`table_colnam\` - You can add new
people interactively during import - Multiple names can be
comma-separated (e.g., "John Doe, Jane Smith")

## See also

\[subplot_list()\] to see available subplot feature types
\[add_subplot_features()\] for the low-level function
\[query_subplot_features()\] to query existing features

## Examples

``` r
if (FALSE) { # \dontrun{
library(CafriplotsR)

# Example 1: Add team leader and PI to plots
plot_features <- data.frame(
  plot_name = c("Plot-A", "Plot-B", "Plot-C"),
  team_leader = c("John Doe", "Jane Smith", "Bob Wilson"),
  principal_investigator = c("Dr. Smith", "Dr. Smith", "Dr. Jones"),
  census_year = c(2020, 2020, 2021)
)

# Dry run first (preview)
result <- add_plot_features(
  data = plot_features,
  dry_run = TRUE
)

# Check what would be added
print(result)

# If satisfied, actually add the features
result <- add_plot_features(
  data = plot_features,
  dry_run = FALSE
)

# Example 2: Add features with custom column names
my_data <- data.frame(
  PlotName = c("Plot-A", "Plot-B"),
  TeamLead = c("John Doe", "Jane Smith"),
  PI = c("Dr. Smith", "Dr. Jones")
)

# Interactive mapping will help match columns
result <- add_plot_features(
  data = my_data,
  interactive = TRUE,
  dry_run = TRUE
)

# Example 3: Non-interactive with pre-defined mapping
mapping <- list(
  PlotName = "plot_name",
  TeamLead = "team_leader",
  PI = "principal_investigator"
)

result <- add_plot_features(
  data = my_data,
  column_mapping = mapping,
  interactive = FALSE,
  dry_run = FALSE
)

# Example 4: See available subplot features
available_features <- subplot_list()
View(available_features)
} # }
```
