# Updating Existing Data in the Database

## Overview

This vignette explains how to update existing records in the database
using the
[`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md)
function.

**The main challenge**: When you query data, you see a *flat table* that
merges information from multiple database tables. The column you want to
update may come from the main table, a lookup table, a features table,
or be an aggregation of multiple records. Understanding this structure
is essential to correctly updating data.

## Prerequisites

``` r

library(CafriplotsR)
library(dplyr)

# Connect to both databases in one step (requires write permissions)
cons <- connect_cafri()
con <- cons$main
```

------------------------------------------------------------------------

## Understanding the Data Structure

### The Problem: Flat Tables vs. Relational Database

When you query plots or individuals, you see columns like:

``` r

# Query a plot
plot_data <- query_plots(plot_name = "my_plot", extract_individuals = TRUE)

# You see columns like:
# - plot_name, ddlat, ddlon (direct from data_liste_plots)
# - country (from table_countries via id_country)
# - method (from methodslist via id_method)
# - team_leader (aggregated from data_liste_sub_plots)
# - dbh, height (aggregated from data_traits_measures)
```

But these columns come from **different sources**:

| Column Type | Example | Source | How to Update |
|----|----|----|----|
| **Direct columns** | `plot_name`, `ddlat`, `ddlon` | Main table | `table_type = "plots"` |
| **Lookup columns** | `country`, `method` | Lookup tables | Auto-resolved by [`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md) |
| **Subplot features** | `team_leader`, `census_date` | `data_liste_sub_plots` | `table_type = "subplot_features"` |
| **Individual features** | `dbh`, `height` | `data_traits_measures` | `table_type = "individual_features"` |
| **Aggregated values** | Multiple DBH per tree | Aggregation | Cannot update directly |

------------------------------------------------------------------------

## The `update_records()` Function

### Basic Usage

``` r

update_records(
  data = your_data,           # Data frame with updates
  table_type = "individuals", # Which table type to update
  execute = FALSE,            # FALSE = dry run, TRUE = apply changes
  con = con
)
```

### Available Table Types

| `table_type` | Database Table | ID Column | Description |
|----|----|----|----|
| `"individuals"` | `data_individuals` | `id_n` | Individual tree records |
| `"plots"` | `data_liste_plots` | `id_liste_plots` | Plot metadata |
| `"individual_features"` | `data_traits_measures` | `id_trait_measures` | Tree measurements (DBH, height, etc.) |
| `"subplot_features"` | `data_liste_sub_plots` | `id_sub_plots` | Plot features (census dates, team, etc.) |
| `"individual_features_metadata"` | `data_ind_measures_feat` | `id_ind_meas_feat` | Metadata on measurements (POM height, etc.) |
| `"methodslist"` | `methodslist` | `id_method` | Sampling methods |
| `"table_colnam"` | `table_colnam` | `id_table_colnam` | Collector names |
| `"traitlist"` | `traitlist` | `id_trait` | Trait definitions |
| `"subplotype_list"` | `subplotype_list` | `id_subplotype` | Subplot feature types |

------------------------------------------------------------------------

## Workflow: Deciding Which Table Type to Use

### Step 1: Identify What Column You Want to Update

Ask yourself: *Where does this column come from?*

**Use `get_column_routing()` to see the structure:**

``` r

# See configuration for individuals
config <- get_column_routing("individuals", con)
print(config$direct_columns)   # Columns directly in data_individuals
print(config$feature_columns)  # Columns that are features (traits)
```

### Step 2: Choose the Correct Table Type

#### Case A: Direct Columns (plot_name, coordinates, tag, etc.)

``` r

# Updating individual coordinates
update_data <- data.frame(
  id_n = c(1001, 1002),  # Individual IDs
  x = c(11.0, 26.0),     # New X coordinates
  y = c(6.0, 13.0)       # New Y coordinates
)

update_records(
  data = update_data,
  table_type = "individuals",
  execute = FALSE,
  con = con
)
```

#### Case B: Feature Columns (DBH, height, etc.)

Feature columns require you to **work at the measurement level**, not
the individual level:

``` r

# WRONG: Trying to update DBH via individuals table
# This won't work because DBH is stored in data_traits_measures

# CORRECT: Get the measurement IDs first
measures <- query_individual_features(
  individual_ids = c(1001, 1002),
  trait_ids = 1,  # ID for DBH trait
  format = "long"
)

# Now update the measurements
update_data <- data.frame(
  id_trait_measures = measures$traits_num[[1]]$id_trait_measures,
  traitvalue = c(45.2, 32.1)  # New DBH values
)

update_records(
  data = update_data,
  table_type = "individual_features",
  execute = FALSE,
  con = con
)
```

#### Case C: Subplot Features (team_leader, census dates, etc.)

``` r

# First, get the subplot feature IDs
subplots <- query_subplot_features(plot_ids = 1, format = "long")

# Update specific subplot features
update_data <- data.frame(
  id_sub_plots = subplots$id_sub_plots[1:2],
  year = c(2024, 2024)  # Update census years
)

update_records(
  data = update_data,
  table_type = "subplot_features",
  execute = FALSE,
  con = con
)
```

------------------------------------------------------------------------

## Handling Special Cases

### Case 1: Aggregated Values Cannot Be Updated Directly

If a column represents an **aggregation** (e.g., average DBH across
multiple censuses), you cannot update it directly:

``` r

# When you query with aggregation:
individuals <- query_individual_features(
  plot_ids = 1,
  trait_ids = c(1, 2),
  format = "wide"  # Aggregates multiple measurements
)

# If individual has 3 DBH measurements across censuses,
# the displayed DBH is an average - you can't update "the average"

# SOLUTION: Work with the raw measurements
individuals_long <- query_individual_features(
  individual_ids = 1001,
  trait_ids = 1,
  format = "long"  # Shows all individual measurements
)

# Now update the specific measurement you want to change
update_records(
  data = data.frame(
    id_trait_measures = individuals_long$traits_num[[1]]$id_trait_measures[1],
    traitvalue = 45.2
  ),
  table_type = "individual_features",
  execute = FALSE,
  con = con
)
```

### Case 2: Census-Specific Features

If your data has columns like `dbh_census_1`, `dbh_census_2`, these are
census-specific and require special handling:

``` r

# Query with census detail
data_census <- query_individual_features(
  plot_ids = 1,
  include_multi_census = TRUE  # Keeps census-specific columns
)

# To update a specific census measurement:
# 1. Filter to the census you want
# 2. Get the measurement ID
# 3. Update using table_type = "individual_features"
```

### Case 3: Lookup Table Values (Country, Method)

The
[`update_records()`](https://umr-amap.github.io/cafriplotsR/reference/update_records.md)
function **automatically resolves** lookup values:

``` r

# You can provide the friendly name (country = "Gabon")
# instead of the ID (id_country = 5)

update_data <- data.frame(
  id_liste_plots = 1,
  country = "Cameroon"  # Will be resolved to id_country automatically
)

update_records(
  data = update_data,
  table_type = "plots",
  execute = FALSE,
  con = con
)
```

------------------------------------------------------------------------

## Practical Examples

### Example 1: Update Plot Coordinates

``` r

# Step 1: Query the plot to get its ID
plot <- query_plots(plot_name = "my_plot")
plot_id <- plot$id_liste_plots

# Step 2: Prepare update data
update_data <- data.frame(
  id_liste_plots = plot_id,
  ddlat = -0.52,
  ddlon = 11.48
)

# Step 3: Dry run
update_records(
  data = update_data,
  table_type = "plots",
  execute = FALSE,
  con = con
)

# Step 4: Execute if satisfied
update_records(
  data = update_data,
  table_type = "plots",
  execute = TRUE,
  con = con
)
```

### Example 2: Update Individual Taxonomy

``` r

# Update the taxon ID for an individual
update_data <- data.frame(
  id_n = 1001,
  idtax_n = 67890  # New taxon ID
)

update_records(
  data = update_data,
  table_type = "individuals",
  execute = FALSE,
  con = con
)
```

### Example 3: Update DBH Measurements

``` r

# Step 1: Get the measurement IDs for the DBH values you want to update
measures <- query_individual_features(
  individual_ids = c(1001, 1002, 1003),
  trait_ids = 1,  # DBH
  format = "long"
)

# Step 2: Review existing data
print(measures$traits_num[[1]] %>% select(id_trait_measures, id_data_individuals, traitvalue))

# Step 3: Prepare updates (only change what you need)
update_data <- data.frame(
  id_trait_measures = c(5001, 5002, 5003),  # Measurement IDs
  traitvalue = c(45.2, 32.1, 28.5)          # New DBH values
)

# Step 4: Update
update_records(
  data = update_data,
  table_type = "individual_features",
  execute = FALSE,
  con = con
)
```

### Example 4: Update Measurement Metadata (POM Height)

``` r

# Get measurement feature IDs
feat <- query_traits_measures_features(id_trait_measures = c(5001, 5002))

# Update the POM height metadata
update_data <- data.frame(
  id_ind_meas_feat = feat$id_ind_meas_feat,
  typevalue = c(1.3, 1.5)  # New POM heights
)

update_records(
  data = update_data,
  table_type = "individual_features_metadata",
  execute = FALSE,
  con = con
)
```

------------------------------------------------------------------------

## Dry Run vs Execute

**Always use `execute = FALSE` first** to preview changes:

``` r

# Dry run - shows what would change without modifying the database
result <- update_records(
  data = update_data,
  table_type = "individuals",
  execute = FALSE,  # IMPORTANT: Preview first!
  con = con
)

# Review the detected changes
print(result$changes)

# If satisfied, execute the update
update_records(
  data = update_data,
  table_type = "individuals",
  execute = TRUE,
  con = con
)
```

------------------------------------------------------------------------

## Decision Flowchart

    I want to update a column...
    │
    ├─► Is it plot_name, ddlat, ddlon, elevation, locality_name?
    │   └─► YES → table_type = "plots"
    │
    ├─► Is it tag, x, y, idtax_n, stem_code (individual core data)?
    │   └─► YES → table_type = "individuals"
    │
    ├─► Is it a measurement (dbh, height, etc.)?
    │   │
    │   ├─► Do you have the measurement ID (id_trait_measures)?
    │   │   └─► YES → table_type = "individual_features"
    │   │
    │   └─► NO → First query with format = "long" to get measurement IDs
    │
    ├─► Is it team_leader, census_date, principal_investigator (plot feature)?
    │   │
    │   ├─► Do you have the subplot ID (id_sub_plots)?
    │   │   └─► YES → table_type = "subplot_features"
    │   │
    │   └─► NO → First query with query_subplot_features() to get IDs
    │
    ├─► Is it metadata on a measurement (pom_height, method, etc.)?
    │   └─► YES → table_type = "individual_features_metadata"
    │
    └─► Is it a lookup table entry (method definition, trait definition)?
        └─► YES → table_type = "methodslist", "traitlist", "table_colnam", etc.

------------------------------------------------------------------------

## Summary of Key Points

1.  **The flat table you see is a merge of multiple database tables**
2.  **Use `execute = FALSE` first** to preview all changes
3.  **Work at the correct level**:
    - Individual core data → `table_type = "individuals"`
    - Plot core data → `table_type = "plots"`
    - Measurements (DBH, height) → `table_type = "individual_features"`
    - Plot features (census, team) → `table_type = "subplot_features"`
4.  **Aggregated values cannot be updated directly** - get the raw
    measurement IDs first
5.  **Lookup values are auto-resolved** - you can use friendly names
    like “Gabon” instead of IDs
6.  **Changes are backed up** automatically in `followup_updates_*`
    tables when available
