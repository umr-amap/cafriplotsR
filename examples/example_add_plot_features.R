# Example: Adding Plot Features to Existing Plots
#
# This script demonstrates how to use the add_plot_features() function
# to add subplot features (team members, census dates, etc.) to existing plots.

library(CafriplotsR)
library(dplyr)

# Connect to database
con <- call.mydb()

# ==============================================================================
# Example 1: Simple Team Information
# ==============================================================================

cat("\n=== Example 1: Adding Team Information ===\n\n")

# Prepare data with team information
team_data <- data.frame(
  plot_name = c("mbalmayo001", "mbalmayo002", "mbalmayo003"),
  team_leader = c("John Doe", "Jane Smith", "Bob Wilson"),
  principal_investigator = c("Dr. Marie Blanc", "Dr. Marie Blanc", "Prof. Gilles Dauby"),
  data_manager = c("Alice Brown", "Tom Jones", "Claire Leblanc"),
  stringsAsFactors = FALSE
)

print(team_data)

# DRY RUN first - always preview before committing!
cat("\nRunning DRY RUN...\n")
result_preview <- add_plot_features(
  data = team_data,
  dry_run = TRUE,
  con = con
)

cat("\nDo you want to proceed with actual import? (y/n): ")
proceed <- readline()

if (tolower(trimws(proceed)) == "y") {
  cat("\nProceeding with actual import...\n")
  result <- add_plot_features(
    data = team_data,
    dry_run = FALSE,
    con = con
  )

  print(result)

  if (result$success) {
    cat("\n✓ Success! Features have been added.\n")
  }
}


# ==============================================================================
# Example 2: Census Date Information
# ==============================================================================

cat("\n\n=== Example 2: Adding Census Dates ===\n\n")

# Prepare census date data
census_data <- data.frame(
  plot_name = c("mbalmayo001", "mbalmayo002", "mbalmayo003"),
  census_year = c(2020, 2020, 2021),
  census_month = c(6, 8, 3),
  census_day = c(15, 22, 10),
  stringsAsFactors = FALSE
)

print(census_data)

# Dry run
cat("\nRunning DRY RUN...\n")
result_preview <- add_plot_features(
  data = census_data,
  dry_run = TRUE,
  con = con
)


# ==============================================================================
# Example 3: Custom Column Names with Interactive Mapping
# ==============================================================================

cat("\n\n=== Example 3: Custom Column Names ===\n\n")

# Data with non-standard column names
custom_data <- data.frame(
  PlotName = c("mbalmayo001", "mbalmayo002"),
  TeamLead = c("John Doe", "Jane Smith"),
  PI = c("Dr. Smith", "Dr. Jones"),
  Year = c(2020, 2021),
  stringsAsFactors = FALSE
)

print(custom_data)

cat("\nThis will use interactive mapping to match columns...\n")

# Interactive mode will prompt you to map columns
result_preview <- add_plot_features(
  data = custom_data,
  interactive = TRUE,
  dry_run = TRUE,
  con = con
)


# ==============================================================================
# Example 4: Using Pre-defined Column Mapping (Non-interactive)
# ==============================================================================

cat("\n\n=== Example 4: Pre-defined Mapping ===\n\n")

# Define mapping explicitly (useful for scripts)
my_mapping <- list(
  PlotName = "plot_name",
  TeamLead = "team_leader",
  PI = "principal_investigator",
  Year = "census_year"
)

# Use the mapping
result_preview <- add_plot_features(
  data = custom_data,
  column_mapping = my_mapping,
  interactive = FALSE,
  dry_run = TRUE,
  con = con
)


# ==============================================================================
# Example 5: Multiple People (Comma-separated)
# ==============================================================================

cat("\n\n=== Example 5: Multiple People per Field ===\n\n")

# Multiple people in comma-separated format
multi_people_data <- data.frame(
  plot_name = c("mbalmayo001", "mbalmayo002"),
  team_leader = c("John Doe, Jane Smith", "Bob Wilson"),
  additional_people = c("Alice Brown, Tom Jones, Emma Davis", "Chris Lee, Ana Garcia"),
  stringsAsFactors = FALSE
)

print(multi_people_data)

cat("\nThe function will split comma-separated names and link each person...\n")

result_preview <- add_plot_features(
  data = multi_people_data,
  dry_run = TRUE,
  con = con
)


# ==============================================================================
# Example 6: Using Plot IDs Instead of Names
# ==============================================================================

cat("\n\n=== Example 6: Using Plot IDs ===\n\n")

# First, query plots to get their IDs
plots <- query_plots(
  locality_name = "Mbalmayo",
  extract_individuals = FALSE,
  con = con
)

cat("Found", nrow(plots), "plots in Mbalmayo\n")

# Use IDs instead of names
if (nrow(plots) > 0) {
  id_data <- data.frame(
    id_liste_plots = plots$id_liste_plots[1:min(3, nrow(plots))],
    team_leader = c("Person A", "Person B", "Person C")[1:min(3, nrow(plots))],
    stringsAsFactors = FALSE
  )

  print(id_data)

  result_preview <- add_plot_features(
    data = id_data,
    plot_id_column = "id_liste_plots",
    dry_run = TRUE,
    con = con
  )
}


# ==============================================================================
# Example 7: Verify Features Were Added
# ==============================================================================

cat("\n\n=== Example 7: Verify Added Features ===\n\n")

# Query plots to see their features
plots_with_features <- query_plots(
  plot_name = c("mbalmayo001", "mbalmayo002", "mbalmayo003"),
  exact_match = TRUE,
  show_multiple_census = TRUE,
  con = con
)

if (!is.null(plots_with_features$census_features)) {
  cat("\nCensus features:\n")
  print(plots_with_features$census_features)
}

# Or query subplot features directly
if (exists("plots") && nrow(plots) > 0) {
  subplot_features <- query_subplot_features(
    plot_ids = plots$id_liste_plots[1:min(3, nrow(plots))],
    format = "wide",
    con = con
  )

  cat("\nSubplot features (wide format):\n")
  print(subplot_features)
}


# ==============================================================================
# Example 8: See Available Subplot Features
# ==============================================================================

cat("\n\n=== Example 8: Available Subplot Features ===\n\n")

# List all available subplot feature types
available_features <- subplot_list(con = con)

cat("Available subplot feature types:\n")
print(head(available_features, 20))

cat("\nTotal feature types available:", nrow(available_features), "\n")


# ==============================================================================
# Cleanup
# ==============================================================================

cat("\n\nExamples completed!\n")
cat("Remember: Always use dry_run = TRUE first to preview changes!\n\n")

# Close connection
DBI::dbDisconnect(con)
