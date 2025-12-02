# Test script for census_strategy feature
# This script tests the new census selection functionality with real database data

library(plotsdatabase)
library(dplyr)
library(tidyr)

cat("\n========================================\n")
cat("CENSUS STRATEGY FEATURE TEST\n")
cat("========================================\n\n")

# Test with a plot that has multiple censuses
test_plot_id <- 1188  # Adjust this to a plot ID that has multiple censuses in your database

cat("Testing with plot ID:", test_plot_id, "\n\n")

# First, check what censuses exist for this plot
cat("Step 1: Checking available censuses for this plot...\n")
cat("----------------------------------------\n")

census_info <- query_plot_features(
  plot_ids = test_plot_id,
  subplot_type = "census"
)

if (!is.na(census_info$census_info[1])) {
  print(census_info$census_info)
  n_census <- max(census_info$census_info$number_of_census, na.rm = TRUE)
  cat("\nNumber of censuses found:", n_census, "\n\n")
} else {
  cat("\nWARNING: No census information found for this plot!\n")
  cat("Please choose a different plot_id with multiple censuses.\n\n")
}

# Extract raw census features to see dates
if (nrow(census_info$features_raw) > 0) {
  cat("\nCensus details:\n")
  census_details <- census_info$features_raw %>%
    filter(type == "census") %>%
    select(typevalue, year, month, day) %>%
    arrange(typevalue)
  print(census_details)
  cat("\n")
}

cat("\n========================================\n")
cat("Step 2: Testing LAST census strategy (default)\n")
cat("========================================\n\n")

result_last <- query_plots(
  id_plot = test_plot_id,
  extract_individuals = TRUE,
  census_strategy = "last",
  show_multiple_census = FALSE, output_style = "auto"
)

if (!is.na(result_last$individuals[1]) && nrow(result_last$individuals) > 0) {
  cat("Number of individuals extracted:", nrow(result_last$individuals), "\n")

  # Show sample of individual features
  feature_cols <- names(result_last$individuals)[grepl("stem_diameter|tree_height|dbh",
                                                     names(result_last$individuals),
                                                     ignore.case = TRUE)]

  if (length(feature_cols) > 0) {
    cat("\nSample of individual features (first 10 rows):\n")
    sample_data <- result_last$extract %>%
      select(id_n, tag, any_of(feature_cols)) %>%
      head(10)
    print(sample_data)

    # Count NAs
    na_counts <- sample_data %>%
      summarise(across(any_of(feature_cols), ~sum(is.na(.))))
    cat("\nNA counts in features:\n")
    print(na_counts)
  }
} else {
  cat("ERROR: No data extracted for LAST census strategy\n")
}

cat("\n========================================\n")
cat("Step 3: Testing FIRST census strategy\n")
cat("========================================\n\n")

result_first <- query_plots(
  id_plot = test_plot_id,
  extract_individuals = TRUE,
  census_strategy = "first",
  extract_traits = FALSE,
  show_multiple_census = FALSE
)

if (!is.na(result_first$extract[1]) && nrow(result_first$extract) > 0) {
  cat("Number of individuals extracted:", nrow(result_first$extract), "\n")

  feature_cols <- names(result_first$extract)[grepl("stem_diameter|tree_height|dbh",
                                                      names(result_first$extract),
                                                      ignore.case = TRUE)]

  if (length(feature_cols) > 0) {
    cat("\nSample of individual features (first 10 rows):\n")
    sample_data <- result_first$extract %>%
      select(id_n, tag, any_of(feature_cols)) %>%
      head(10)
    print(sample_data)

    na_counts <- sample_data %>%
      summarise(across(any_of(feature_cols), ~sum(is.na(.))))
    cat("\nNA counts in features:\n")
    print(na_counts)
  }
} else {
  cat("ERROR: No data extracted for FIRST census strategy\n")
}

cat("\n========================================\n")
cat("Step 4: Testing MEAN census strategy (original behavior)\n")
cat("========================================\n\n")

result_mean <- query_plots(
  id_plot = test_plot_id,
  extract_individuals = TRUE,
  census_strategy = "mean",
  extract_traits = FALSE,
  show_multiple_census = FALSE
)

if (!is.na(result_mean$extract[1]) && nrow(result_mean$extract) > 0) {
  cat("Number of individuals extracted:", nrow(result_mean$extract), "\n")

  feature_cols <- names(result_mean$extract)[grepl("stem_diameter|tree_height|dbh",
                                                     names(result_mean$extract),
                                                     ignore.case = TRUE)]

  if (length(feature_cols) > 0) {
    cat("\nSample of individual features (first 10 rows):\n")
    sample_data <- result_mean$extract %>%
      select(id_n, tag, any_of(feature_cols)) %>%
      head(10)
    print(sample_data)

    na_counts <- sample_data %>%
      summarise(across(any_of(feature_cols), ~sum(is.na(.))))
    cat("\nNA counts in features:\n")
    print(na_counts)
  }
} else {
  cat("ERROR: No data extracted for MEAN census strategy\n")
}

cat("\n========================================\n")
cat("Step 5: COMPARISON - Same individual across strategies\n")
cat("========================================\n\n")

# Pick a sample individual that exists in all results
if (!is.na(result_last$extract[1]) && !is.na(result_first$extract[1]) && !is.na(result_mean$extract[1])) {

  sample_id <- result_last$extract$id_n[1]

  cat("Comparing individual ID:", sample_id, "\n\n")

  feature_cols <- names(result_last$extract)[grepl("stem_diameter|tree_height|dbh",
                                                     names(result_last$extract),
                                                     ignore.case = TRUE)]

  if (length(feature_cols) > 0 && length(feature_cols) <= 5) {

    comparison <- tibble(
      strategy = c("FIRST", "LAST", "MEAN")
    )

    for (col in feature_cols) {
      val_first <- result_first$extract %>% filter(id_n == sample_id) %>% pull(!!sym(col))
      val_last <- result_last$extract %>% filter(id_n == sample_id) %>% pull(!!sym(col))
      val_mean <- result_mean$extract %>% filter(id_n == sample_id) %>% pull(!!sym(col))

      comparison[[col]] <- c(
        ifelse(length(val_first) > 0, val_first[1], NA),
        ifelse(length(val_last) > 0, val_last[1], NA),
        ifelse(length(val_mean) > 0, val_mean[1], NA)
      )
    }

    cat("Feature comparison:\n")
    print(comparison)

    cat("\n\nExpected behavior:\n")
    cat("- FIRST: Should show value from earliest census only\n")
    cat("- LAST: Should show value from most recent census only\n")
    cat("- MEAN: Should show average across all censuses\n")
    cat("- If individual was recruited later: FIRST should be NA\n")
    cat("- If individual died earlier: LAST should be NA\n")
  }
}

cat("\n========================================\n")
cat("Step 6: Testing with show_multiple_census = TRUE\n")
cat("========================================\n\n")
cat("When show_multiple_census=TRUE, census_strategy should be ignored\n")
cat("and all censuses should be shown as separate columns\n\n")

result_multi <- query_plots(
  id_plot = test_plot_id,
  extract_individuals = TRUE,
  census_strategy = "last",  # Should be ignored
  extract_traits = FALSE,
  show_multiple_census = TRUE  # This should override census_strategy
)

if (!is.na(result_multi$extract[1]) && nrow(result_multi$extract) > 0) {
  cat("Number of individuals extracted:", nrow(result_multi$extract), "\n")

  # Look for census-specific columns (e.g., stem_diameter_census_1, stem_diameter_census_2)
  census_cols <- names(result_multi$extract)[grepl("_census_\\d+$", names(result_multi$extract))]

  if (length(census_cols) > 0) {
    cat("\nCensus-specific columns found:\n")
    print(census_cols)

    cat("\nSample data with census columns (first 5 rows):\n")
    sample_multi <- result_multi$extract %>%
      select(id_n, tag, any_of(census_cols[1:min(6, length(census_cols))])) %>%
      head(5)
    print(sample_multi)
  } else {
    cat("\nWARNING: No census-specific columns found!\n")
  }
}

cat("\n========================================\n")
cat("TEST COMPLETE\n")
cat("========================================\n\n")

cat("Summary:\n")
cat("- If FIRST and LAST show different values → SUCCESS\n")
cat("- If MEAN is between FIRST and LAST (or equal) → SUCCESS\n")
cat("- If some individuals have NA in FIRST but not LAST → SUCCESS (recruited)\n")
cat("- If some individuals have NA in LAST but not FIRST → SUCCESS (died)\n")
cat("- If show_multiple_census=TRUE shows census_1, census_2 columns → SUCCESS\n\n")
