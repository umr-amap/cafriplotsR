# Test script to diagnose dbh mapping issue

# Reload package
devtools::load_all()

# Create test data with dbh column
test_data <- data.frame(
  plot = c("P1", "P2"),
  subplot = c("S1", "S2"),
  dbh = c(10, 20),
  tag = c("T1", "T2"),
  species = c("Sp1", "Sp2")
)

# Get config for individuals
config <- get_import_column_routing("individuals", con = NULL)

# Check what's in the config
cat("\n=== CONFIG CHECK ===\n")
cat("direct_columns count:", length(config$direct_columns), "\n")
cat("feature_columns count:", length(config$feature_columns), "\n")
cat("synonyms count:", length(config$import_config$column_synonyms), "\n")

cat("\nChecking for stem_diameter in synonyms:\n")
if ("stem_diameter" %in% names(config$import_config$column_synonyms)) {
  cat("✓ stem_diameter found in synonyms\n")
  cat("  Synonyms:", paste(config$import_config$column_synonyms[["stem_diameter"]], collapse=", "), "\n")
} else {
  cat("✗ stem_diameter NOT in synonyms\n")
  cat("  Available synonym keys with 'stem':",
      paste(grep("stem", names(config$import_config$column_synonyms), value=TRUE), collapse=", "), "\n")
}

cat("\n=== FEATURE COLUMNS CHECK ===\n")
if ("stem_diameter" %in% config$feature_columns) {
  cat("✓ stem_diameter is in feature_columns\n")
} else {
  cat("✗ stem_diameter is NOT in feature_columns\n")
  cat("  Sample feature_columns:", paste(head(config$feature_columns, 10), collapse=", "), "\n")
}

# Now test mapping
cat("\n=== RUNNING MAPPING ===\n")
mapping_result <- map_user_columns(test_data, config)

cat("\n=== MAPPING RESULTS ===\n")
cat("Mapped columns:\n")
print(mapping_result$mappings)
cat("\nUnmapped columns:\n")
unmapped <- names(mapping_result$mappings)[is.na(mapping_result$mappings)]
print(unmapped)
