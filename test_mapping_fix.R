# Test column mapping fix without database connection
devtools::load_all()

# Create minimal test data with multiple columns
test_data <- data.frame(
  plot = c("P1", "P2"),
  subplot = c("S1", "S2"),
  dbh = c(10, 20),
  tag = c("T1", "T2"),
  species = c("Sp1", "Sp2")
)

# Build config manually with merged synonyms (simulating fixed behavior)
config <- list(
  direct_columns = c("plot_name", "tag", "date_y"),
  feature_columns = c("stem_diameter", "height", "quadrat"),
  import_config = list(
    column_synonyms = list(
      plot_name = c("plot", "plotid", "plot_id"),
      stem_diameter = c("dbh", "d.b.h.", "diameter", "diam", "d"),
      quadrat = c("subplot", "sub_plot", "sub-plot", "subplotid", "quarter"),
      tag = c("tag", "tag_id"),
      species = c("species", "sp", "sp_name")
    ),
    required_columns = c("plot_name", "tag")
  ),
  subplot_features = c("plot"),
  direct_columns_old = c("plot")
)

cat("\n=== TESTING COLUMN MAPPING FIX ===\n")
cat("Test data columns:", paste(names(test_data), collapse=", "), "\n\n")

# Test mapping
result <- map_user_columns(test_data, config)

cat("=== MAPPING RESULTS ===\n")
cat("Mappings:\n")
for (user_col in names(result$mappings)) {
  mapped_to <- result$mappings[user_col]
  method <- result$mapping_methods[user_col]
  confidence <- result$mapping_confidence[user_col]
  if (!is.na(confidence) && is.numeric(confidence)) {
    confidence <- round(confidence, 3)
  }
  status <- if (is.na(mapped_to)) "✗ UNMAPPED" else "✓ MAPPED"
  cat(sprintf("  %s: %s → %s (%s, confidence: %s)\n",
              status, user_col, mapped_to, method, confidence))
}

cat("\n=== KEY TESTS ===\n")
# Critical test cases
tests <- list(
  list(col = "plot", expected = "plot_name", desc = "plot→plot_name (not feature 'plot')"),
  list(col = "subplot", expected = "quadrat", desc = "subplot→quadrat (new synonym)"),
  list(col = "dbh", expected = "stem_diameter", desc = "dbh→stem_diameter (fixed merge)")
)

passed <- 0
for (test in tests) {
  actual <- result$mappings[test$col]
  if (!is.na(actual) && actual == test$expected) {
    cat("✓ PASS:", test$desc, "\n")
    passed <- passed + 1
  } else {
    cat("✗ FAIL:", test$desc,
        "\n       Expected:", test$expected, "but got:", actual, "\n")
  }
}

cat(sprintf("\nResult: %d/%d critical tests passed\n", passed, length(tests)))
