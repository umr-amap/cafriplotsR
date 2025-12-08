# Test Script for Column Mapping Redesign
# Tests both new single-table and old two-table workflows

# Load the package from source (development mode)
devtools::load_all(".")

# Setup: Create database connection
con <- call.mydb(user = "dauby", pass = "AmapENS2024")

cat("\n")
cat("======================================================================\n")
cat("TEST 1: Single Flat Table Workflow (NON-INTERACTIVE)\n")
cat("======================================================================\n")
cat("\n")

# Create test data with mixed columns (individuals + features)
test_flat_data <- data.frame(
  Plot = c("PLOT-A", "PLOT-A", "PLOT-B"),
  TreeTag = c("1", "2", "1"),
  Species = c("Beilschmiedia mannii", "Coula edulis", "Dacryodes edulis"),
  idtax = c(12345, 67890, 11111),
  DBH = c(15.3, 22.1, 8.5),
  Height = c(12.0, 18.5, 6.2),
  stringsAsFactors = FALSE
)

cat("Test data structure:\n")
print(str(test_flat_data))
cat("\n")

# Test non-interactive mode (automatic mapping only)
cat("Testing NON-INTERACTIVE mode (automatic mapping only)...\n")
cat("\n")

result1 <- tryCatch({
  map_individual_columns(
    data = test_flat_data,
    interactive = FALSE,
    con = con
  )
}, error = function(e) {
  cat("ERROR in Test 1:\n")
  print(e)
  return(NULL)
})

if (!is.null(result1)) {
  cat("\n=== TEST 1 RESULTS ===\n")
  cat("\nIndividuals table:\n")
  print(result1$individuals)

  cat("\nFeatures table:\n")
  print(result1$features)

  cat("\nMapping info:\n")
  print(result1$mapping_info)

  # Validate results
  cat("\n=== TEST 1 VALIDATION ===\n")
  stopifnot("individuals table exists" = !is.null(result1$individuals))
  stopifnot("plot_name column exists" = "plot_name" %in% names(result1$individuals))
  stopifnot("idtax_n column exists" = "idtax_n" %in% names(result1$individuals))
  cat("✓ Test 1 PASSED\n")
} else {
  cat("✗ Test 1 FAILED\n")
}


cat("\n")
cat("======================================================================\n")
cat("TEST 2: Two Tables Workflow (BACKWARD COMPATIBILITY)\n")
cat("======================================================================\n")
cat("\n")

# Create test data using old two-table approach
individuals_data <- data.frame(
  plot_name = c("PLOT-A", "PLOT-A", "PLOT-B"),
  tag = c("1", "2", "1"),
  idtax_n = c(12345, 67890, 11111),
  original_tax_name = c("Beilschmiedia mannii", "Coula edulis", "Dacryodes edulis"),
  stringsAsFactors = FALSE
)

features_data <- data.frame(
  plot_name = c("PLOT-A", "PLOT-A", "PLOT-B"),
  tag = c("1", "2", "1"),
  stem_diameter = c(15.3, 22.1, 8.5),
  tree_height = c(12.0, 18.5, 6.2),
  stringsAsFactors = FALSE
)

cat("Individuals data:\n")
print(individuals_data)
cat("\nFeatures data:\n")
print(features_data)
cat("\n")

# Test old two-table workflow
cat("Testing TWO-TABLE workflow (backward compatibility)...\n")
cat("\n")

result2 <- tryCatch({
  map_individual_columns(
    individuals_data = individuals_data,
    features_data = features_data,
    interactive = FALSE,
    con = con
  )
}, error = function(e) {
  cat("ERROR in Test 2:\n")
  print(e)
  return(NULL)
})

if (!is.null(result2)) {
  cat("\n=== TEST 2 RESULTS ===\n")
  cat("\nIndividuals table:\n")
  print(result2$individuals)

  cat("\nFeatures table:\n")
  print(result2$features)

  # Validate results
  cat("\n=== TEST 2 VALIDATION ===\n")
  stopifnot("individuals table exists" = !is.null(result2$individuals))
  stopifnot("features table exists" = !is.null(result2$features))
  stopifnot("plot_name preserved" = all(result2$individuals$plot_name == individuals_data$plot_name))
  stopifnot("stem_diameter preserved" = all(result2$features$stem_diameter == features_data$stem_diameter))
  cat("✓ Test 2 PASSED (backward compatibility maintained)\n")
} else {
  cat("✗ Test 2 FAILED\n")
}


cat("\n")
cat("======================================================================\n")
cat("TEST 3: Error Handling - Conflicting Parameters\n")
cat("======================================================================\n")
cat("\n")

# Test that providing both 'data' and 'individuals_data' raises error
cat("Testing error when both 'data' and 'individuals_data' provided...\n")

result3 <- tryCatch({
  map_individual_columns(
    data = test_flat_data,
    individuals_data = individuals_data,
    con = con
  )
  cat("✗ Test 3 FAILED: Should have raised error\n")
  FALSE
}, error = function(e) {
  cat("✓ Expected error raised:\n")
  cat("  ", conditionMessage(e), "\n")
  cat("✓ Test 3 PASSED\n")
  TRUE
})


cat("\n")
cat("======================================================================\n")
cat("TEST 4: Error Handling - No Parameters\n")
cat("======================================================================\n")
cat("\n")

# Test that providing neither 'data' nor 'individuals_data' raises error
cat("Testing error when neither 'data' nor 'individuals_data' provided...\n")

result4 <- tryCatch({
  map_individual_columns(con = con)
  cat("✗ Test 4 FAILED: Should have raised error\n")
  FALSE
}, error = function(e) {
  cat("✓ Expected error raised:\n")
  cat("  ", conditionMessage(e), "\n")
  cat("✓ Test 4 PASSED\n")
  TRUE
})


cat("\n")
cat("======================================================================\n")
cat("TEST SUMMARY\n")
cat("======================================================================\n")
cat("\n")

all_passed <- !is.null(result1) && !is.null(result2) && result3 && result4

if (all_passed) {
  cat("✓✓✓ ALL TESTS PASSED ✓✓✓\n")
  cat("\n")
  cat("Implementation is working correctly:\n")
  cat("  ✓ Single flat table workflow (new)\n")
  cat("  ✓ Two-table workflow (backward compatible)\n")
  cat("  ✓ Non-interactive mode (automatic mapping)\n")
  cat("  ✓ Parameter validation and error handling\n")
} else {
  cat("✗✗✗ SOME TESTS FAILED ✗✗✗\n")
  cat("\nCheck output above for details.\n")
}

cat("\n")

# Cleanup
DBI::dbDisconnect(con)

cat("Tests complete.\n")
