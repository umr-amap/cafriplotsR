# Test Script: Hierarchy Consistency Functions
# Demonstrates the consistency check and update functions

library(CafriplotsR)

con <- call.mydb.taxa(use_env_credentials = TRUE)

cat("\n========================================\n")
cat("TEST: Hierarchy Consistency Functions\n")
cat("========================================\n\n")

# Test 1: Check current consistency
cat("Test 1: Checking current hierarchy consistency...\n")
cat("─────────────────────────────────────────\n")
issues <- check_hierarchy_consistency(con, limit = 20)

if (is.null(issues)) {
  cat("\n✓ All tests passed - hierarchy is consistent!\n")
} else {
  cat("\n⚠ Found inconsistencies (showing first few):\n")
  for (issue_type in names(issues)) {
    cat(sprintf("\n%s:\n", issue_type))
    print(head(issues[[issue_type]], 5))
  }

  cat("\n\nWould you like to fix these? (This will update flat columns to match id_parent)\n")
  response <- readline("Fix automatically? (y/n): ")

  if (tolower(response) == "y") {
    cat("\nFixing inconsistencies...\n")
    check_hierarchy_consistency(con, fix = TRUE)

    cat("\nRe-checking consistency...\n")
    check_hierarchy_consistency(con, limit = 20)
  }
}

# Test 2: Demonstrate safe parent update (dry run - doesn't execute)
cat("\n\n========================================\n")
cat("Test 2: Safe Parent Update (Demo)\n")
cat("========================================\n")

cat("\nExample usage of update_taxon_parent():\n")
cat("
# Move a species to different genus:
update_taxon_parent(
  idtax_n = 12345,         # Species to move
  new_parent_id = 67890,   # New parent genus
  con = con
)

# This updates BOTH:
# - id_parent = 67890
# - tax_gen = (genus name from parent 67890)
")

cat("\n✓ Consistency functions are ready to use!\n")

cleanup_connections()

cat("\n========================================\n")
cat("COMPLETE\n")
cat("========================================\n")
