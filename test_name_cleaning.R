# Test script for taxonomic name cleaning
# Run this to verify the clean_taxonomic_name() function works correctly

library(plotsdatabase)

# Test cases
test_names <- c(
  "Fabaceae sp.",
  "Fabaceae sp",
  "Garcinia spp.",
  "Brachystegia spp",
  "Garcinia cf. kola",
  "Garcinia cf kola",
  "Gilbertiodendron aff. dewevrei",
  "Pentaclethra ? macrophylla",
  "Strombosia  cf.  glaucescens",
  "Dialium sp.",
  "Normal name without markers",
  "Fabaceae",  # Should not change
  "Garcinia kola"  # Should not change
)

cat("\n=== Testing clean_taxonomic_name() ===\n\n")

for (name in test_names) {
  cleaned <- clean_taxonomic_name(name)
  changed <- if (name != cleaned) " ✓ CLEANED" else ""
  cat(sprintf("%-40s → %-40s%s\n",
              shQuote(name),
              shQuote(cleaned),
              changed))
}

cat("\n=== Testing with match_taxonomic_names() ===\n\n")

# Test that matching works with these patterns
test_match_names <- c(
  "Fabaceae sp.",
  "Garcinia cf. kola"
)

cat("Testing matching with cleaned names:\n")
for (name in test_match_names) {
  cat("\nInput:", shQuote(name), "\n")
  matches <- match_taxonomic_names(name, max_matches = 3, verbose = FALSE)
  if (nrow(matches) > 0 && !is.na(matches$idtax_n[1])) {
    cat("  → Original input preserved:", shQuote(matches$input_name[1]), "\n")
    cat("  → Best match:", shQuote(matches$matched_name[1]), "\n")
    cat("  → Match score:", round(matches$match_score[1], 3), "\n")
  } else {
    cat("  → No match found\n")
  }
}

cat("\n=== Test complete ===\n")
