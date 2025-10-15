# Test script for parse_taxonomic_name() function
# This can run without database connection

# Load development version
devtools::load_all()

cat("\n========================================\n")
cat("   TAXONOMIC NAME PARSING TESTS\n")
cat("========================================\n\n")

# Test cases covering various name formats
test_cases <- list(
  list(
    name = "Pericopsis elata",
    expected_genus = "Pericopsis",
    expected_species = "elata",
    expected_infra = NA_character_,
    description = "Simple binomial"
  ),
  list(
    name = "Garcinia kola",
    expected_genus = "Garcinia",
    expected_species = "kola",
    expected_infra = NA_character_,
    description = "Simple binomial"
  ),
  list(
    name = "Terminalia superba var. superba",
    expected_genus = "Terminalia",
    expected_species = "superba",
    expected_infra = "var. superba",
    description = "Name with variety"
  ),
  list(
    name = "Coffea canephora Pierre ex A.Froehner",
    expected_genus = "Coffea",
    expected_species = "canephora",
    expected_infra = "Pierre ex A.Froehner",
    description = "Name with author (captured as infraspecific)"
  ),
  list(
    name = "Cola acuminata (P.Beauv.) Schott & Endl.",
    expected_genus = "Cola",
    expected_species = "acuminata",
    expected_infra = "(P.Beauv.) Schott & Endl.",
    description = "Name with complex author notation"
  ),
  list(
    name = "Gilbertiodendron dewevrei",
    expected_genus = "Gilbertiodendron",
    expected_species = "dewevrei",
    expected_infra = NA_character_,
    description = "Simple binomial with long genus"
  ),
  list(
    name = "Brachystegia",
    expected_genus = "Brachystegia",
    expected_species = NA_character_,
    expected_infra = NA_character_,
    description = "Genus only"
  ),
  list(
    name = "Anthonotha macrophylla var. oblongifolia",
    expected_genus = "Anthonotha",
    expected_species = "macrophylla",
    expected_infra = "var. oblongifolia",
    description = "Name with variety"
  ),
  list(
    name = "garcinia kola",
    expected_genus = "Garcinia",  # Should capitalize
    expected_species = "kola",
    expected_infra = NA_character_,
    description = "Lowercase input (should capitalize genus)"
  ),
  list(
    name = "",
    expected_genus = NA_character_,
    expected_species = NA_character_,
    expected_infra = NA_character_,
    description = "Empty string"
  ),
  list(
    name = NA_character_,
    expected_genus = NA_character_,
    expected_species = NA_character_,
    expected_infra = NA_character_,
    description = "NA input"
  )
)

# Run tests
n_tests <- length(test_cases)
n_passed <- 0
n_failed <- 0

for (i in seq_along(test_cases)) {
  test <- test_cases[[i]]

  cat(sprintf("Test %d/%d: %s\n", i, n_tests, test$description))
  cat(sprintf("  Input: '%s'\n", ifelse(is.na(test$name), "NA", test$name)))

  parsed <- parse_taxonomic_name(test$name)

  # Check results
  genus_ok <- identical(parsed$genus, test$expected_genus)
  species_ok <- identical(parsed$species, test$expected_species)
  infra_ok <- identical(parsed$infraspecific, test$expected_infra)

  all_ok <- genus_ok && species_ok && infra_ok

  if (all_ok) {
    cat("  ✓ PASS\n")
    n_passed <- n_passed + 1
  } else {
    cat("  ✗ FAIL\n")
    n_failed <- n_failed + 1

    if (!genus_ok) {
      cat(sprintf("    Genus: expected '%s', got '%s'\n",
                  test$expected_genus, parsed$genus))
    }
    if (!species_ok) {
      cat(sprintf("    Species: expected '%s', got '%s'\n",
                  test$expected_species, parsed$species))
    }
    if (!infra_ok) {
      cat(sprintf("    Infraspecific: expected '%s', got '%s'\n",
                  test$expected_infra, parsed$infraspecific))
    }
  }

  cat(sprintf("    Parsed: genus='%s', species='%s', infra='%s'\n",
              ifelse(is.na(parsed$genus), "NA", parsed$genus),
              ifelse(is.na(parsed$species), "NA", parsed$species),
              ifelse(is.na(parsed$infraspecific), "NA", parsed$infraspecific)))
  cat(sprintf("    Full name: '%s'\n\n", parsed$full_name_no_auth))
}

# Summary
cat("========================================\n")
cat(sprintf("SUMMARY: %d/%d tests passed (%.1f%%)\n",
            n_passed, n_tests, 100 * n_passed / n_tests))
if (n_failed > 0) {
  cat(sprintf("         %d tests FAILED\n", n_failed))
}
cat("========================================\n\n")

if (n_failed == 0) {
  cat("✓ All parsing tests passed successfully!\n\n")
  cat("Next steps:\n")
  cat("  1. Test match_taxonomic_names() interactively with database connection\n")
  cat("  2. Create batch processing function for data frames\n")
  cat("  3. Build modular Shiny app for taxonomic standardization\n\n")
} else {
  cat("✗ Some tests failed. Please review and fix the parse_taxonomic_name() function.\n\n")
}
