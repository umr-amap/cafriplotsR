# Test script for taxonomic name matching functions
# Testing parse_taxonomic_name() and match_taxonomic_names()

# Load development version
devtools::load_all()

library(dplyr)

# Test 1: Name parsing
cat("\n=== TEST 1: Name Parsing ===\n")

test_names <- c(
  "Pericopsis elata",
  "Garcinia kola",
  "Terminalia superba var. superba",
  "Coffea canephora Pierre ex A.Froehner",
  "Cola acuminata (P.Beauv.) Schott & Endl.",
  "Gilbertiodendron dewevrei"
)

cat("\nTesting parse_taxonomic_name():\n")
for (name in test_names) {
  parsed <- parse_taxonomic_name(name)
  cat(sprintf("\nInput: %s\n", name))
  cat(sprintf("  Genus: %s\n", parsed$genus))
  cat(sprintf("  Species: %s\n", parsed$species))
  cat(sprintf("  Infraspecific: %s\n", ifelse(is.na(parsed$infraspecific), "NA", parsed$infraspecific)))
  cat(sprintf("  Name without author: %s\n", parsed$full_name_no_auth))
}

# Test 2: Exact matching (should find perfect matches)
cat("\n\n=== TEST 2: Exact Matching ===\n")

exact_test_names <- c(
  "Pericopsis elata",
  "Garcinia kola"
)

cat("\nTesting exact matches:\n")
exact_results <- match_taxonomic_names(
  exact_test_names,
  method = "exact",
  max_matches = 5,
  verbose = TRUE
)

if (!is.null(exact_results) && nrow(exact_results) > 0) {
  print(exact_results %>%
    select(input_name, match_rank, match_score, match_method, tax_gen, tax_esp, tax_fam, idtax_n))
} else {
  cat("No exact matches found\n")
}

# Test 3: Fuzzy matching with misspellings
cat("\n\n=== TEST 3: Fuzzy Matching (with misspellings) ===\n")

fuzzy_test_names <- c(
  "Periopsis elata",      # Missing 'c' in Pericopsis
  "Garcinea kola",        # Extra 'e' in Garcinia
  "Gilbertiodendron dewevrei"  # Correct (should still work)
)

cat("\nTesting fuzzy matches with typos:\n")
fuzzy_results <- match_taxonomic_names(
  fuzzy_test_names,
  method = "fuzzy",
  max_matches = 3,
  min_similarity = 0.7,
  verbose = TRUE
)

if (!is.null(fuzzy_results) && nrow(fuzzy_results) > 0) {
  print(fuzzy_results %>%
    select(input_name, match_rank, match_score, match_method, tax_gen, tax_esp, tax_fam))
} else {
  cat("No fuzzy matches found\n")
}

# Test 4: Genus-constrained matching (KEY TEST)
cat("\n\n=== TEST 4: Genus-Constrained Matching ===\n")

genus_test_names <- c(
  "Garcinia mangostana",   # Real species but may not be in database
  "Terminalia catappa",    # Real species
  "Cola nitida"            # Real species
)

cat("\nTesting genus-constrained matching:\n")
genus_results <- match_taxonomic_names(
  genus_test_names,
  method = "genus_constrained",
  max_matches = 5,
  min_similarity = 0.7,
  verbose = TRUE
)

if (!is.null(genus_results) && nrow(genus_results) > 0) {
  print(genus_results %>%
    select(input_name, match_rank, match_score, match_method, tax_gen, tax_esp, tax_fam))
} else {
  cat("No genus-constrained matches found\n")
}

# Test 5: Hierarchical matching (auto strategy)
cat("\n\n=== TEST 5: Hierarchical Matching (Auto Strategy) ===\n")

mixed_test_names <- c(
  "Pericopsis elata",           # Exact match expected
  "Garcinea mangostana",        # Genus typo + species may not exist
  "Coffea arabica",             # Exact or close match expected
  "Somethingwrong badname"      # Should fail or return low similarity
)

cat("\nTesting hierarchical matching strategy:\n")
hierarchical_results <- match_taxonomic_names(
  mixed_test_names,
  method = "auto",  # Will try exact → genus_constrained → fuzzy
  max_matches = 3,
  min_similarity = 0.6,
  verbose = TRUE
)

if (!is.null(hierarchical_results) && nrow(hierarchical_results) > 0) {
  print(hierarchical_results %>%
    select(input_name, match_rank, match_score, match_method, tax_gen, tax_esp, tax_fam))
} else {
  cat("No hierarchical matches found\n")
}

# Test 6: With synonyms
cat("\n\n=== TEST 6: Including Synonyms ===\n")

cat("\nTesting with synonym resolution:\n")
synonym_results <- match_taxonomic_names(
  "Pericopsis elata",
  method = "exact",
  include_synonyms = TRUE,
  verbose = TRUE
)

if (!is.null(synonym_results) && nrow(synonym_results) > 0) {
  print(synonym_results %>%
    select(input_name, match_rank, tax_gen, tax_esp, idtax_n, idtax_good_n, is_synonym))
} else {
  cat("No matches with synonyms found\n")
}

cat("\n\n=== ALL TESTS COMPLETED ===\n")
