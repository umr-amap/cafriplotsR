# Interactive examples for taxonomic name matching
# These require database connection - run manually in RStudio

library(plotsdatabase)
library(dplyr)

# Example 1: Parse taxonomic names (no database needed)
# =====================================================

cat("\n=== Example 1: Name Parsing ===\n")

# Parse various name formats
parse_taxonomic_name("Pericopsis elata")
parse_taxonomic_name("Terminalia superba var. superba")
parse_taxonomic_name("Coffea canephora Pierre ex A.Froehner")
parse_taxonomic_name("Brachystegia")  # Genus only


# Example 2: Exact matching
# ==========================

cat("\n=== Example 2: Exact Matching ===\n")

# Find exact matches for correct names
exact_matches <- match_taxonomic_names(
  c("Pericopsis elata", "Garcinia kola"),
  method = "exact",
  max_matches = 5
)

print(exact_matches)


# Example 3: Fuzzy matching with typos
# =====================================

cat("\n=== Example 3: Fuzzy Matching ===\n")

# Names with intentional typos
typo_names <- c(
  "Periopsis elata",      # Missing 'c' in Pericopsis
  "Garcinea kola",        # Extra 'e' in Garcinia
  "Gilbertiodendron dewevrei"  # Correct (should still match)
)

fuzzy_matches <- match_taxonomic_names(
  typo_names,
  method = "fuzzy",
  max_matches = 3,
  min_similarity = 0.7
)

print(fuzzy_matches)


# Example 4: Genus-constrained matching (KEY INNOVATION)
# =======================================================

cat("\n=== Example 4: Genus-Constrained Matching ===\n")

# Names where genus is correct but species might be wrong or unknown in database
genus_test <- c(
  "Garcinia mangostana",   # Real species, may not be in Central African database
  "Terminalia catappa",    # Real species
  "Cola nitida"            # Real species
)

genus_matches <- match_taxonomic_names(
  genus_test,
  method = "genus_constrained",
  max_matches = 5,
  min_similarity = 0.7
)

print(genus_matches)


# Example 5: Hierarchical (auto) strategy
# ========================================

cat("\n=== Example 5: Hierarchical Matching (Auto) ===\n")

# Mixed quality names - let the function choose best strategy
mixed_names <- c(
  "Pericopsis elata",           # Perfect - should get exact match
  "Garcinea mangostana",        # Genus typo + species may not exist
  "Coffea arabica",             # May or may not be in database
  "Brachystegia laurentii",     # Common Central African species
  "Somethingwrong badname"      # Should fail gracefully
)

auto_matches <- match_taxonomic_names(
  mixed_names,
  method = "auto",  # Will try exact → genus_constrained → fuzzy
  max_matches = 3,
  min_similarity = 0.6
)

print(auto_matches)

# View top match for each name
auto_matches %>%
  filter(match_rank == 1) %>%
  select(input_name, matched_name, match_method, match_score, genus, species, family)


# Example 6: With synonym information
# ====================================

cat("\n=== Example 6: Including Synonyms ===\n")

synonym_matches <- match_taxonomic_names(
  "Pericopsis elata",
  method = "exact",
  include_synonyms = TRUE,
  max_matches = 5
)

print(synonym_matches %>%
        select(input_name, matched_name, is_synonym, accepted_name, idtax_n, idtax_good_n))


# Example 7: Batch processing with data frame
# =============================================================================

cat("\n=== Example 7: Batch Processing ===\n")

# Create sample data frame
sample_data <- tibble(
  plot_id = c(1, 1, 2, 2, 3),
  tree_id = c("A01", "A02", "B01", "B02", "C01"),
  species_name = c(
    "Pericopsis elata",
    "Garcinea kola",  # Typo
    "Brachystegia laurentii",
    "Unknown species",
    "Julbernardia seretii"
  )
)

# Use batch function - adds columns with best match for each name
matched_data <- standardize_taxonomic_batch(
  data = sample_data,
  name_column = "species_name",
  method = "auto",
  min_similarity = 0.7
)

print(matched_data)

# View key matching columns
matched_data %>%
  select(tree_id, species_name, matched_name, match_method, match_score, match_genus, match_family)


# Example 8: Batch processing with all matches (for review)
# ==========================================================

cat("\n=== Example 8: Batch with All Matches ===\n")

# Keep all matches for manual review
all_matches_data <- standardize_taxonomic_batch(
  data = sample_data,
  name_column = "species_name",
  method = "auto",
  min_similarity = 0.6,
  keep_all_matches = TRUE
)

# View all suggestions for names with typos
all_matches_data %>%
  filter(species_name == "Garcinea kola") %>%
  select(species_name, match_rank, matched_name, match_score, match_method)


cat("\n=== Examples Complete ===\n")
cat("\nNext steps:\n")
cat("  1. Rebuild Shiny app with modular structure\n")
cat("  2. Add bilingual support (EN/FR)\n")
cat("  3. Implement file upload and direct data modes\n")
