## ========================================
## NEW WORKFLOW: Update Specimen Identifications
## Using the modern update_records() function
## ========================================

library(CafriplotsR)
library(dplyr)
library(readxl)
library(stringr)      # For parsing collection numbers with suffixes
library(stringdist)   # For fuzzy suffix matching

# ========================================
# Step 1: Load identification data
# ========================================

# Load your Excel file with new identifications
# Expected columns:
# - id_specimen OR (colnam + colnbr) to identify specimens
# - idtax_n: new taxonomy ID (recommended) OR genus/species names
# - detby: determiner name (optional)
# - dety, detm, detd: determination date (optional)

new_ident <- read_excel("D:/MonDossierR/database.transects/Identifications_Transects_2025_part2.xlsx")

new_ident <- 
  new_ident %>% 
  rename(idtax_n = ID.dico.name)

cat("Loaded", nrow(new_ident), "identification records\n")
print(head(new_ident))

# ========================================
# Step 2: Parse collection numbers (split number and suffix)
# ========================================

# Handle cases where colnbr contains suffixes (e.g., "95bis", "424bis")
# Database stores these as separate colnbr (numeric) and suffix (character) fields

if ("colnbr" %in% names(new_ident)) {

  cat("\n--- Parsing collection numbers ---\n")

  # Extract numeric part and suffix from colnbr
  new_ident <- new_ident %>%
    mutate(
      # Extract leading digits as colnbr_numeric
      colnbr_numeric = as.numeric(stringr::str_extract(colnbr, "^[0-9]+")),
      # Extract everything after digits as suffix (if present)
      suffix_parsed = stringr::str_extract(colnbr, "[^0-9]+$"),
      # Clean up suffix (remove NA, trim whitespace)
      suffix_parsed = ifelse(is.na(suffix_parsed), "", trimws(suffix_parsed))
    )

  # Show which records have suffixes
  with_suffix <- new_ident %>% filter(suffix_parsed != "")
  if (nrow(with_suffix) > 0) {
    cat("Found", nrow(with_suffix), "records with suffixes:\n")
    print(with_suffix %>% select(colnam, colnbr, colnbr_numeric, suffix_parsed) %>% head(10))
  }

  # Check for any that couldn't be parsed
  unparsed <- new_ident %>% filter(is.na(colnbr_numeric))
  if (nrow(unparsed) > 0) {
    cat("\nWARNING:", nrow(unparsed), "records with unparseable colnbr:\n")
    print(unparsed %>% select(colnam, colnbr))
  }
}

# ========================================
# Step 3: Get specimen IDs (if not already present)
# ========================================

# If you have collector name + number instead of id_specimen
if (!"id_specimen" %in% names(new_ident) && "colnam" %in% names(new_ident)) {

  cat("\n--- Querying specimens by collector and number ---\n")

  # Query specimens to get IDs
  # Note: query_specimens returns all specimens for the collector
  specimens <- query_specimens(
    collector = unique(new_ident$colnam),
    subset_columns = FALSE,
    con = call.mydb()
  )

  cat("Retrieved", nrow(specimens), "specimens from database\n")

  # Prepare database specimens for matching
  specimens_clean <- specimens %>%
    select(colnam, colnbr, suffix, id_specimen) %>%
    # Handle NA suffixes in database (treat as empty string)
    mutate(suffix_db = ifelse(is.na(suffix), "", trimws(tolower(suffix))))

  # First try: exact match (after normalization)
  new_ident <- new_ident %>%
    mutate(suffix_normalized = trimws(tolower(suffix_parsed))) %>%
    left_join(
      specimens_clean %>% select(colnam, colnbr, suffix_db, id_specimen),
      by = c("colnam" = "colnam",
             "colnbr_numeric" = "colnbr",
             "suffix_normalized" = "suffix_db")
    )

  # For records that didn't match, try fuzzy matching on suffix
  unmatched <- new_ident %>% filter(is.na(id_specimen))

  if (nrow(unmatched) > 0) {
    cat("\n--- Attempting fuzzy suffix matching for", nrow(unmatched), "unmatched records ---\n")

    # Configurable similarity threshold (0.7 = 70% similar)
    fuzzy_threshold <- 0.7

    for (i in 1:nrow(unmatched)) {
      rec <- unmatched[i, ]

      # Find specimens with same collector and number
      candidates <- specimens_clean %>%
        filter(colnam == rec$colnam, colnbr == rec$colnbr_numeric)

      if (nrow(candidates) > 0) {
        # Calculate string distance for suffix
        candidates <- candidates %>%
          mutate(
            # Levenshtein distance
            suffix_dist = stringdist::stringdist(rec$suffix_normalized, suffix_db, method = "lv"),
            # Similarity score (0-1, higher is better)
            suffix_sim = 1 - (suffix_dist / pmax(nchar(rec$suffix_normalized), nchar(suffix_db), 1))
          ) %>%
          arrange(desc(suffix_sim))

        # Get best match
        best_match <- candidates %>% slice(1)

        if (best_match$suffix_sim >= fuzzy_threshold) {
          # Auto-accept perfect matches (case variations only)
          if (best_match$suffix_sim >= 0.99) {
            cat(sprintf(
              "  Auto-matched: %s %d '%s' → '%s' (perfect match)\n",
              rec$colnam, rec$colnbr_numeric,
              rec$suffix_parsed, best_match$suffix
            ))

            # Update the match
            new_ident$id_specimen[new_ident$colnam == rec$colnam &
                                    new_ident$colnbr_numeric == rec$colnbr_numeric &
                                    new_ident$suffix_normalized == rec$suffix_normalized] <- best_match$id_specimen

          } else {
            # Fuzzy match - show and ask for confirmation
            cat(sprintf(
              "  Possible fuzzy match: %s %d '%s' → '%s' (similarity: %.2f)\n",
              rec$colnam, rec$colnbr_numeric,
              rec$suffix_parsed, best_match$suffix,
              best_match$suffix_sim
            ))
            cat(sprintf("    ID: %d\n", best_match$id_specimen))

            response <- readline("    Accept this match? (y/n): ")
            if (tolower(trimws(response)) == "y") {
              # Update the match
              new_ident$id_specimen[new_ident$colnam == rec$colnam &
                                      new_ident$colnbr_numeric == rec$colnbr_numeric &
                                      new_ident$suffix_normalized == rec$suffix_normalized] <- best_match$id_specimen
              cat("    ✓ Accepted\n")
            } else {
              cat("    ✗ Rejected\n")
            }
          }
        }
      }
    }
  }

  # Check for missing IDs
  missing <- new_ident %>% filter(is.na(id_specimen))
  if (nrow(missing) > 0) {
    cat("\nWARNING:", nrow(missing), "specimens not found in database:\n")
    print(missing %>% select(colnam, colnbr, colnbr_numeric, suffix_parsed))

    # Try to provide helpful diagnostics
    for (i in 1:min(5, nrow(missing))) {
      rec <- missing[i,]
      cat("\nSearching for:", rec$colnam, "number:", rec$colnbr_numeric, "suffix:", rec$suffix_parsed, "\n")

      # Check what exists in database for this collector + number
      db_matches <- specimens %>%
        filter(colnam == rec$colnam, colnbr == rec$colnbr_numeric)

      if (nrow(db_matches) > 0) {
        cat("  Found in database with different suffix:\n")
        print(db_matches %>% select(colnam, colnbr, suffix, id_specimen))
      } else {
        cat("  No match found in database (collector + number)\n")
      }
    }

    cat("\nThese", nrow(missing), "specimens will be skipped.\n")
    new_ident <- new_ident %>% filter(!is.na(id_specimen))
  }

  cat("\nSuccessfully matched", nrow(new_ident), "specimens\n")
}

# ========================================
# Step 4: Resolve taxonomy IDs (if needed)
# ========================================

# If you have genus/species names instead of idtax_n
if (!"idtax_n" %in% names(new_ident) &&
    any(c("genus", "species") %in% names(new_ident))) {

  cat("\n--- Resolving taxonomy from genus/species names ---\n")

  # Get unique combinations
  unique_taxa <- new_ident %>%
    select(any_of(c("genus", "species", "family"))) %>%
    distinct()

  # Query taxa database
  taxa_matches <- query_taxa(
    genus = if ("genus" %in% names(unique_taxa)) unique_taxa$genus else NULL,
    species = if ("species" %in% names(unique_taxa)) unique_taxa$species else NULL,
    family = if ("family" %in% names(unique_taxa)) unique_taxa$family else NULL,
    check_synonymy = TRUE,
    verbose = FALSE
  )

  # Join back to get idtax_n
  new_ident <- new_ident %>%
    left_join(
      taxa_matches %>% select(any_of(c("genus", "species", "idtax_n"))),
      by = intersect(names(.), c("genus", "species"))
    )

  # Check for unresolved taxa
  unresolved <- new_ident %>% filter(is.na(idtax_n))
  if (nrow(unresolved) > 0) {
    cat("\nWARNING:", nrow(unresolved), "taxa could not be resolved:\n")
    print(unresolved %>% select(any_of(c("genus", "species"))) %>% distinct())
  }
}

# ========================================
# Step 5: Prepare data for update
# ========================================

# Select only the columns we want to update
update_data <- new_ident %>%
  select(
    id_specimen,
    any_of(c(
      "idtax_n",           # New taxonomy ID
      "detby",             # Determiner
      "dety", "detm", "detd", # Determination date
      "detvalue",          # Determination confidence
      "original_tax_name", # Original name (if changed)
      "colnam"             # Collector name (for ID lookup if needed)
    ))
  ) %>%
  filter(!is.na(id_specimen))  # Only keep specimens with valid IDs

cat("\n--- Prepared", nrow(update_data), "records for update ---\n")

# ========================================
# Step 6: DRY RUN - Preview changes
# ========================================

cat("\n========================================\n")
cat("DRY RUN - Previewing changes\n")
cat("========================================\n\n")

dry_run_result <- update_records(
  data = update_data,
  table_type = "specimens",
  execute = FALSE,        # DRY RUN - no changes made
  method = "batch",       # Use batch mode for efficiency
  con = call.mydb(),
  interactive = TRUE,     # Allow interactive metadata matching
  similarity_threshold = 0.7  # Fuzzy matching threshold for colnam
)

# Review the changes
print(dry_run_result$changes)

# ========================================
# Step 7: Execute updates (after review)
# ========================================

cat("\n========================================\n")
response <- readline("Proceed with update? (yes/no): ")
cat("========================================\n\n")

if (tolower(response) == "yes") {

  cat("\n--- Executing updates ---\n")

  result <- update_records(
    data = update_data,
    table_type = "specimens",
    execute = TRUE,         # EXECUTE - apply changes
    method = "batch",
    con = call.mydb(),
    interactive = TRUE,
    similarity_threshold = 0.7
  )

  cat("\n========================================\n")
  cat("Update completed successfully!\n")
  cat("========================================\n\n")

  # Verify changes by querying updated specimens
  updated_specimens <- query_specimens(
    id_specimen = update_data$id_specimen,
    subset_columns = FALSE,
    con = call.mydb()
  )

  cat("\n--- Sample of updated specimens ---\n")
  # Note: query_specimens returns taxonomy as tax_gen, tax_esp, tax_fam (not genus, species)
  print(updated_specimens %>%
    select(id_specimen, colnam, colnbr,
           any_of(c("tax_gen", "tax_esp", "tax_fam", "genus", "species")),
           any_of(c("detby", "dety"))) %>%
    head(10))

} else {
  cat("\nUpdate cancelled by user.\n")
}

# ========================================
# NOTES ON THE NEW WORKFLOW
# ========================================

# Benefits of using update_records():
# 1. Automatic change detection - only updates changed values
# 2. Metadata mapping - automatically resolves colnam to id_colnam
# 3. Dry run capability - preview changes before executing
# 4. Batch updates - efficient for large datasets
# 5. Automatic backups - changes saved to followup_updates_specimens
# 6. Interactive prompts - guides you through metadata resolution
# 7. Consistent with other table update patterns in the package

# Column mapping notes:
# - colnam (collector name) → automatically mapped to id_colnam via table_colnam
# - colnbr with suffixes (e.g., "95bis") → automatically split into colnbr + suffix
# - idtax_n should be pre-resolved (use query_taxa or match_tax)
# - All other columns (detby, dety, etc.) are direct updates

# Suffix handling:
# The workflow automatically handles collection numbers with suffixes:
# - "95bis" → colnbr = 95, suffix = "bis"
# - "424bis" → colnbr = 424, suffix = "bis"
# - "123" → colnbr = 123, suffix = ""
# This matches the database schema where colnbr and suffix are separate fields
#
# Fuzzy suffix matching handles spelling variations:
# 1. First tries exact match (case-insensitive, trimmed)
# 2. If no match, uses Levenshtein distance for fuzzy matching
# 3. Accepts matches with similarity >= 0.7 (70%)
# Examples:
# - "bis" matches "Bis" (100% similarity - case variation)
# - "bis" matches "biss" (75% similarity - typo)
# - "ter" matches "Ter" (100% similarity - case variation)
# This ensures robust matching even with data entry inconsistencies

# Comparison with old workflow:
# OLD: Manual .link_colnam + loop with update_ident_specimens
# NEW: Single update_records call with automatic metadata mapping + suffix parsing
