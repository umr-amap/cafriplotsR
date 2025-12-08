# Script to merge Import Wizard translations into main translation.json
# This ensures no duplicates are added

library(jsonlite)

# Read existing translations
existing_json <- readLines("inst/translations/translation.json", warn = FALSE)
existing <- fromJSON(paste(existing_json, collapse = "\n"), simplifyDataFrame = FALSE)

# Read new translations
new_json <- readLines("inst/translations/import_wizard_translations.json", warn = FALSE)
new_trans <- fromJSON(paste(new_json, collapse = "\n"), simplifyDataFrame = FALSE)

existing_translation <- existing$translation
new_entries <- new_trans$translations

# Get existing English texts (for duplicate check)
existing_en <- character(length(existing_translation))
for (i in seq_along(existing_translation)) {
  existing_en[i] <- existing_translation[[i]]$en
}

cat(sprintf("Existing translations: %d entries\n", length(existing_translation)))
cat(sprintf("New translations to merge: %d entries\n", length(new_entries)))

# Filter out duplicates (entries where English text already exists)
to_add <- list()
duplicates <- 0

for (i in seq_along(new_entries)) {
  en_text <- new_entries[[i]]$en

  if (!en_text %in% existing_en) {
    to_add[[length(to_add) + 1]] <- new_entries[[i]]
  } else {
    duplicates <- duplicates + 1
  }
}

cat(sprintf("Found %d duplicates (skipping)\n", duplicates))
cat(sprintf("Adding %d new translation entries\n", length(to_add)))

# Merge translations
merged_translation <- c(existing_translation, to_add)

cat(sprintf("Total after merge: %d entries\n", length(merged_translation)))

# Create updated structure
updated <- list(
  languages = existing$languages,
  translation = merged_translation
)

# Write updated JSON with pretty formatting
write_json(
  updated,
  "inst/translations/translation.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

cat("\n✓ Successfully merged translations into translation.json\n")
