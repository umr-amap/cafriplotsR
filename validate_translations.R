# Validate translation.json structure and count entries

library(jsonlite)

cat("Validating translation.json...\n\n")

# Read JSON (use simplifyDataFrame = FALSE to preserve structure)
json_text <- readLines("inst/translations/translation.json", warn = FALSE)
json <- fromJSON(paste(json_text, collapse = "\n"), simplifyDataFrame = FALSE)

# Check structure
if (!is.list(json)) {
  stop("JSON is not a list")
}

if (!"languages" %in% names(json)) {
  stop("Missing 'languages' field")
}

if (!"translation" %in% names(json)) {
  stop("Missing 'translation' field")
}

# Count entries
languages <- json$languages
translations <- json$translation

cat(sprintf("✓ JSON structure is valid\n"))
cat(sprintf("✓ Languages: %s\n", paste(languages, collapse = ", ")))
cat(sprintf("✓ Total translation entries: %d\n\n", length(translations)))

# Check for duplicates
en_texts <- character(length(translations))
for (i in seq_along(translations)) {
  en_texts[i] <- translations[[i]]$en
}

duplicates <- en_texts[duplicated(en_texts)]
if (length(duplicates) > 0) {
  cat(sprintf("⚠ Warning: Found %d duplicate English key(s):\n", length(duplicates)))
  for (dup in unique(duplicates)) {
    cat(sprintf("  - %s\n", dup))
  }
} else {
  cat("✓ No duplicate keys found\n")
}

# Check for missing translations
missing_fr <- 0
for (i in seq_along(translations)) {
  if (is.null(translations[[i]]$fr) || translations[[i]]$fr == "") {
    missing_fr <- missing_fr + 1
  }
}

if (missing_fr > 0) {
  cat(sprintf("⚠ Warning: %d entries missing French translation\n", missing_fr))
} else {
  cat("✓ All entries have French translations\n")
}

cat("\n✓ Validation complete!\n")
