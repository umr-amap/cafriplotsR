# Check what traits actually exist in the database

library(CafriplotsR)

# Connect to database
con <- call.mydb()

# Query traitlist for all traits
all_traits <- DBI::dbGetQuery(con, "SELECT DISTINCT trait FROM traitlist ORDER BY trait")

cat("Total traits in database:", nrow(all_traits), "\n\n")

# Check if stem_diameter exists
if ("stem_diameter" %in% all_traits$trait) {
  cat("✓ 'stem_diameter' IS in the database\n")
} else {
  cat("✗ 'stem_diameter' is NOT in the database\n")
}

# Look for similar trait names
cat("\nTraits with 'stem' or 'diam' in the name:\n")
matching <- all_traits$trait[grepl("stem|diam", all_traits$trait, ignore.case = TRUE)]
if (length(matching) > 0) {
  for (t in matching) {
    cat("  -", t, "\n")
  }
} else {
  cat("  (none found)\n")
}

# Show first 20 traits
cat("\nFirst 20 traits in database:\n")
print(head(all_traits$trait, 20))

# Clean up
DBI::dbDisconnect(con)
