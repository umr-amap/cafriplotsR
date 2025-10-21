# Script to Add tax_level Field to table_taxa
#
# This script adds a new column 'tax_level' to the table_taxa table in the
# rainbio database, which explicitly indicates the taxonomic level of each entry.
#
# Author: Generated for plotsdatabase package
# Date: 2025-10-20

library(DBI)
library(dplyr)
library(plotsdatabase)

# Connect to taxa database
mydb_taxa <- call.mydb.taxa()

cat("=" , rep("=", 60), "=\n", sep = "")
cat("Adding tax_level Field to table_taxa\n")
cat("=" , rep("=", 60), "=\n\n", sep = "")

# Step 1: Check if column already exists
cat("Step 1: Checking if tax_level column already exists...\n")
existing_cols <- DBI::dbListFields(mydb_taxa, "table_taxa")

if ("tax_level" %in% existing_cols) {
  cat("✓ tax_level column already exists\n\n")
  cat("Do you want to recreate it? (y/n): ")
  response <- readline()

  if (tolower(response) != "y") {
    cat("Exiting without changes.\n")
    DBI::dbDisconnect(mydb_taxa)
    stop("User chose not to recreate existing column", call. = FALSE)
  }

  cat("Dropping existing tax_level column...\n")
  DBI::dbExecute(mydb_taxa, "ALTER TABLE table_taxa DROP COLUMN IF EXISTS tax_level")
  cat("✓ Dropped existing column\n\n")
} else {
  cat("✓ Column does not exist, will create it\n\n")
}

# Step 2: Add the tax_level column
cat("Step 2: Adding tax_level column...\n")
DBI::dbExecute(mydb_taxa, "ALTER TABLE table_taxa ADD COLUMN tax_level VARCHAR(50)")
cat("✓ Column added\n\n")

# Step 3: Populate the tax_level column based on which fields are populated
cat("Step 3: Populating tax_level values...\n")

# The logic:
# - If tax_nam01 or tax_nam02 is not null/empty: infraspecific (variety, subspecies, etc.)
# - Else if tax_esp is not null/empty: species
# - Else if tax_gen is not null/empty: genus
# - Else if tax_fam is not null/empty: family
# - Else: higher (for order, class, etc.)

sql_update <- "
UPDATE table_taxa
SET tax_level = CASE
  -- Infraspecific level (has infraspecific names)
  WHEN tax_nam01 IS NOT NULL AND tax_nam01 != '' THEN 'infraspecific'
  WHEN tax_nam02 IS NOT NULL AND tax_nam02 != '' THEN 'infraspecific'

  -- Species level (has species epithet but no infraspecific)
  WHEN tax_esp IS NOT NULL AND tax_esp != '' THEN 'species'

  -- Genus level (has genus but no species)
  WHEN tax_gen IS NOT NULL AND tax_gen != '' THEN 'genus'

  -- Family level (has family but no genus)
  WHEN tax_fam IS NOT NULL AND tax_fam != '' THEN 'family'
  
  WHEN tax_order IS NOT NULL AND tax_order != '' THEN 'order'

  -- Higher taxonomic levels (order, class, etc.)
  ELSE 'higher'
END
"

rows_updated <- DBI::dbExecute(mydb_taxa, sql_update)
cat("✓ Updated", rows_updated, "rows\n\n")

# Step 4: Verify the results
cat("Step 4: Verifying results...\n")

summary_sql <- "
SELECT
  tax_level,
  COUNT(*) as n_records,
  ROUND(COUNT(*) * 100.0 / SUM(COUNT(*)) OVER (), 2) as percentage
FROM table_taxa
GROUP BY tax_level
ORDER BY
  CASE tax_level
    WHEN 'infraspecific' THEN 1
    WHEN 'species' THEN 2
    WHEN 'genus' THEN 3
    WHEN 'family' THEN 4
    WHEN 'order' THEN 5
    WHEN 'higher' THEN 6
    ELSE 7
  END
"

summary <- DBI::dbGetQuery(mydb_taxa, summary_sql)

cat("\nTaxonomic Level Distribution:\n")
cat("─────────────────────────────────────────\n")
print(summary, row.names = FALSE)
cat("\n")

# Step 5: Show examples of each level
cat("Step 5: Example entries for each level:\n\n")

for (level in c("infraspecific", "species", "genus", "family", "order", "higher")) {
  cat("Examples of '", level, "' level:\n", sep = "")

  example_sql <- paste0("
    SELECT
      idtax_n,
      tax_order,
      tax_fam,
      tax_gen,
      tax_esp,
      tax_rank01,
      tax_nam01,
      tax_level
    FROM table_taxa
    WHERE tax_level = '", level, "'
    LIMIT 3
  ")

  examples <- DBI::dbGetQuery(mydb_taxa, example_sql)

  if (nrow(examples) > 0) {
    print(examples, row.names = FALSE)
  } else {
    cat("  (No records found)\n")
  }
  cat("\n")
}

# Step 6: Create an index on tax_level for performance
cat("Step 6: Creating index on tax_level for query performance...\n")

index_sql <- "CREATE INDEX IF NOT EXISTS idx_table_taxa_tax_level ON table_taxa(tax_level)"
DBI::dbExecute(mydb_taxa, index_sql)
cat("✓ Index created\n\n")

# Step 7: Test queries
cat("Step 7: Testing queries with new tax_level field...\n\n")

cat("Query 1: Get all genus-level taxa named 'Acacia'\n")
test1 <- dplyr::tbl(mydb_taxa, "table_taxa") %>%
  dplyr::filter(tax_level == "genus", tax_gen == "Acacia") %>%
  dplyr::select(idtax_n, tax_fam, tax_gen, tax_level) %>%
  dplyr::collect()
cat("  Result: Found", nrow(test1), "records\n\n")

cat("Query 2: Get all family-level taxa\n")
test2 <- dplyr::tbl(mydb_taxa, "table_taxa") %>%
  dplyr::filter(tax_level == "family") %>%
  dplyr::select(idtax_n, tax_fam, tax_level) %>%
  head(5) %>%
  dplyr::collect()
cat("  Result: Found records (showing first 5):\n")
print(test2, row.names = FALSE)
cat("\n")

cat("Query 3: Get all species-level taxa in family 'Fabaceae' (first 5)\n")
test3 <- dplyr::tbl(mydb_taxa, "table_taxa") %>%
  dplyr::filter(tax_level == "species", tax_fam == "Fabaceae") %>%
  dplyr::select(idtax_n, tax_fam, tax_gen, tax_esp, tax_level) %>%
  head(5) %>%
  dplyr::collect()
cat("  Result:\n")
print(test3, row.names = FALSE)
cat("\n")

# Summary
cat("=" , rep("=", 60), "=\n", sep = "")
cat("✓ COMPLETE: tax_level field added and populated successfully!\n")
cat("=" , rep("=", 60), "=\n\n", sep = "")

cat("Usage examples:\n\n")
cat("# Get all genus-level taxa:\n")
cat("genera <- tbl(mydb_taxa, 'table_taxa') %>%\n")
cat("  filter(tax_level == 'genus') %>%\n")
cat("  collect()\n\n")

cat("# Get all family-level taxa:\n")
cat("families <- tbl(mydb_taxa, 'table_taxa') %>%\n")
cat("  filter(tax_level == 'family') %>%\n")
cat("  collect()\n\n")

cat("# Get unique families (no duplicates):\n")
cat("unique_families <- tbl(mydb_taxa, 'table_taxa') %>%\n")
cat("  filter(tax_level == 'family') %>%\n")
cat("  group_by(tax_fam) %>%\n")
cat("  filter(n() == 1) %>%\n")
cat("  collect()\n\n")

# Disconnect
DBI::dbDisconnect(mydb_taxa)
cat("✓ Database connection closed\n")
