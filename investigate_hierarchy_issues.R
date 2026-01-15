# Investigation Script: Hierarchy Migration Issues
# Run this in your R console after connecting to the database

library(CafriplotsR)

con <- call.mydb.taxa()

cat("\n========================================\n")
cat("INVESTIGATION 1: ORPHANED FAMILIES (267)\n")
cat("========================================\n\n")

# Get sample of orphaned families
orphan_families <- DBI::dbGetQuery(con, "
  SELECT idtax_n, tax_fam, tax_order, tax_famclass
  FROM table_taxa
  WHERE tax_level = 'family' AND id_parent IS NULL
  ORDER BY tax_fam
  LIMIT 20
")

cat("Sample of orphaned families:\n")
print(orphan_families)

# Check if order entries exist for these families
if (nrow(orphan_families) > 0) {
  sample_order <- orphan_families$tax_order[1]
  sample_class <- orphan_families$tax_famclass[1]

  cat("\n\nChecking if order entry exists for:", sample_order, "\n")
  order_exists <- DBI::dbGetQuery(con, sprintf("
    SELECT idtax_n, tax_order, tax_famclass, tax_level
    FROM table_taxa
    WHERE tax_order = '%s' AND tax_level = 'order'
    LIMIT 5
  ", sample_order))

  if (nrow(order_exists) > 0) {
    cat("✓ Order entries exist:\n")
    print(order_exists)
  } else {
    cat("✗ No order entry found for:", sample_order, "\n")

    # Check if order exists with different tax_level
    order_other <- DBI::dbGetQuery(con, sprintf("
      SELECT idtax_n, tax_order, tax_famclass, tax_level
      FROM table_taxa
      WHERE tax_order = '%s'
      LIMIT 5
    ", sample_order))

    if (nrow(order_other) > 0) {
      cat("\nBut found order with different tax_level:\n")
      print(order_other)
    }
  }
}

# Count families by whether their order exists
cat("\n\nAnalyzing all orphaned families...\n")
orphan_analysis <- DBI::dbGetQuery(con, "
  WITH orphaned_families AS (
    SELECT tax_fam, tax_order, tax_famclass
    FROM table_taxa
    WHERE tax_level = 'family' AND id_parent IS NULL
  )
  SELECT
    COUNT(*) as n_orphaned,
    COUNT(CASE WHEN o.idtax_n IS NOT NULL THEN 1 END) as has_order_entry,
    COUNT(CASE WHEN o.idtax_n IS NULL THEN 1 END) as missing_order_entry
  FROM orphaned_families f
  LEFT JOIN (
    SELECT idtax_n, tax_order FROM table_taxa WHERE tax_level = 'order'
  ) o ON f.tax_order = o.tax_order
")

cat("Analysis of 267 orphaned families:\n")
print(orphan_analysis)


cat("\n\n========================================\n")
cat("INVESTIGATION 2: ORPHANED SPECIES (9)\n")
cat("========================================\n\n")

# Get orphaned species
orphan_species <- DBI::dbGetQuery(con, "
  SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_order, tax_famclass
  FROM table_taxa
  WHERE tax_level = 'species' AND id_parent IS NULL
")

cat("All orphaned species:\n")
print(orphan_species)

# Check if genus entries exist
if (nrow(orphan_species) > 0) {
  for (i in 1:nrow(orphan_species)) {
    sp <- orphan_species[i, ]
    cat(sprintf("\n\nChecking species: %s %s (family: %s)\n", sp$tax_gen, sp$tax_esp, sp$tax_fam))

    genus_check <- DBI::dbGetQuery(con, sprintf("
      SELECT idtax_n, tax_gen, tax_fam, tax_level
      FROM table_taxa
      WHERE tax_gen = '%s' AND tax_fam = '%s' AND tax_level = 'genus'
      LIMIT 3
    ", sp$tax_gen, sp$tax_fam))

    if (nrow(genus_check) > 0) {
      cat("✓ Genus entry exists:\n")
      print(genus_check)
    } else {
      cat("✗ No genus entry found\n")
    }
  }
}


cat("\n\n========================================\n")
cat("INVESTIGATION 3: NA TAX_LEVEL (19)\n")
cat("========================================\n\n")

# Get taxa with NA tax_level
na_level <- DBI::dbGetQuery(con, "
  SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_order, tax_famclass,
         tax_rank01, tax_nam01
  FROM table_taxa
  WHERE tax_level IS NULL
  ORDER BY idtax_n
")

cat("Taxa with NA tax_level:\n")
print(na_level)


cat("\n\n========================================\n")
cat("SUMMARY\n")
cat("========================================\n\n")

cat("1. Orphaned families: Check if their order entries exist or need creation\n")
cat("2. Orphaned species: Check if their genus entries exist with correct family match\n")
cat("3. NA tax_level: These need tax_level assigned before they can be linked\n")

cleanup_connections()
