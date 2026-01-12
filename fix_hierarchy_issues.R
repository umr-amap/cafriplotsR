# Fix Script: Hierarchy Migration Issues
# Run this AFTER investigating with investigate_hierarchy_issues.R

library(CafriplotsR)

con <- call.mydb.taxa()

cat("\n========================================\n")
cat("FIX 1: ASSIGN TAX_LEVEL TO NA TAXA\n")
cat("========================================\n\n")

cat("NOTE: Starting from this fix, all future taxa entries will automatically\n")
cat("have tax_level set via add_entry_taxa() and .add_taxa_noninteractive().\n")
cat("This fix only handles existing taxa with NA tax_level.\n\n")

# This function determines tax_level based on which fields are populated
determine_tax_level <- function(row) {
  if (!is.na(row$tax_nam01) && row$tax_nam01 != "") {
    return("infraspecific")
  } else if (!is.na(row$tax_esp) && row$tax_esp != "") {
    return("species")
  } else if (!is.na(row$tax_gen) && row$tax_gen != "") {
    return("genus")
  } else if (!is.na(row$tax_fam) && row$tax_fam != "") {
    return("family")
  } else if (!is.na(row$tax_order) && row$tax_order != "") {
    return("order")
  } else if (!is.na(row$tax_famclass) && row$tax_famclass != "") {
    return("class")
  } else {
    return(NA)
  }
}

# Get taxa with NA tax_level
na_taxa <- DBI::dbGetQuery(con, "
  SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_order, tax_famclass,
         tax_rank01, tax_nam01
  FROM table_taxa
  WHERE tax_level IS NULL
")

cat("Found", nrow(na_taxa), "taxa with NA tax_level\n\n")

if (nrow(na_taxa) > 0) {
  cat("Determining correct tax_level for each...\n")

  for (i in 1:nrow(na_taxa)) {
    row <- na_taxa[i, ]
    correct_level <- determine_tax_level(row)

    cat(sprintf("ID %d: %s -> %s\n",
                row$idtax_n,
                paste(row$tax_gen, row$tax_esp, row$tax_nam01, collapse = " "),
                correct_level))

    # Update tax_level
    if (!is.na(correct_level)) {
      DBI::dbExecute(con, sprintf("
        UPDATE table_taxa
        SET tax_level = '%s'
        WHERE idtax_n = %d
      ", correct_level, row$idtax_n))
    }
  }

  cat("\n✓ Updated tax_level for", nrow(na_taxa), "taxa\n")
} else {
  cat("No taxa with NA tax_level found - all are current!\n")
}


cat("\n\n========================================\n")
cat("FIX 2: RE-RUN LINKING FOR ORPHANS\n")
cat("========================================\n\n")

cat("Re-running migration_link_hierarchy to catch newly fixed taxa...\n\n")

# This should link taxa that now have proper tax_level or whose parents were created
result <- migration_link_hierarchy(con, dry_run = FALSE, verbose = TRUE)

cat("\n✓ Linking complete\n")


cat("\n\n========================================\n")
cat("FIX 3: CREATE MISSING ORDER ENTRIES\n")
cat("========================================\n\n")

# Check if orphaned families have missing order entries
orphan_families <- DBI::dbGetQuery(con, "
  SELECT DISTINCT f.tax_order, f.tax_famclass
  FROM table_taxa f
  WHERE f.tax_level = 'family' AND f.id_parent IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM table_taxa o
    WHERE o.tax_order = f.tax_order AND o.tax_level = 'order'
  )
")

if (nrow(orphan_families) > 0) {
  cat("Found", nrow(orphan_families), "orders that need to be created\n\n")

  for (i in 1:nrow(orphan_families)) {
    order_row <- orphan_families[i, ]
    cat(sprintf("Creating order entry: %s (class: %s)\n",
                order_row$tax_order, order_row$tax_famclass))

    # Use the internal helper from migration script
    new_id <- .create_hierarchy_entry(
      con,
      tax_order = order_row$tax_order,
      tax_famclass = order_row$tax_famclass,
      tax_level = "order"
    )

    cat(sprintf("  ✓ Created with ID: %d\n", new_id))
  }

  cat("\n✓ Created", nrow(orphan_families), "order entries\n")

  # Now re-link families to these new orders
  cat("\nRe-linking families to newly created orders...\n")
  result <- DBI::dbExecute(con, "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_order = child.tax_order
        AND parent.tax_level = 'order'
      LIMIT 1
    )
    WHERE child.tax_level = 'family'
      AND child.id_parent IS NULL;
  ")

  cat(sprintf("✓ Linked %d families to orders\n", result))
} else {
  cat("No missing order entries found\n")
}


cat("\n\n========================================\n")
cat("FIX 4: CREATE MISSING GENUS ENTRIES\n")
cat("========================================\n\n")

# Check if orphaned species have missing genus entries
orphan_species <- DBI::dbGetQuery(con, "
  SELECT DISTINCT s.tax_gen, s.tax_fam, s.tax_order, s.tax_famclass
  FROM table_taxa s
  WHERE s.tax_level = 'species' AND s.id_parent IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM table_taxa g
    WHERE g.tax_gen = s.tax_gen AND g.tax_fam = s.tax_fam AND g.tax_level = 'genus'
  )
")

if (nrow(orphan_species) > 0) {
  cat("Found", nrow(orphan_species), "genera that need to be created\n\n")

  for (i in 1:nrow(orphan_species)) {
    genus_row <- orphan_species[i, ]
    cat(sprintf("Creating genus entry: %s (family: %s)\n",
                genus_row$tax_gen, genus_row$tax_fam))

    new_id <- .create_hierarchy_entry(
      con,
      tax_gen = genus_row$tax_gen,
      tax_fam = genus_row$tax_fam,
      tax_order = genus_row$tax_order,
      tax_famclass = genus_row$tax_famclass,
      tax_level = "genus"
    )

    cat(sprintf("  ✓ Created with ID: %d\n", new_id))
  }

  cat("\n✓ Created", nrow(orphan_species), "genus entries\n")

  # Now re-link species to these new genera
  cat("\nRe-linking species to newly created genera...\n")
  result <- DBI::dbExecute(con, "
    UPDATE table_taxa child
    SET id_parent = (
      SELECT parent.idtax_n FROM table_taxa parent
      WHERE parent.tax_gen = child.tax_gen
        AND parent.tax_fam = child.tax_fam
        AND parent.tax_level = 'genus'
      LIMIT 1
    )
    WHERE child.tax_level = 'species'
      AND child.id_parent IS NULL;
  ")

  cat(sprintf("✓ Linked %d species to genera\n", result))
} else {
  cat("No missing genus entries found\n")
}


cat("\n\n========================================\n")
cat("FINAL VERIFICATION\n")
cat("========================================\n\n")

final_check <- verify_hierarchy_integrity(con)

cleanup_connections()

cat("\n✓ All fixes applied!\n")
