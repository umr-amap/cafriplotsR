# Create Missing Species Entries for Infraspecific Taxa
#
# Problem: Some infraspecific taxa (var., subsp., f.) don't have a parent species
# entry in table_taxa. This script creates those missing species entries.

library(CafriplotsR)
library(DBI)
library(dplyr)

con <- call.mydb.taxa(use_env_credentials = TRUE)

# Get actual connection
if (inherits(con, "Pool")) {
  actual_con <- pool::poolCheckout(con)
} else {
  actual_con <- con
}

cat("\n========================================\n")
cat("CREATE MISSING SPECIES FOR INFRASPECIFIC TAXA\n")
cat("========================================\n\n")

# Find infraspecific taxa without a parent species
cat("Step 1: Identifying infraspecific taxa without parent species...\n")

# Get all distinct species (genus + species + family) from infraspecific taxa
# that don't have a species-level parent
missing_species_sql <- "
  WITH infraspecific_taxa AS (
    SELECT DISTINCT tax_gen, tax_esp, tax_fam, tax_order, tax_famclass, id_tax_famclass
    FROM table_taxa
    WHERE tax_level = 'infraspecific'
      AND id_parent IS NULL
      AND tax_esp IS NOT NULL  -- Must have species epithet
  )
  SELECT i.*
  FROM infraspecific_taxa i
  WHERE NOT EXISTS (
    SELECT 1 FROM table_taxa s
    WHERE s.tax_gen = i.tax_gen
      AND s.tax_esp = i.tax_esp
      AND s.tax_fam = i.tax_fam
      AND s.tax_level = 'species'
  )
  ORDER BY i.tax_gen, i.tax_esp
"

missing_species <- DBI::dbGetQuery(actual_con, missing_species_sql)

cat("Found", nrow(missing_species), "species that need to be created\n\n")

if (nrow(missing_species) == 0) {
  cat("✓ All infraspecific taxa have parent species!\n")
} else {

  cat("Sample of species to create:\n")
  print(head(missing_species, 20))

  cat("\n\nStep 2: Creating species entries...\n")
  cat("Press Enter to continue or Ctrl+C to abort: ")
  readline()

  created_count <- 0
  failed_count <- 0

  for (i in 1:nrow(missing_species)) {
    species_row <- missing_species[i, ]

    tryCatch({
      # Create new species entry
      # Note: dbWriteTable handles transactions internally, no need for explicit BEGIN/COMMIT
      new_species <- tibble::tibble(
        tax_gen = species_row$tax_gen,
        tax_esp = species_row$tax_esp,
        tax_fam = species_row$tax_fam,
        tax_order = species_row$tax_order,
        tax_famclass = species_row$tax_famclass,
        id_tax_famclass = species_row$id_tax_famclass,
        tax_rank01 = NA_character_,
        tax_nam01 = NA_character_,
        tax_rank02 = NA_character_,
        tax_nam02 = NA_character_,
        tax_source = "HMIG",  # Mark as created by hierarchy migration (max 5 chars)
        tax_level = "species",
        tax_rank = NA_character_,
        tax_rankinf = "ESP",
        tax_rankesp = "ESP",
        idtax_good_n = NA_integer_,
        morpho_species = FALSE
      )

      # Add modification fields
      new_species <- .add_modif_field(new_species)
      new_species <- new_species %>%
        dplyr::rename(
          data_modif_m = date_modif_m,
          data_modif_y = date_modif_y,
          data_modif_d = date_modif_d
        )

      # Find parent genus entry for id_parent
      parent_genus_query <- sprintf("
        SELECT idtax_n FROM table_taxa
        WHERE tax_gen = '%s'
          AND tax_fam = '%s'
          AND tax_level = 'genus'
        LIMIT 1
      ", species_row$tax_gen, species_row$tax_fam)

      parent_genus <- DBI::dbGetQuery(actual_con, parent_genus_query)

      if (nrow(parent_genus) > 0) {
        new_species$id_parent <- as.integer(parent_genus$idtax_n[1])
      } else {
        # No genus parent found - will link later or create genus if needed
        new_species$id_parent <- NA_integer_
      }

      # Insert into database (dbWriteTable commits automatically)
      DBI::dbWriteTable(actual_con, "table_taxa", new_species,
                       append = TRUE, row.names = FALSE)

      created_count <- created_count + 1

      if (created_count %% 100 == 0) {
        cat(sprintf("  Created %d/%d species entries...\n", created_count, nrow(missing_species)))
      }

    }, error = function(e) {
      # Check if it's a duplicate error (might be OK)
      if (grepl("duplicate|unique", e$message, ignore.case = TRUE)) {
        # Duplicate - likely created in parallel, skip
        if (failed_count == 0) {
          cat("\n  (Some duplicates detected - this is OK, means species was just created)\n")
        }
      } else {
        failed_count <- failed_count + 1
        if (failed_count <= 5) {
          cat(sprintf("\n  ✗ Failed to create: %s %s (%s)\n",
                      species_row$tax_gen, species_row$tax_esp, e$message))
        }
      }
    })
  }

  cat(sprintf("\n✓ Created %d species entries\n", created_count))
  if (failed_count > 0) {
    cat(sprintf("⚠ Failed to create %d entries (check errors above)\n", failed_count))
  }

  cat("\n\nStep 3: Now linking infraspecific taxa to their parent species...\n")

  # Link infraspecific to newly created species
  tryCatch({
    DBI::dbExecute(actual_con, "BEGIN;")

    link_sql <- "
      UPDATE table_taxa child
      SET id_parent = (
        SELECT parent.idtax_n FROM table_taxa parent
        WHERE parent.tax_gen = child.tax_gen
          AND parent.tax_fam = child.tax_fam
          AND parent.tax_esp = child.tax_esp
          AND parent.tax_level = 'species'
        LIMIT 1
      )
      WHERE child.tax_level = 'infraspecific'
        AND child.id_parent IS NULL;
    "

    result <- DBI::dbExecute(actual_con, link_sql)

    DBI::dbExecute(actual_con, "COMMIT;")

    cat(sprintf("✓ Linked %d infraspecific taxa to parent species\n", result))

  }, error = function(e) {
    tryCatch(DBI::dbExecute(actual_con, "ROLLBACK;"), error = function(e2) {})
    cat("✗ Linking failed:", e$message, "\n")
  })

  cat("\n\nStep 4: Verification...\n")

  # Count remaining unlinked
  remaining <- DBI::dbGetQuery(actual_con, "
    SELECT COUNT(*) as n
    FROM table_taxa
    WHERE tax_level = 'infraspecific' AND id_parent IS NULL
  ")$n[1]

  cat("Remaining unlinked infraspecific taxa:", remaining, "\n")

  if (remaining == 0) {
    cat("\n✓ SUCCESS! All infraspecific taxa are now linked!\n")
  } else {
    cat("\n⚠ Some infraspecific taxa still unlinked.\n")
    cat("These may be:\n")
    cat("  - Taxa with tax_esp = NULL (invalid data)\n")
    cat("  - Taxa whose genus parent is also missing\n")
    cat("Run verify_hierarchy_integrity() for full report.\n")
  }
}

# Clean up
if (inherits(con, "Pool")) {
  pool::poolReturn(actual_con)
}

cat("\n========================================\n")
cat("COMPLETE\n")
cat("========================================\n")
