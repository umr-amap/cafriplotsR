# Diagnostic: Why aren't infraspecific taxa being linked?

library(CafriplotsR)
library(DBI)
library(dplyr)

con <- call.mydb.taxa(use_env_credentials = TRUE)

cat("\n========================================\n")
cat("DIAGNOSTIC: Infraspecific Linking Issue\n")
cat("========================================\n\n")

# Get actual connection
if (inherits(con, "Pool")) {
  actual_con <- pool::poolCheckout(con)
} else {
  actual_con <- con
}

# 1. Count unlinked infraspecific taxa
cat("1. Counting unlinked infraspecific taxa...\n")
unlinked <- DBI::dbGetQuery(actual_con, "
  SELECT COUNT(*) as n
  FROM table_taxa
  WHERE tax_level = 'infraspecific' AND id_parent IS NULL
")
cat("   Unlinked infraspecific taxa:", unlinked$n, "\n\n")

# 2. Sample some unlinked infraspecific taxa
cat("2. Sample of unlinked infraspecific taxa:\n")
sample_infra <- DBI::dbGetQuery(actual_con, "
  SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_nam01, tax_rank01
  FROM table_taxa
  WHERE tax_level = 'infraspecific' AND id_parent IS NULL
  LIMIT 10
")
print(sample_infra)

# 3. For each sample, check if parent species exists
cat("\n3. Checking if parent species exist for samples:\n")
for (i in 1:min(5, nrow(sample_infra))) {
  infra <- sample_infra[i, ]
  cat(sprintf("\n   Infraspecific: %s %s %s %s\n",
              infra$tax_gen, infra$tax_esp, infra$tax_rank01, infra$tax_nam01))

  # Check for parent species
  parent_check <- DBI::dbGetQuery(actual_con, sprintf("
    SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_level
    FROM table_taxa
    WHERE tax_gen = '%s'
      AND tax_esp = '%s'
      AND tax_fam = '%s'
      AND tax_level = 'species'
    LIMIT 3
  ", infra$tax_gen, infra$tax_esp, infra$tax_fam))

  if (nrow(parent_check) > 0) {
    cat("   ✓ Parent species found:\n")
    print(parent_check)
  } else {
    cat("   ✗ No parent species found with tax_level='species'\n")

    # Check if species exists with different tax_level
    species_any <- DBI::dbGetQuery(actual_con, sprintf("
      SELECT idtax_n, tax_gen, tax_esp, tax_fam, tax_level
      FROM table_taxa
      WHERE tax_gen = '%s'
        AND tax_esp = '%s'
        AND tax_fam = '%s'
      LIMIT 3
    ", infra$tax_gen, infra$tax_esp, infra$tax_fam))

    if (nrow(species_any) > 0) {
      cat("   ⚠ But species entry exists with different tax_level:\n")
      print(species_any)
    } else {
      cat("   ✗ No species entry at all (missing parent)\n")
    }
  }
}

# 4. Test a single UPDATE manually to see if it persists
cat("\n\n4. Testing if UPDATE persists...\n")
test_infra <- sample_infra[1, ]
cat("   Testing with ID:", test_infra$idtax_n, "\n")

# Get parent ID
parent_id_query <- sprintf("
  SELECT idtax_n FROM table_taxa
  WHERE tax_gen = '%s'
    AND tax_esp = '%s'
    AND tax_fam = '%s'
    AND tax_level = 'species'
  LIMIT 1
", test_infra$tax_gen, test_infra$tax_esp, test_infra$tax_fam)

parent_id_result <- DBI::dbGetQuery(actual_con, parent_id_query)

if (nrow(parent_id_result) > 0) {
  parent_id <- parent_id_result$idtax_n[1]
  cat("   Parent ID:", parent_id, "\n")

  # Check current id_parent
  before <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT id_parent FROM table_taxa WHERE idtax_n = %d",
    test_infra$idtax_n
  ))
  cat("   Before update: id_parent =", before$id_parent, "\n")

  # Execute UPDATE
  update_sql <- sprintf("
    UPDATE table_taxa
    SET id_parent = %d
    WHERE idtax_n = %d
  ", parent_id, test_infra$idtax_n)

  result <- DBI::dbExecute(actual_con, update_sql)
  cat("   Update result:", result, "row(s) affected\n")

  # Check after update (immediate)
  after_immediate <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT id_parent FROM table_taxa WHERE idtax_n = %d",
    test_infra$idtax_n
  ))
  cat("   After update (immediate): id_parent =", after_immediate$id_parent, "\n")

  # Return and re-checkout connection (force commit for pool)
  if (inherits(con, "Pool")) {
    pool::poolReturn(actual_con)
    Sys.sleep(0.5)
    actual_con <- pool::poolCheckout(con)
    cat("   Connection returned and re-checked out\n")
  }

  # Check after connection cycle
  after_cycle <- DBI::dbGetQuery(actual_con, sprintf(
    "SELECT id_parent FROM table_taxa WHERE idtax_n = %d",
    test_infra$idtax_n
  ))
  cat("   After connection cycle: id_parent =", after_cycle$id_parent, "\n")

  if (is.na(after_cycle$id_parent)) {
    cat("\n   ✗ UPDATE DID NOT PERSIST - Transaction not committed!\n")
    cat("   This is the root cause - need explicit transaction management\n")
  } else {
    cat("\n   ✓ UPDATE persisted successfully\n")
  }

} else {
  cat("   ✗ Cannot test - no parent species found\n")
}

# Clean up
if (inherits(con, "Pool")) {
  pool::poolReturn(actual_con)
}

cat("\n========================================\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("========================================\n")
