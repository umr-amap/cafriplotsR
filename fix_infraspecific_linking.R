# Fix Script: Link Infraspecific Taxa to Species
# Simplified version with explicit transaction handling

library(CafriplotsR)
library(DBI)

con <- call.mydb.taxa(use_env_credentials = TRUE)

# Handle pool connections - get actual connection
if (inherits(con, "Pool")) {
  actual_con <- pool::poolCheckout(con)
  cat("Using pool connection\n")
} else {
  actual_con <- con
  cat("Using direct connection\n")
}

cat("\n========================================\n")
cat("FIX: Link infraspecific taxa to species\n")
cat("========================================\n\n")

# Count how many need updating
count_sql <- "SELECT COUNT(*) as n FROM table_taxa WHERE tax_level = 'infraspecific' AND id_parent IS NULL;"
infra_count <- DBI::dbGetQuery(actual_con, count_sql)$n[1]

cat("Found", infra_count, "infraspecific taxa to link\n\n")

if (infra_count == 0) {
  cat("✓ All infraspecific taxa already linked!\n")
} else {

  # Get all infraspecific IDs that need updating
  cat("Fetching infraspecific IDs...\n")
  infra_ids <- DBI::dbGetQuery(actual_con,
    "SELECT idtax_n FROM table_taxa WHERE tax_level = 'infraspecific' AND id_parent IS NULL ORDER BY idtax_n"
  )$idtax_n

  cat("Fetched", length(infra_ids), "IDs\n")

  batch_size <- 500  # Smaller batches for better control
  n_batches <- ceiling(length(infra_ids) / batch_size)
  total_linked <- 0

  cat("\nProcessing in", n_batches, "batches of", batch_size, "\n\n")

  for (batch_num in seq_len(n_batches)) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, length(infra_ids))
    batch_ids <- infra_ids[start_idx:end_idx]

    # Create comma-separated list of IDs for IN clause
    ids_str <- paste(batch_ids, collapse = ", ")

    sql_batch <- sprintf("
      UPDATE table_taxa child
      SET id_parent = (
        SELECT parent.idtax_n FROM table_taxa parent
        WHERE parent.tax_gen = child.tax_gen
          AND parent.tax_fam = child.tax_fam
          AND parent.tax_esp = child.tax_esp
          AND parent.tax_level = 'species'
        LIMIT 1
      )
      WHERE child.idtax_n IN (%s);
    ", ids_str)

    tryCatch({
      # Execute update
      result <- DBI::dbExecute(actual_con, sql_batch)
      total_linked <- total_linked + result

      cat(sprintf("Batch %d/%d: linked %d taxa (total: %d)\n",
                  batch_num, n_batches, result, total_linked))

      # Force flush after every batch (for pool connections)
      if (inherits(con, "Pool")) {
        # Return and checkout again to force commit
        pool::poolReturn(actual_con)
        Sys.sleep(0.1)  # Brief pause
        actual_con <- pool::poolCheckout(con)
      }

    }, error = function(e) {
      cat("\n✗ Batch", batch_num, "failed:", e$message, "\n")
      cat("Progress saved (", total_linked, "already linked). Re-run to continue.\n")
      stop(e)
    })
  }

  cat("\n✓ Linked", total_linked, "infraspecific taxa to species\n")

  # Verify the fix worked
  cat("\n--- Verification ---\n")
  remaining <- DBI::dbGetQuery(actual_con, count_sql)$n[1]
  cat("Remaining unlinked infraspecific taxa:", remaining, "\n")

  if (remaining > 0) {
    cat("\n⚠ Warning: Some taxa remain unlinked. Possible reasons:\n")
    cat("  - No matching species entry exists (check parent species)\n")
    cat("  - Species entry doesn't have tax_level = 'species'\n")
    cat("Run investigate_hierarchy_issues.R for details\n")
  } else {
    cat("\n✓ Success! All infraspecific taxa are now linked.\n")
  }
}

# Clean up connection
if (inherits(con, "Pool")) {
  pool::poolReturn(actual_con)
  cat("\nPool connection returned\n")
} else {
  cat("\nDirect connection still open\n")
}

cat("\n========================================\n")
cat("COMPLETE\n")
cat("========================================\n")
