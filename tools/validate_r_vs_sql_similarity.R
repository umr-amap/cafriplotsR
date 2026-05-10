# Validation script: compare R-side trigram similarity vs PostgreSQL pg_trgm
# SIMILARITY() on a representative sample of taxonomic names.
#
# This script is meant to be run ONCE, manually, while online — to confirm
# that the R-side fuzzy matching used in offline mode produces scores close
# enough to the SQL-side fuzzy matching that the same `min_similarity`
# threshold behaves predictably for users.
#
# It does not run as part of the package test suite (no DB available there).
#
# Usage:
#   Rscript tools/validate_r_vs_sql_similarity.R
#
# Outputs a table with paired R and SQL similarity scores and a summary of
# their agreement (correlation, mean abs. delta, % within ±0.05).

suppressPackageStartupMessages({
  library(CafriplotsR)
  library(dplyr)
  library(tibble)
  library(stringdist)
  library(DBI)
})

# --- Connect to the taxa database ----------------------------------------
con <- call.mydb.taxa()

# --- Pick representative test pairs --------------------------------------
# Mix of: exact matches, 1-letter typos, name shuffling, and unrelated names.
test_pairs <- tibble::tribble(
  ~query,                  ~target,
  # Exact / near-exact
  "Garcinia kola",          "Garcinia kola",
  "Brachystegia laurentii", "Brachystegia laurentii",
  # 1-letter typos
  "Garcinea kola",          "Garcinia kola",
  "Garcinia kolla",         "Garcinia kola",
  "Brachystegea laurentii", "Brachystegia laurentii",
  "Julbernardea seretii",   "Julbernardia seretii",
  # 2+ letter typos
  "Gilbertodendron dewev",  "Gilbertiodendron dewevrei",
  "Pericopis elata",        "Pericopsis elata",
  # Genus-only
  "Garcinia",               "Garcinia",
  "Garcineia",              "Garcinia",
  # Family
  "Fabaceae",               "Fabaceae",
  "Fabacaee",               "Fabaceae",
  # Unrelated
  "Quercus robur",          "Garcinia kola"
)

cat("Computing R-side trigram-Jaccard scores (stringdist q=3)...\n")
test_pairs <- test_pairs %>%
  mutate(
    r_sim = stringdist::stringsim(tolower(query), tolower(target),
                                  method = "jaccard", q = 3L)
  )

cat("Computing SQL-side pg_trgm SIMILARITY()...\n")
test_pairs$sql_sim <- vapply(seq_len(nrow(test_pairs)), function(i) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT SIMILARITY(lower($1), lower($2)) AS sim",
    params = list(test_pairs$query[i], test_pairs$target[i])
  )
  res$sim
}, numeric(1))

test_pairs <- test_pairs %>%
  mutate(delta = r_sim - sql_sim)

print(test_pairs, n = Inf)

cat("\n--- Agreement summary ---\n")
cat(sprintf("Pearson correlation:    %.4f\n",
            cor(test_pairs$r_sim, test_pairs$sql_sim)))
cat(sprintf("Mean |R - SQL|:         %.4f\n",
            mean(abs(test_pairs$delta))))
cat(sprintf("Max  |R - SQL|:         %.4f\n",
            max(abs(test_pairs$delta))))
cat(sprintf("%% pairs within  0.05:   %.1f%%\n",
            100 * mean(abs(test_pairs$delta) <= 0.05)))
cat(sprintf("%% pairs within  0.10:   %.1f%%\n",
            100 * mean(abs(test_pairs$delta) <= 0.10)))

# --- Threshold-equivalent test -------------------------------------------
# What R-side threshold matches a 0.5 SQL threshold?
cat("\n--- Threshold-equivalent calibration ---\n")
for (sql_thr in c(0.3, 0.5, 0.7)) {
  sql_pass <- test_pairs$sql_sim >= sql_thr
  # Find R threshold that maximises agreement with sql_pass
  best <- vapply(seq(0, 1, by = 0.01), function(thr) {
    mean((test_pairs$r_sim >= thr) == sql_pass)
  }, numeric(1))
  best_thr <- seq(0, 1, by = 0.01)[which.max(best)]
  cat(sprintf("SQL >= %.2f  ->  R-side equivalent: %.2f  (agreement: %.1f%%)\n",
              sql_thr, best_thr, 100 * max(best)))
}

cat("\nDone. If correlation > 0.95 and mean delta < 0.05, the R-side path\n")
cat("can use the same min_similarity values as the SQL path. Otherwise,\n")
cat("either pad strings to mimic pg_trgm or document the calibration above.\n")
