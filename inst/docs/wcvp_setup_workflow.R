# =============================================================================
# WCVP Backbone Setup Workflow
#
# Run this script once (or when a new WCVP version is released) to:
#   1. Create the WCVP tables in the taxa database
#   2. Import WCVP names from rWCVPdata
#   3. Match internal taxa to WCVP and save the link table
#
# Prerequisites:
#   install.packages("rWCVP")
#   install.packages("rWCVPdata")   # large package (~200 MB)
#
# After completing this workflow, all CafriplotsR functions that accept
# backbone = "wcvp" will use the WCVP synonym resolution.
# =============================================================================

library(CafriplotsR)

# Connect to taxa database
con_taxa <- call.mydb.taxa(use_env_credentials = T)


# ============================================================
# STEP 1 — Create the WCVP tables in the database (once only)
# ============================================================
# Creates: wcvp_names, wcvp_idtax_link, wcvp_import_metadata
# Idempotent: safe to re-run (uses IF NOT EXISTS)

# Preview the SQL first (dry run)
setup_wcvp_schema(con_taxa, dry_run = TRUE)

# Execute for real
setup_wcvp_schema(con_taxa, dry_run = FALSE)


# ============================================================
# STEP 2 — Import WCVP names from rWCVPdata into the database
# ============================================================
# Loads rWCVPdata::wcvp_names and inserts into wcvp_names table.
# Skips automatically if the same WCVP version is already loaded.
# Use force = TRUE to re-import even if version matches.

# Check current status before import
get_wcvp_status(con_taxa)

# Import
import_wcvp_names(con_taxa, batch_size = 50000, verbose = TRUE, force = T)

# Verify record count and version
get_wcvp_status(con_taxa)


# ============================================================
# STEP 3 — Match internal taxa to WCVP (build the link table)
# ============================================================
# Matches accepted taxa from table_taxa against wcvp_names.
# Returns a tibble for REVIEW — does NOT write to DB automatically.
# Inspect fuzzy matches carefully before saving.

matches <- match_taxa_to_wcvp(
  con_taxa         = con_taxa,
  tax_ids          = NULL,          # NULL = all accepted taxa in table_taxa
  methods          = c("exact", "fuzzy"),
  fuzzy_threshold  = 0.9,
  author_match     = "fuzzy",       # "none" | "exact" | "fuzzy"
  author_threshold = 0.6,           # Jaro-Winkler threshold (only used when author_match = "fuzzy")
  n_cores          = parallel::detectCores() - 5,  # parallel genus blocks (1 = sequential)
  verbose          = TRUE
)

# --- Review matches before saving ---

# Match type breakdown
dplyr::count(matches, match_type)

# Inspect fuzzy matches (most likely to need manual review)
dplyr::filter(matches, match_type == "fuzzy") |>
  dplyr::select(idtax_n, taxon_name_internal, wcvp_taxon_name, match_score) |>
  dplyr::arrange(match_score) |>
  head(30)

# Unmatched taxa (no WCVP equivalent found)
dplyr::filter(matches, is.na(plant_name_id))

# --- Save approved matches to wcvp_idtax_link ---
# replace = TRUE: overwrites existing links for the same idtax_n
save_wcvp_links(matches, con_taxa, replace = TRUE, verbose = TRUE)


# ============================================================
# STEP 4 — Verify the integration works
# ============================================================

# Check whether a newer WCVP version is available via rWCVP
check_wcvp_update(con_taxa)

# Test backbone switching on a known genus
query_taxa(genus = "Allanblackia", backbone = "wcvp")
query_taxa(genus = "Allanblackia", backbone = "internal")

# Test synonym resolution
resolve_taxon_synonyms(idtax = c(3095, 219), backbone = "wcvp",  con_taxa = con_taxa)
resolve_taxon_synonyms(idtax = c(3095, 219), backbone = "internal", con_taxa = con_taxa)


# ============================================================
# RE-IMPORT WORKFLOW (when a new WCVP version is released)
# ============================================================
# Step 2 with force = TRUE truncates wcvp_names and reloads fresh data.
# Then re-run Step 3 to rebuild the link table.

# import_wcvp_names(con_taxa, force = TRUE, verbose = TRUE)
# matches <- match_taxa_to_wcvp(con_taxa, verbose = TRUE)
# # ... review matches ...
# save_wcvp_links(matches, con_taxa, replace = TRUE)
