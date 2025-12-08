# Updated Batch Matching Code Using tax_level Field
#
# This is the updated version of the batch matching logic that uses the new
# tax_level field for cleaner and more reliable queries.
#
# Replace the corresponding section in R/mod_auto_matching.R after running
# add_tax_level_field.R

# STEP 1: Download entire taxonomic backbone once (UPDATED)
mydb_taxa <- call.mydb.taxa()

backbone <- dplyr::tbl(mydb_taxa, "table_taxa") %>%
  dplyr::select(
    idtax_n,
    idtax_good_n,
    tax_fam,
    tax_gen,
    tax_esp,
    tax_rank01,
    tax_nam01,
    tax_rank02,
    tax_nam02,
    tax_level  # NEW FIELD
  ) %>%
  dplyr::collect()

# Create formatted name columns for matching
backbone <- backbone %>%
  dplyr::mutate(
    # Full species-level name (genus + species + infraspecific)
    tax_sp_level = dplyr::case_when(
      !is.na(tax_nam01) & tax_nam01 != "" ~ paste(tax_gen, tax_esp, tax_rank01, tax_nam01),
      !is.na(tax_esp) & tax_esp != "" ~ paste(tax_gen, tax_esp),
      TRUE ~ NA_character_
    ),
    # Genus-level name (just genus)
    tax_gen_level = tax_gen,
    # Family-level name (just family)
    tax_fam_level = tax_fam
  )

# STEP 2: Prepare input names for batch matching
input_df <- data.frame(
  input_name = unique_names,
  stringsAsFactors = FALSE
)

# STEP 3: Batch exact matching on species level (UPDATED - uses tax_level)
# First identify unique species names in backbone
unique_species <- backbone %>%
  dplyr::filter(
    tax_level %in% c("species", "infraspecific"),  # Species or infraspecific taxa
    !is.na(tax_sp_level)
  ) %>%
  dplyr::group_by(tax_sp_level) %>%
  dplyr::filter(dplyr::n() == 1) %>%  # Only unique matches
  dplyr::ungroup() %>%
  dplyr::select(
    tax_sp_level,
    idtax_n,
    idtax_good_n,
    tax_fam,
    tax_gen,
    tax_esp,
    tax_rank01,
    tax_nam01,
    tax_level
  ) %>%
  dplyr::mutate(
    matched_name = tax_sp_level,
    match_method = "exact",
    match_score = 1.0
  )

# Match input names to unique species
matches_species <- input_df %>%
  dplyr::left_join(
    unique_species,
    by = c("input_name" = "tax_sp_level")
  )

# STEP 4: Batch exact matching on genus level (UPDATED - uses tax_level)
unmatched_after_species <- matches_species %>%
  dplyr::filter(is.na(idtax_n)) %>%
  dplyr::select(input_name)

unique_genera <- backbone %>%
  dplyr::filter(
    tax_level == "genus",  # CLEANER: explicit genus-level filter
    !is.na(tax_gen_level)
  ) %>%
  dplyr::group_by(tax_gen_level) %>%
  dplyr::filter(dplyr::n() == 1) %>%  # Only unique matches
  dplyr::ungroup() %>%
  dplyr::select(
    tax_gen_level,
    idtax_n,
    idtax_good_n,
    tax_fam,
    tax_gen,
    tax_level
  ) %>%
  dplyr::mutate(
    matched_name = tax_gen_level,
    match_method = "exact",
    match_score = 1.0
  )

matches_genus <- unmatched_after_species %>%
  dplyr::left_join(
    unique_genera,
    by = c("input_name" = "tax_gen_level")
  )

# STEP 5: Batch exact matching on family level (UPDATED - uses tax_level)
unmatched_after_genus <- matches_genus %>%
  dplyr::filter(is.na(idtax_n)) %>%
  dplyr::select(input_name)

unique_families <- backbone %>%
  dplyr::filter(
    tax_level == "family",  # CLEANER: explicit family-level filter
    !is.na(tax_fam_level)
  ) %>%
  dplyr::group_by(tax_fam_level) %>%
  dplyr::filter(dplyr::n() == 1) %>%  # Only unique matches
  dplyr::ungroup() %>%
  dplyr::select(
    tax_fam_level,
    idtax_n,
    idtax_good_n,
    tax_fam,
    tax_level
  ) %>%
  dplyr::mutate(
    matched_name = tax_fam_level,
    match_method = "exact",
    match_score = 1.0
  )

matches_family <- unmatched_after_genus %>%
  dplyr::left_join(
    unique_families,
    by = c("input_name" = "tax_fam_level")
  )

# STEP 6: Combine all batch exact matches (same as before)
best_matches <- matches_species %>%
  dplyr::rows_update(
    matches_genus %>% dplyr::filter(!is.na(idtax_n)),
    by = "input_name",
    unmatched = "ignore"
  ) %>%
  dplyr::rows_update(
    matches_family %>% dplyr::filter(!is.na(idtax_n)),
    by = "input_name",
    unmatched = "ignore"
  )

# Rest of the code remains the same (fuzzy matching, synonym resolution, etc.)
