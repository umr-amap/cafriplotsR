# Tests for R-side (cache-only) taxonomic matching.
# These do not require a database connection — they construct a small
# synthetic backbone tibble matching the schema written by
# save_backbone_cache() / mod_auto_matching.R.

# Build a minimal backbone fixture with the columns the R-side helpers need.
.fake_backbone <- function() {
  bb <- tibble::tibble(
    idtax_n      = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L),
    idtax_good_n = c(1L, 2L, 3L, 1L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L),
    tax_fam      = c("Clusiaceae", "Clusiaceae", "Fabaceae",
                     "Clusiaceae", "Fabaceae", "Fabaceae",
                     "Clusiaceae",  NA, NA, "Clusiaceae", "Fabaceae", NA),
    tax_famclass = c(rep("Magnoliopsida", 7), NA, NA, NA, NA, "Magnoliopsida"),
    tax_gen      = c("Garcinia", "Garcinia", "Brachystegia",
                     "Garcinia", "Brachystegia", "Julbernardia",
                     "Garcinia",  "Garcinia", "Brachystegia", NA, NA, NA),
    tax_esp      = c("kola", "punctata", "laurentii",
                     "cola", "spiciformis", "seretii",
                     NA, NA, NA, NA, NA, NA),
    tax_rank01   = NA_character_,
    tax_nam01    = NA_character_,
    tax_rank02   = NA_character_,
    tax_nam02    = NA_character_,
    tax_level    = c("species", "species", "species",
                     "species", "species", "species",
                     "genus", "genus", "genus",
                     "family", "family", "higher"),
    author1      = NA_character_
  )
  bb$tax_sp_level <- ifelse(!is.na(bb$tax_esp),
                            paste(bb$tax_gen, bb$tax_esp), NA_character_)
  bb$tax_gen_level <- bb$tax_gen
  bb$tax_fam_level <- bb$tax_fam
  bb$tax_class_level <- bb$tax_famclass
  bb
}


# =============================================================================
# .build_backbone_name_field
# =============================================================================

test_that(".build_backbone_name_field concatenates genus + species correctly", {
  bb <- .fake_backbone()
  out <- .build_backbone_name_field(bb)
  expect_equal(out[1], "Garcinia kola")
  expect_equal(out[7], "Garcinia")              # no species
  expect_true(is.na(out[10]))                   # tax_gen NA → NA
})

test_that(".build_backbone_name_field handles missing infraspecific cols", {
  bb <- .fake_backbone()
  bb$tax_rank01 <- NULL
  out <- .build_backbone_name_field(bb)
  expect_equal(out[1], "Garcinia kola")
})


# =============================================================================
# .trigram_sim
# =============================================================================

test_that(".trigram_sim returns 1 for identical strings (case-insensitive)", {
  expect_equal(.trigram_sim("Garcinia kola", "Garcinia kola"), 1)
  expect_equal(.trigram_sim("garcinia kola", "Garcinia Kola"), 1)
})

test_that(".trigram_sim returns lower scores for distant strings", {
  s1 <- .trigram_sim("Garcinia kola", "Garcinea kola")  # 1-letter typo
  s2 <- .trigram_sim("Garcinia kola", "Brachystegia laurentii")
  expect_gt(s1, 0.5)
  expect_lt(s2, 0.2)
  expect_gt(s1, s2)
})


# =============================================================================
# .match_exact_r — species, genus, family
# =============================================================================

test_that("exact species match found", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Garcinia kola")
  parsed$original_input <- "Garcinia kola"
  res <- .match_exact_r(parsed, bb, include_authors = FALSE, max_matches = 5)
  expect_gt(nrow(res), 0)
  expect_equal(res$matched_name[1], "Garcinia kola")
  expect_equal(res$idtax_n[1], 1L)
  expect_equal(res$match_method[1], "exact")
  expect_equal(res$match_score[1], 1.0)
})

test_that("exact family match returns one row", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Fabaceae")
  parsed$original_input <- "Fabaceae"
  res <- .match_exact_r(parsed, bb, include_authors = FALSE, max_matches = 5)
  expect_equal(nrow(res), 1)
  expect_equal(res$matched_name[1], "Fabaceae")
})

test_that("exact match returns empty tibble when no hit", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Nonsensia inexistensus")
  parsed$original_input <- "Nonsensia inexistensus"
  res <- .match_exact_r(parsed, bb, include_authors = FALSE, max_matches = 5)
  expect_equal(nrow(res), 0)
})


# =============================================================================
# .match_genus_constrained_r
# =============================================================================

test_that("genus-constrained finds typo within correct genus", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Garcinia kolla")  # typo
  parsed$original_input <- "Garcinia kolla"
  res <- .match_genus_constrained_r(parsed, bb, min_similarity = 0.3,
                                    include_authors = FALSE, max_matches = 5)
  expect_gt(nrow(res), 0)
  expect_equal(res$tax_gen[1], "Garcinia")
  expect_equal(res$match_method[1], "genus_constrained")
  expect_lt(res$match_score[1], 1.0)
  expect_gt(res$match_score[1], 0.3)
})

test_that("genus-constrained returns empty when genus not similar enough", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Nonsensia kola")
  parsed$original_input <- "Nonsensia kola"
  res <- .match_genus_constrained_r(parsed, bb, min_similarity = 0.7,
                                    include_authors = FALSE, max_matches = 5)
  expect_equal(nrow(res), 0)
})


# =============================================================================
# .match_fuzzy_r
# =============================================================================

test_that("fuzzy match finds typo'd genus", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Garcineia kola")  # typo in genus
  parsed$original_input <- "Garcineia kola"
  res <- .match_fuzzy_r(parsed, bb, min_similarity = 0.3,
                        include_authors = FALSE, max_matches = 5)
  expect_gt(nrow(res), 0)
  # Best should be a Garcinia
  expect_equal(res$tax_gen[1], "Garcinia")
})

test_that("fuzzy match family ranks by similarity", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Fbaceae")  # typo, still parses as family
  parsed$original_input <- "Fbaceae"
  expect_equal(parsed$rank, "family")  # sanity: parser must classify as family
  res <- .match_fuzzy_r(parsed, bb, min_similarity = 0.3,
                        include_authors = FALSE, max_matches = 5)
  expect_gt(nrow(res), 0)
  expect_equal(res$matched_name[1], "Fabaceae")
})


# =============================================================================
# .match_single_name_r — full pipeline
# =============================================================================

test_that("hierarchical method tries exact then fuzzy on cached backbone", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Garcinia kola")
  parsed$original_input <- "Garcinia kola"
  res <- .match_single_name_r(parsed, bb, method = "hierarchical",
                              max_matches = 1, min_similarity = 0.3,
                              include_authors = FALSE, verbose = FALSE)
  expect_equal(res$match_method[1], "exact")
})

test_that("returns no_match row when nothing passes threshold", {
  bb <- .fake_backbone()
  parsed <- parse_taxonomic_name("Quercus robur")
  parsed$original_input <- "Quercus robur"
  res <- .match_single_name_r(parsed, bb, method = "hierarchical",
                              max_matches = 1, min_similarity = 0.95,
                              include_authors = FALSE, verbose = FALSE)
  expect_equal(res$match_method[1], "no_match")
  expect_true(is.na(res$idtax_n[1]))
})


# =============================================================================
# match_taxonomic_names() top-level — backbone path
# =============================================================================

test_that("match_taxonomic_names() with backbone arg avoids DB and returns matches", {
  bb <- .fake_backbone()
  res <- match_taxonomic_names(
    names           = c("Garcinia kola", "Brachystegia laurentii", "Quercus robur"),
    method          = "hierarchical",
    max_matches     = 1,
    min_similarity  = 0.3,
    include_synonyms = TRUE,
    return_scores   = TRUE,
    include_authors = FALSE,
    con             = NULL,
    backbone        = bb,
    verbose         = FALSE
  )
  expect_equal(nrow(res), 3)
  # Garcinia kola → exact
  expect_equal(res$match_method[res$input_name == "Garcinia kola"], "exact")
  # Brachystegia laurentii → exact
  expect_equal(res$match_method[res$input_name == "Brachystegia laurentii"], "exact")
  # Quercus robur → no_match (not in fixture)
  expect_equal(res$match_method[res$input_name == "Quercus robur"], "no_match")
})


# =============================================================================
# .add_synonym_info_r
# =============================================================================

test_that("synonym info is added from cached backbone", {
  bb <- .fake_backbone()
  # Row 4 (Garcinia cola) is a synonym of row 1 (Garcinia kola): idtax_good_n = 1
  matches <- tibble::tibble(
    input_name   = "Garcinia cola",
    matched_name = "Garcinia cola",
    idtax_n      = 4L,
    idtax_good_n = 1L,
    match_method = "exact",
    match_score  = 1.0,
    tax_gen      = "Garcinia",
    tax_esp      = "cola",
    tax_fam      = "Clusiaceae",
    tax_level    = "species",
    match_rank   = 1L
  )
  res <- .add_synonym_info_r(matches, bb)
  expect_true(res$is_synonym[1])
  expect_equal(res$accepted_name[1], "Garcinia kola")
})
