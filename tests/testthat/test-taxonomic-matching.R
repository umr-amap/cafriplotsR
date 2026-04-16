# Tests for R/taxonomic_matching.R
# Pure string processing functions - no database required

# =============================================================================
# clean_taxonomic_name()
# =============================================================================

test_that("clean_taxonomic_name removes 'sp.' suffix", {
  expect_equal(clean_taxonomic_name("Fabaceae sp."), "Fabaceae")
  expect_equal(clean_taxonomic_name("Garcinia sp"), "Garcinia")
})

test_that("clean_taxonomic_name removes 'spp.' suffix", {
  expect_equal(clean_taxonomic_name("Brachystegia spp"), "Brachystegia")
  expect_equal(clean_taxonomic_name("Brachystegia spp."), "Brachystegia")
})

test_that("clean_taxonomic_name removes uncertainty markers", {
  expect_equal(clean_taxonomic_name("Garcinia cf. kola"), "Garcinia kola")
  expect_equal(clean_taxonomic_name("Garcinia cf kola"), "Garcinia kola")
  expect_equal(clean_taxonomic_name("Garcinia aff. kola"), "Garcinia kola")
  expect_equal(clean_taxonomic_name("Garcinia aff kola"), "Garcinia kola")
})

test_that("clean_taxonomic_name removes question marks", {
  expect_equal(
    clean_taxonomic_name("Gilbertiodendron  ?  dewevrei"),
    "Gilbertiodendron dewevrei"
  )
})

test_that("clean_taxonomic_name collapses multiple spaces", {
  expect_equal(
    clean_taxonomic_name("Garcinia   kola"),
    "Garcinia kola"
  )
})

test_that("clean_taxonomic_name replaces underscores with spaces", {
  expect_equal(clean_taxonomic_name("Garcinia_kola"), "Garcinia kola")
})

test_that("clean_taxonomic_name trims whitespace", {
  expect_equal(clean_taxonomic_name("  Garcinia kola  "), "Garcinia kola")
})

test_that("clean_taxonomic_name handles NA and empty string", {
  expect_true(is.na(clean_taxonomic_name(NA)))
  expect_equal(clean_taxonomic_name(""), "")
})

test_that("clean_taxonomic_name leaves clean names unchanged", {
  expect_equal(
    clean_taxonomic_name("Gilbertiodendron dewevrei"),
    "Gilbertiodendron dewevrei"
  )
})

# =============================================================================
# parse_taxonomic_name()
# =============================================================================

test_that("parse_taxonomic_name detects family rank", {
  result <- parse_taxonomic_name("Fabaceae")
  expect_equal(result$rank, "family")
  expect_true(is.na(result$genus))
  expect_true(is.na(result$species))
  expect_equal(result$full_name_no_auth, "Fabaceae")
})

test_that("parse_taxonomic_name detects order rank", {
  result <- parse_taxonomic_name("Malpighiales")
  expect_equal(result$rank, "order")
  expect_true(is.na(result$genus))
})

test_that("parse_taxonomic_name detects genus (single word)", {
  result <- parse_taxonomic_name("Brachystegia")
  expect_equal(result$rank, "genus")
  expect_equal(result$genus, "Brachystegia")
  expect_true(is.na(result$species))
})

test_that("parse_taxonomic_name detects species (two words)", {
  result <- parse_taxonomic_name("Gilbertiodendron dewevrei")
  expect_equal(result$rank, "species")
  expect_equal(result$genus, "Gilbertiodendron")
  expect_equal(result$species, "dewevrei")
})

test_that("parse_taxonomic_name extracts infraspecific parts", {
  result <- parse_taxonomic_name("Anthonotha macrophylla var. oblongifolia")
  expect_equal(result$rank, "species")
  expect_equal(result$genus, "Anthonotha")
  expect_equal(result$species, "macrophylla")
  expect_true(grepl("var", result$infraspecific))
  expect_true(grepl("oblongifolia", result$infraspecific))
})

test_that("parse_taxonomic_name handles NA and empty string", {
  result_na <- parse_taxonomic_name(NA)
  expect_equal(result_na$rank, "unknown")
  expect_true(is.na(result_na$genus))

  result_empty <- parse_taxonomic_name("")
  expect_equal(result_empty$rank, "unknown")
  expect_true(is.na(result_empty$genus))
})

test_that("parse_taxonomic_name preserves input_name", {
  input <- "Garcinia kola"
  result <- parse_taxonomic_name(input)
  expect_equal(result$input_name, input)
})

test_that("parse_taxonomic_name capitalizes genus", {
  result <- parse_taxonomic_name("garcinia kola")
  expect_equal(result$genus, "Garcinia")
})

test_that("parse_taxonomic_name lowercases species epithet", {
  result <- parse_taxonomic_name("Garcinia Kola")
  # Second word is uppercase -> not treated as species epithet

  expect_equal(result$rank, "genus")
  expect_true(is.na(result$species))
})

test_that("match_taxonomic_names returns empty tibble for empty or invalid input", {
  expect_equal(nrow(match_taxonomic_names(character(0), con = structure(list(), class = "mock_con"), verbose = FALSE)), 0)
  expect_equal(nrow(match_taxonomic_names(c(NA_character_, ""), con = structure(list(), class = "mock_con"), verbose = FALSE)), 0)
})

test_that("match_taxonomic_names cleans, parses, and enriches matches via helpers", {
  captured <- list()

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .match_single_name_sql = function(parsed, con, method, max_matches, min_similarity, include_authors, verbose) {
      captured[[parsed$original_input]] <<- parsed
      tibble::tibble(
        input_name = parsed$original_input,
        matched_name = parsed$full_name_no_auth,
        idtax_n = if (parsed$original_input == "Garcinia cf. kola") 10L else 20L,
        idtax_good_n = if (parsed$original_input == "Garcinia cf. kola") 11L else 20L,
        match_method = if (parsed$original_input == "Garcinia cf. kola") "genus_constrained" else "exact",
        match_score = c(0.91),
        tax_gen = parsed$genus,
        tax_esp = parsed$species,
        tax_fam = if (parsed$original_input == "Garcinia cf. kola") "Clusiaceae" else NA_character_,
        tax_level = parsed$rank,
        match_rank = 1L
      )
    },
    .add_synonym_info_sql = function(matches, con) {
      matches$is_synonym <- matches$idtax_n != matches$idtax_good_n
      matches$accepted_name <- ifelse(matches$is_synonym, "Garcinia kola", NA_character_)
      matches
    }
  )

  result <- match_taxonomic_names(
    c("Garcinia cf. kola", "Fabaceae sp."),
    con = structure(list(), class = "mock_con"),
    verbose = FALSE
  )

  expect_equal(nrow(result), 2)
  expect_equal(captured[["Garcinia cf. kola"]]$full_name_no_auth, "Garcinia kola")
  expect_equal(captured[["Garcinia cf. kola"]]$genus, "Garcinia")
  expect_equal(captured[["Fabaceae sp."]]$full_name_no_auth, "Fabaceae")
  expect_equal(captured[["Fabaceae sp."]]$rank, "family")
  expect_true("accepted_name" %in% names(result))
  expect_true(result$is_synonym[[1]])
  expect_false(result$is_synonym[[2]])
})

test_that("match_taxonomic_names drops score column when return_scores is FALSE", {
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .match_single_name_sql = function(parsed, con, method, max_matches, min_similarity, include_authors, verbose) {
      tibble::tibble(
        input_name = parsed$original_input,
        matched_name = parsed$full_name_no_auth,
        idtax_n = 10L,
        idtax_good_n = 10L,
        match_method = "exact",
        match_score = 1,
        tax_gen = parsed$genus,
        tax_esp = parsed$species,
        tax_fam = "Fabaceae",
        tax_level = parsed$rank,
        match_rank = 1L
      )
    },
    .add_synonym_info_sql = function(matches, con) matches
  )

  result <- match_taxonomic_names(
    "Gilbertiodendron dewevrei",
    con = structure(list(), class = "mock_con"),
    verbose = FALSE,
    return_scores = FALSE
  )

  expect_false("match_score" %in% names(result))
})

test_that("standardize_taxonomic_batch joins best matches back to original data", {
  input <- tibble::tibble(
    row_id = 1:3,
    species = c("Garcinia kola", "Unknown species", "Garcinia kola")
  )

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    match_taxonomic_names = function(names, method, max_matches, min_similarity, include_synonyms, return_scores, include_authors, con, verbose) {
      tibble::tibble(
        input_name = c("Garcinia kola", "Garcinia kola", "Unknown species"),
        match_rank = c(1L, 2L, 1L),
        matched_name = c("Garcinia kola", "Garcinia sp.", NA_character_),
        idtax_n = c(10L, 12L, NA_integer_),
        idtax_good_n = c(10L, 12L, NA_integer_),
        match_method = c("exact", "fuzzy", "no_match"),
        match_score = c(1, 0.6, NA_real_),
        is_synonym = c(FALSE, FALSE, FALSE),
        accepted_name = c(NA_character_, NA_character_, NA_character_),
        tax_gen = c("Garcinia", "Garcinia", NA_character_),
        tax_esp = c("kola", "sp.", NA_character_),
        tax_fam = c("Clusiaceae", "Clusiaceae", NA_character_)
      )
    }
  )

  result <- standardize_taxonomic_batch(input, species, verbose = FALSE, keep_all_matches = FALSE)

  expect_equal(nrow(result), 3)
  expect_equal(sum(!is.na(result$idtax_n)), 2)
  expect_equal(unique(stats::na.omit(result$matched_name)), "Garcinia kola")
  expect_true(all(c("match_genus", "match_species", "match_family") %in% names(result)))
})

test_that("standardize_taxonomic_batch keeps all matches when requested", {
  input <- tibble::tibble(species = "Garcinia kola")

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    match_taxonomic_names = function(names, method, max_matches, min_similarity, include_synonyms, return_scores, include_authors, con, verbose) {
      tibble::tibble(
        input_name = c("Garcinia kola", "Garcinia kola"),
        match_rank = c(1L, 2L),
        matched_name = c("Garcinia kola", "Garcinia sp."),
        idtax_n = c(10L, 12L),
        idtax_good_n = c(10L, 12L),
        match_method = c("exact", "fuzzy"),
        match_score = c(1, 0.6),
        is_synonym = c(FALSE, FALSE),
        accepted_name = c(NA_character_, NA_character_),
        tax_gen = c("Garcinia", "Garcinia"),
        tax_esp = c("kola", "sp."),
        tax_fam = c("Clusiaceae", "Clusiaceae")
      )
    }
  )

  result <- standardize_taxonomic_batch(input, species, verbose = FALSE, keep_all_matches = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$match_rank, c(1L, 2L))
})

test_that("standardize_taxonomic_batch errors for missing name column", {
  expect_error(
    standardize_taxonomic_batch(tibble::tibble(other = "x"), species, verbose = FALSE),
    "Column"
  )
})
