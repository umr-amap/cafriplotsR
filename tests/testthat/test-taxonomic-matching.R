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

test_that('.match_single_name_sql prefers exact matches before later strategies', {
  parsed <- list(
    input_name = 'Garcinia kola',
    original_input = 'Garcinia kola',
    genus = 'Garcinia'
  )

  calls <- character()

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    .match_exact_sql = function(parsed, con, include_authors, max_matches) {
      calls <<- c(calls, 'exact')
      tibble::tibble(
        input_name = parsed$input_name,
        matched_name = 'Garcinia kola',
        idtax_n = 1L,
        idtax_good_n = 1L,
        match_method = 'exact',
        match_score = 1,
        tax_gen = 'Garcinia',
        tax_esp = 'kola',
        tax_fam = 'Clusiaceae',
        tax_level = 'species'
      )
    },
    .match_genus_constrained_sql = function(...) {
      calls <<- c(calls, 'genus')
      tibble::tibble()
    },
    .match_fuzzy_sql = function(...) {
      calls <<- c(calls, 'fuzzy')
      tibble::tibble()
    }
  )

  result <- CafriplotsR:::.match_single_name_sql(parsed, structure(list(), class = 'mock_con'), 'auto', 5, 0.3, FALSE, FALSE)

  expect_equal(calls, 'exact')
  expect_equal(result$match_rank, 1L)
  expect_equal(result$match_method, 'exact')
})

test_that('.match_single_name_sql returns a no_match row when all strategies fail', {
  parsed <- list(
    input_name = 'Unknown species',
    original_input = 'Unknown species',
    genus = 'Unknown'
  )

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    .match_exact_sql = function(...) tibble::tibble(),
    .match_genus_constrained_sql = function(...) tibble::tibble(),
    .match_fuzzy_sql = function(...) tibble::tibble()
  )

  result <- CafriplotsR:::.match_single_name_sql(parsed, structure(list(), class = 'mock_con'), 'auto', 5, 0.3, FALSE, FALSE)

  expect_equal(nrow(result), 1)
  expect_equal(result$match_method, 'no_match')
  expect_true(is.na(result$idtax_n))
  expect_equal(result$match_rank, 1)
})

test_that('.match_exact_sql handles class and species-level result formatting', {
  mock_con <- structure(list(), class = 'mock_con')
  fetch_calls <- 0L

  testthat::local_mocked_bindings(
    .package = 'glue',
    glue_sql = function(..., .con = NULL) 'SELECT mocked SQL'
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    func_try_fetch = function(con, sql) {
      fetch_calls <<- fetch_calls + 1L
      if (fetch_calls == 1L) {
        tibble::tibble(
          idtax_n = c(1L, 2L),
          idtax_good_n = c(1L, 2L),
          tax_gen = c(NA_character_, NA_character_),
          tax_esp = c(NA_character_, NA_character_),
          tax_fam = c('Fabaceae', 'Fabaceae'),
          tax_level = c('higher', 'higher'),
          matched_name = c('Magnoliopsida', 'Magnoliopsida'),
          similarity_score = c(1, 1)
        )
      } else {
        tibble::tibble(
          idtax_n = 10L,
          idtax_good_n = 10L,
          tax_gen = 'Garcinia',
          tax_esp = 'kola',
          tax_fam = 'Clusiaceae',
          tax_level = 'species',
          tax_rank01 = NA_character_,
          tax_nam01 = NA_character_,
          tax_rank02 = NA_character_,
          tax_nam02 = NA_character_,
          author1 = NA_character_,
          author2 = NA_character_,
          author3 = NA_character_,
          matched_name = 'Garcinia kola',
          similarity_score = 1
        )
      }
    }
  )

  class_result <- CafriplotsR:::.match_exact_sql(
    list(rank = 'class', full_name_no_auth = 'Magnoliopsida', input_name = 'Magnoliopsida', original_input = 'Magnoliopsida'),
    mock_con,
    FALSE,
    5
  )
  species_result <- CafriplotsR:::.match_exact_sql(
    list(rank = 'species', full_name_no_auth = 'Garcinia kola', input_name = 'Garcinia kola', original_input = 'Garcinia kola'),
    mock_con,
    FALSE,
    5
  )

  expect_equal(nrow(class_result), 1)
  expect_equal(class_result$match_method, 'exact')
  expect_equal(class_result$matched_name, 'Magnoliopsida')
  expect_equal(species_result$matched_name, 'Garcinia kola')
  expect_equal(species_result$tax_gen, 'Garcinia')
})

test_that('.match_genus_constrained_sql returns empty for missing genus and formats matches otherwise', {
  mock_con <- structure(list(), class = 'mock_con')

  empty_result <- CafriplotsR:::.match_genus_constrained_sql(
    list(genus = NA_character_, input_name = 'Unknown', original_input = 'Unknown'),
    mock_con,
    0.3,
    FALSE,
    5
  )
  expect_equal(nrow(empty_result), 0)

  fetch_calls <- 0L
  testthat::local_mocked_bindings(
    .package = 'glue',
    glue_sql = function(..., .con = NULL) 'SELECT mocked SQL'
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    func_try_fetch = function(con, sql) {
      fetch_calls <<- fetch_calls + 1L
      if (fetch_calls == 1L) {
        tibble::tibble(tax_gen = 'Garcinia', genus_sim = 0.92)
      } else {
        tibble::tibble(
          idtax_n = 10L,
          idtax_good_n = 10L,
          tax_gen = 'Garcinia',
          tax_esp = 'kola',
          tax_fam = 'Clusiaceae',
          tax_level = 'species',
          tax_rank01 = NA_character_,
          tax_nam01 = NA_character_,
          tax_rank02 = NA_character_,
          tax_nam02 = NA_character_,
          author1 = NA_character_,
          author2 = NA_character_,
          author3 = NA_character_,
          matched_name = 'Garcinia kola',
          similarity_score = 0.91
        )
      }
    }
  )

  matched <- CafriplotsR:::.match_genus_constrained_sql(
    list(genus = 'Garcinia', input_name = 'Garcinea kola', original_input = 'Garcinea kola'),
    mock_con,
    0.3,
    FALSE,
    5
  )

  expect_equal(matched$match_method, 'genus_constrained')
  expect_equal(matched$match_score, 0.91)
  expect_equal(matched$input_name, 'Garcinea kola')
})

test_that('.match_fuzzy_sql handles order fallback errors and class-level fuzzy matches', {
  mock_con <- structure(list(), class = 'mock_con')

  testthat::local_mocked_bindings(
    .package = 'glue',
    glue_sql = function(..., .con = NULL) 'SELECT mocked SQL'
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    func_try_fetch = function(con, sql) stop('tax_order missing')
  )

  order_result <- CafriplotsR:::.match_fuzzy_sql(
    list(rank = 'order', full_name_no_auth = 'Malpighiales', input_name = 'Malpighiales', original_input = 'Malpighiales'),
    mock_con,
    0.3,
    FALSE,
    5
  )
  expect_equal(nrow(order_result), 0)

  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    func_try_fetch = function(con, sql) {
      tibble::tibble(
        idtax_n = 1L,
        idtax_good_n = 1L,
        tax_gen = NA_character_,
        tax_esp = NA_character_,
        tax_fam = 'Fabaceae',
        tax_level = 'higher',
        matched_name = 'Magnoliopsida',
        similarity_score = 0.88
      )
    }
  )

  class_result <- CafriplotsR:::.match_fuzzy_sql(
    list(rank = 'class', full_name_no_auth = 'Magnoliopsda', input_name = 'Magnoliopsda', original_input = 'Magnoliopsda'),
    mock_con,
    0.3,
    FALSE,
    5
  )

  expect_equal(class_result$match_method, 'fuzzy')
  expect_equal(class_result$match_score, 0.88)
  expect_equal(class_result$matched_name, 'Magnoliopsida')
})

test_that('.add_synonym_info_sql annotates synonyms and preserves accepted taxa', {
  matches <- tibble::tibble(
    input_name = c('Synonym name', 'Accepted name'),
    matched_name = c('Synonym name', 'Accepted name'),
    idtax_n = c(10L, 20L),
    idtax_good_n = c(11L, 20L),
    match_method = c('exact', 'exact'),
    match_score = c(1, 1),
    tax_gen = c('Garcinia', 'Gilbertiodendron'),
    tax_esp = c('kola', 'dewevrei'),
    tax_fam = c('Clusiaceae', 'Fabaceae'),
    tax_level = c('species', 'species')
  )

  testthat::local_mocked_bindings(
    .package = 'glue',
    glue_sql = function(..., .con = NULL) 'SELECT mocked SQL'
  )
  testthat::local_mocked_bindings(
    .package = 'CafriplotsR',
    func_try_fetch = function(con, sql) {
      tibble::tibble(idtax_n = 11L, accepted_name = 'Garcinia kola')
    }
  )

  result <- CafriplotsR:::.add_synonym_info_sql(matches, structure(list(), class = 'mock_con'))

  expect_equal(result$is_synonym, c(TRUE, FALSE))
  expect_equal(result$accepted_name[[1]], 'Garcinia kola')
  expect_true(is.na(result$accepted_name[[2]]))
})
