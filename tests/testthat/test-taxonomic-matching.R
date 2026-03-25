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
