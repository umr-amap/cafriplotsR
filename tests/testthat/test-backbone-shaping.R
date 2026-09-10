# The app matches against a downloaded copy of table_taxa, while
# match_taxonomic_names() queries the live table. Anything .shape_backbone()
# drops is therefore a name the app cannot find but the package can — the
# exact asymmetry that made "Wilczekra congolensis" unmatchable in the app.

.taxa_fixture <- function() {
  data.frame(
    idtax_n      = 1:5,
    idtax_good_n = 1:5,
    tax_fam      = c("Celastraceae", "Clusiaceae", "Fabaceae", "Fabaceae", "Poaceae"),
    tax_famclass = rep("Magnoliopsida", 5),
    tax_gen      = c("Wilczekra", "Garcinia", "Anthonotha", "Brachystegia", "Panicum"),
    tax_esp      = c("congolensis", "kola", "macrophylla", NA, "maximum"),
    tax_rank01   = c(NA, NA, "var.", NA, NA),
    tax_nam01    = c(NA, NA, "oblongifolia", NA, NA),
    tax_rank02   = NA_character_,
    tax_nam02    = NA_character_,
    tax_level    = c("species", "species", "variety", "genus", "species"),
    author1      = c(NA, "Heckel", "P.Beauv.", "Benth.", "ZZ auct."),
    stringsAsFactors = FALSE
  )
}

test_that(".shape_backbone() keeps taxa that have no recorded author", {
  shaped <- .shape_backbone(.taxa_fixture())

  expect_true("Wilczekra congolensis" %in% shaped$tax_sp_level)
  expect_true(1L %in% shaped$idtax_n)
})

test_that(".shape_backbone() still drops the ZZ auct. placeholder", {
  shaped <- .shape_backbone(.taxa_fixture())

  expect_false(5L %in% shaped$idtax_n)
  expect_false("ZZ auct." %in% shaped$author1)
  expect_equal(nrow(shaped), 4L)
})

test_that(".shape_backbone() derives the level keys the matching stages join on", {
  shaped <- .shape_backbone(.taxa_fixture())
  sp <- stats::setNames(shaped$tax_sp_level, shaped$idtax_n)

  expect_equal(unname(sp[["2"]]), "Garcinia kola")
  # Infraspecific rank and epithet are folded into the species key
  expect_equal(unname(sp[["3"]]), "Anthonotha macrophylla var. oblongifolia")
  # Genus-level rows have no species key at all
  expect_true(is.na(unname(sp[["4"]])))

  expect_equal(shaped$tax_gen_level, shaped$tax_gen)
  expect_equal(shaped$tax_fam_level, shaped$tax_fam)
  expect_equal(shaped$tax_class_level, shaped$tax_famclass)
})

test_that("a taxon with no author is matched exactly once it is in the backbone", {
  # End to end over the fix: shape the table the way the download does, then
  # match the name the way the app does.
  backbone <- .shape_backbone(.taxa_fixture())

  res <- match_taxonomic_names(
    "Wilczekra congolensis (R. Wilczek) M.P. Simmons",
    backbone = backbone,
    include_authors = TRUE,
    verbose = FALSE
  )

  expect_equal(nrow(res), 1L)
  expect_equal(res$match_method[1], "exact")
  expect_equal(res$idtax_n[1], 1L)
})
