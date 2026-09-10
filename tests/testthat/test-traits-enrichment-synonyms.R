# Traits enrichment for taxa matched to a synonym.
#
# Trait measurements are keyed by the taxon they were recorded on, which may be
# any member of a synonym group. The enrichment module used to ask for the
# matched id alone and join the answer back on that same id; both are wrong for
# a synonym, and between them they meant no name matched to a synonym ever
# showed a single trait. These lock the two halves of the fix.

.enrich_matched_taxa <- function(...) {
  base <- list(
    idtax_n       = c(1437L, 351086L),
    idtax_good_n  = c(351086L, NA_integer_),
    matched_name  = c("Dictyandra arborescens", "Leptactina arborescens"),
    corrected_name = c("Leptactina arborescens", "Leptactina arborescens")
  )

  overrides <- list(...)
  base[names(overrides)] <- overrides

  as.data.frame(base, stringsAsFactors = FALSE)
}


# ---------------------------------------------------------------------------
# .with_accepted_idtax()
# ---------------------------------------------------------------------------

test_that(".with_accepted_idtax() keys a synonym on its accepted taxon", {
  res <- .with_accepted_idtax(.enrich_matched_taxa())

  # Dictyandra arborescens is a synonym of Leptactina arborescens: the traits
  # come back under 351086, so that is the key it has to carry.
  expect_equal(res$idtax_resolved[1], 351086L)
})


test_that(".with_accepted_idtax() leaves an accepted taxon on its own id", {
  res <- .with_accepted_idtax(.enrich_matched_taxa())

  expect_equal(res$idtax_resolved[2], 351086L)
  expect_true(is.na(res$idtax_good_n[2]))
})


test_that(".with_accepted_idtax() preserves the other columns and row count", {
  input <- .enrich_matched_taxa()
  res <- .with_accepted_idtax(input)

  expect_equal(nrow(res), nrow(input))
  expect_equal(res$matched_name, input$matched_name)
  expect_equal(res$idtax_n, input$idtax_n)
  expect_true("idtax_resolved" %in% names(res))
})


test_that(".with_accepted_idtax() uses the same rule as resolve_taxon_synonyms()", {
  # resolve_taxon_synonyms() is ifelse(is.na(idtax_good_n), idtax_n,
  # idtax_good_n), and query_taxa_traits() relabels measurements with it. Any
  # divergence here silently empties the join, so keep the rule identical.
  input <- .enrich_matched_taxa(
    idtax_n      = c(10L, 20L, 30L),
    idtax_good_n = c(NA_integer_, 1L, 99L),
    matched_name = c("a", "b", "c"),
    corrected_name = c("a", "b", "c")
  )

  expect_equal(.with_accepted_idtax(input)$idtax_resolved, c(10L, 1L, 99L))
})


# ---------------------------------------------------------------------------
# .trait_group_idtax()
# ---------------------------------------------------------------------------

test_that(".trait_group_idtax() asks for the accepted id, not just the matched one", {
  testthat::local_mocked_bindings(
    resolve_taxon_synonyms = function(idtax, ...) {
      data.frame(idtax = idtax, idtax_good = idtax)
    }
  )

  ids <- .trait_group_idtax(.enrich_matched_taxa())

  expect_true(1437L %in% ids)
  expect_true(351086L %in% ids)
})


test_that(".trait_group_idtax() widens to the rest of the synonym group", {
  # A sibling synonym can hold the measurements, and only the taxa database
  # knows it exists.
  testthat::local_mocked_bindings(
    resolve_taxon_synonyms = function(idtax, ...) {
      data.frame(idtax = c(idtax, 777L), idtax_good = 351086L)
    }
  )

  expect_true(777L %in% .trait_group_idtax(.enrich_matched_taxa()))
})


test_that(".trait_group_idtax() returns a sorted set with no duplicates", {
  testthat::local_mocked_bindings(
    resolve_taxon_synonyms = function(idtax, ...) {
      data.frame(idtax = c(idtax, idtax), idtax_good = idtax)
    }
  )

  ids <- .trait_group_idtax(.enrich_matched_taxa())

  expect_equal(ids, sort(unique(ids)))
})


test_that(".trait_group_idtax() falls back to matched and accepted ids on error", {
  testthat::local_mocked_bindings(
    resolve_taxon_synonyms = function(...) stop("connection refused")
  )

  ids <- suppressMessages(.trait_group_idtax(.enrich_matched_taxa()))

  # Degraded, but still an improvement on the matched id alone: the accepted
  # taxon is where the traits usually sit.
  expect_equal(ids, c(1437L, 351086L))
})


test_that(".trait_group_idtax() drops NA ids", {
  testthat::local_mocked_bindings(
    resolve_taxon_synonyms = function(idtax, ...) {
      data.frame(idtax = idtax, idtax_good = idtax)
    }
  )

  ids <- .trait_group_idtax(.enrich_matched_taxa(
    idtax_n      = c(1437L, 351086L),
    idtax_good_n = c(NA_integer_, NA_integer_)
  ))

  expect_false(anyNA(ids))
  expect_equal(ids, c(1437L, 351086L))
})


test_that(".trait_group_idtax() handles an empty set without touching the database", {
  testthat::local_mocked_bindings(
    resolve_taxon_synonyms = function(...) stop("must not be called")
  )

  empty <- .enrich_matched_taxa()[0, ]
  expect_length(.trait_group_idtax(empty), 0)
})


# ---------------------------------------------------------------------------
# The join the module performs
# ---------------------------------------------------------------------------

test_that("traits keyed by the accepted taxon reach a matched synonym", {
  # What the module does, in miniature: query_taxa_traits() returns idtax
  # rewritten to the accepted taxon, so joining on idtax_n dropped every
  # synonym. Joining on idtax_resolved is what makes the row land.
  matched <- .with_accepted_idtax(.enrich_matched_taxa())

  traits <- data.frame(idtax = 351086L, taxa_wood_density_mean = 0.62)

  joined <- dplyr::left_join(matched, traits,
                             by = c("idtax_resolved" = "idtax"))

  synonym_row <- joined[joined$idtax_n == 1437L, ]
  expect_equal(synonym_row$taxa_wood_density_mean, 0.62)

  # The old key would have found nothing at all.
  old <- dplyr::left_join(matched, traits, by = c("idtax_n" = "idtax"))
  expect_true(is.na(old$taxa_wood_density_mean[old$idtax_n == 1437L]))
})


test_that("two synonyms of one taxon both receive the group's traits", {
  matched <- .with_accepted_idtax(.enrich_matched_taxa(
    idtax_n        = c(1437L, 888L, 351086L),
    idtax_good_n   = c(351086L, 351086L, NA_integer_),
    matched_name   = c("Dictyandra arborescens", "Another synonym",
                       "Leptactina arborescens"),
    corrected_name = rep("Leptactina arborescens", 3)
  ))

  traits <- data.frame(idtax = 351086L, taxa_wood_density_mean = 0.62)

  joined <- dplyr::left_join(matched, traits,
                             by = c("idtax_resolved" = "idtax"))

  expect_equal(nrow(joined), 3L)
  expect_false(anyNA(joined$taxa_wood_density_mean))
})
