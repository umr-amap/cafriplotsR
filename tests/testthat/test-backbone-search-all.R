# A name being added to the database usually exists in several backbones, and
# its identifier is worth recording in each. These tests cover the two pieces
# that decide what gets linked without a database: which hit of a backbone is
# unambiguous enough to link by itself, and how the synonymy suggestions of
# several backbones are merged into one list.

hit <- function(backbone, external_id, taxon_name, match_type = "exact") {
  dplyr::tibble(
    backbone = backbone, external_id = external_id,
    taxon_name = taxon_name, match_type = match_type
  )
}

test_that("an exact hit in each backbone is selected, one per backbone", {
  res <- rbind(
    hit("apd", "A1", "Diospyros iturensis"),
    hit("wcvp", "W1", "Diospyros iturensis")
  )
  sel <- .auto_backbone_selection(res, "Diospyros iturensis")

  expect_equal(sort(names(sel)), c("apd", "wcvp"))
  expect_equal(sel[["apd"]], "A1")
  expect_equal(sel[["wcvp"]], "W1")
})

test_that("a backbone with no exact hit contributes nothing", {
  res <- rbind(
    hit("apd", "A1", "Diospyros iturensis"),
    hit("wcvp", "W9", "Diospyros iturensiana", match_type = "fuzzy")
  )
  sel <- .auto_backbone_selection(res, "Diospyros iturensis")

  expect_equal(names(sel), "apd")
})

test_that("two names of the same spelling are ambiguous and left alone", {
  # A homonym, or a name both accepted and synonymised: a person must choose.
  res <- rbind(
    hit("apd", "A1", "Diospyros iturensis"),
    hit("apd", "A2", "Diospyros iturensis")
  )
  expect_length(.auto_backbone_selection(res, "Diospyros iturensis"), 0)
})

test_that("an exact hit on a different name is not selected", {
  # The infraspecifics of a binomial come back as exact matches too; they are
  # not the name that was searched for.
  res <- rbind(
    hit("apd", "A1", "Diospyros iturensis var. alba"),
    hit("apd", "A2", "Diospyros iturensis subsp. beta")
  )
  expect_length(.auto_backbone_selection(res, "Diospyros iturensis"), 0)
})

test_that("case and surrounding spaces do not prevent selection", {
  res <- hit("apd", "A1", " diospyros ITURENSIS ")
  sel <- .auto_backbone_selection(res, "Diospyros iturensis")
  expect_equal(sel[["apd"]], "A1")
})

test_that("empty and malformed input selects nothing", {
  expect_length(.auto_backbone_selection(NULL, "Diospyros iturensis"), 0)
  expect_length(.auto_backbone_selection(hit("apd", "A1", "x")[0, ], "x"), 0)
  expect_length(.auto_backbone_selection(hit("apd", "A1", "x"), ""), 0)
  expect_length(.auto_backbone_selection(hit("apd", "A1", "x"), NA_character_), 0)
  expect_length(.auto_backbone_selection(data.frame(a = 1), "x"), 0)
})

test_that("search_all_backbones() rejects a name it cannot search", {
  # No database is touched: the name is refused before any connection.
  for (bad in list("", "   ", NA_character_, c("a", "b"), 1)) {
    out <- search_all_backbones(bad)
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 0)
  }
})

test_that("search_all_backbones() returns the documented columns when empty", {
  out <- search_all_backbones("")
  expect_true(all(c("backbone", "backbone_name", "is_name_source",
                    "external_id", "taxon_name", "match_type") %in% names(out)))
})

# ---- Merging what several backbones suggest ----

cand <- function(idtax_n, backbone_label, backbone_name, status_raw = "Accepted") {
  data.frame(
    idtax_n = idtax_n, backbone = tolower(backbone_label),
    backbone_label = backbone_label, backbone_name = backbone_name,
    status_raw = status_raw, tax_gen = "Diospyros", tax_esp = "iturensis",
    stringsAsFactors = FALSE
  )
}

test_that("a taxon suggested by two backbones is offered once, with both", {
  merged <- .merge_synonymy_candidates(list(
    cand(101, "APD", "Diospyros alba"),
    cand(101, "WCVP", "Diospyros alba", status_raw = "Synonym")
  ))

  expect_equal(nrow(merged), 1)
  expect_equal(merged$idtax_n, 101)
  expect_match(merged$sources, "APD: Diospyros alba [Accepted]", fixed = TRUE)
  expect_match(merged$sources, "WCVP: Diospyros alba [Synonym]", fixed = TRUE)
})

test_that("taxa suggested by only one backbone are all kept", {
  merged <- .merge_synonymy_candidates(list(
    cand(101, "APD", "Diospyros alba"),
    cand(202, "WCVP", "Diospyros beta")
  ))

  expect_equal(sort(merged$idtax_n), c(101, 202))
  expect_equal(merged$sources[merged$idtax_n == 202], "WCVP: Diospyros beta [Accepted]")
})

test_that("identical evidence from one backbone is not repeated", {
  merged <- .merge_synonymy_candidates(list(
    rbind(cand(101, "APD", "Diospyros alba"), cand(101, "APD", "Diospyros alba"))
  ))
  expect_equal(merged$sources, "APD: Diospyros alba [Accepted]")
})

test_that("a missing status is left out rather than shown as NA", {
  merged <- .merge_synonymy_candidates(list(
    cand(101, "APD", "Diospyros alba", status_raw = NA_character_)
  ))
  expect_equal(merged$sources, "APD: Diospyros alba")
})

test_that("nothing suggested gives NULL, not an empty frame", {
  expect_null(.merge_synonymy_candidates(list()))
  expect_null(.merge_synonymy_candidates(list(NULL, NULL)))
  expect_null(.merge_synonymy_candidates(list(cand(1, "APD", "x")[0, ])))
})
