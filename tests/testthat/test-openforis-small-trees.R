# Tests for R/openforis_small_tree_processing.R
#
# A small-tree export names its plots three different ways and never names the
# quadrat a stem sits in. Everything here defends the two pieces of reasoning
# that close those gaps: normalising a plot name onto the database spelling,
# and assigning a stem to a quadrat by the tag range that `firsttag` opens.
#
# .apply_specimen_prefix() is covered too — it lives in openforis_processing.R
# but exists because the small-tree form records vouchers already prefixed.


# =============================================================================
# .normalise_plot_name()
# =============================================================================

test_that("separators are dropped and the trailing number zero-padded", {
  expect_equal(
    .normalise_plot_name(c("Mbalmayo1__", "Mbalmayo-09", "mbalmayo010_",
                           "Mbalmayo10_", "mbalmayo01")),
    c("mbalmayo001", "mbalmayo009", "mbalmayo010",
      "mbalmayo010", "mbalmayo001")
  )
})

test_that("every spelling of one plot collapses onto the same name", {
  # The three quadrats of a plot are typed with a growing number of trailing
  # underscores; all of them are the same parent plot.
  spellings <- c("Mbalmayo11", "Mbalmayo11_", "Mbalmayo11__")
  expect_length(unique(.normalise_plot_name(spellings)), 1)
})

test_that("a number already wide enough is left alone", {
  expect_equal(.normalise_plot_name("mbalmayo1234"), "mbalmayo1234")
  expect_equal(.normalise_plot_name("mbalmayo010"), "mbalmayo010")
})

test_that("padding can be switched off", {
  expect_equal(.normalise_plot_name("Mbalmayo1__", digits = NULL), "mbalmayo1")
})

test_that("a name with no trailing number survives untouched", {
  expect_equal(.normalise_plot_name("Bouamir"), "bouamir")
})

test_that("an explicit map wins over the derived name", {
  expect_equal(
    .normalise_plot_name(c("Mbalmayo1__", "odd one"),
                         map = c("odd one" = "mbalmayo099")),
    c("mbalmayo001", "mbalmayo099")
  )
})

test_that("NA in, NA out", {
  expect_true(is.na(.normalise_plot_name(NA_character_)))
})


# =============================================================================
# .apply_specimen_prefix()
# =============================================================================

test_that("a prefix already typed in the field is not doubled", {
  # "PIRD Pird 107" would be a different herbarium number, not a tidier one
  expect_equal(.apply_specimen_prefix("Pird 107", "PIRD"), "PIRD 107")
  expect_equal(.apply_specimen_prefix("PIRD 100", "PIRD"), "PIRD 100")
})

test_that("a bare number gets the prefix", {
  expect_equal(.apply_specimen_prefix(107, "PIRD"), "PIRD 107")
})

test_that("the prefix is applied in its own case, whatever was recorded", {
  expect_equal(.apply_specimen_prefix(c("pird-42", "PIRD_42"), "PIRD"),
               c("PIRD 42", "PIRD 42"))
})

test_that("applying twice changes nothing", {
  once <- .apply_specimen_prefix("Pird 107", "PIRD")
  expect_equal(.apply_specimen_prefix(once, "PIRD"), once)
})

test_that("blanks and NAs stay missing", {
  expect_true(all(is.na(.apply_specimen_prefix(c(NA, "", "  "), "PIRD"))))
})

test_that("no prefix returns the value untouched", {
  expect_equal(.apply_specimen_prefix("Pird 107", NULL), "Pird 107")
})


# =============================================================================
# .prepare_openforis_small_tree_quadrats()
# =============================================================================

fake_plot_file <- function(...) {
  base <- data.frame(
    plot_name = c("mbalmayo01", "mbalmayo01", "mbalmayo12"),
    quadrat = c(9, 13, 9),
    firsttag = c(1, 105, 1),
    stringsAsFactors = FALSE
  )
  args <- list(...)
  for (nm in names(args)) base[[nm]] <- args[[nm]]
  base
}

quadrat_codes <- data.frame(
  quadrat_code = c(9, 13, 17),
  quadrat_label_en = c("20_20", "40_40", "60_60"),
  stringsAsFactors = FALSE
)

test_that("each row becomes a plot named parent + quadrat label", {
  q <- .prepare_openforis_small_tree_quadrats(fake_plot_file(),
                                              quadrat_codes = quadrat_codes)
  expect_equal(sort(q$plot_name),
               c("mbalmayo001_20_20", "mbalmayo001_40_40", "mbalmayo012_20_20"))
  expect_equal(q$parent_plot_name[q$plot_name == "mbalmayo001_40_40"],
               "mbalmayo001")
})

test_that("without a code list the raw quadrat code names the plot", {
  q <- .prepare_openforis_small_tree_quadrats(fake_plot_file())
  expect_true("mbalmayo001_9" %in% q$plot_name)
})

test_that("a missing required column is an error naming it", {
  raw <- fake_plot_file()
  raw$firsttag <- NULL
  expect_error(.prepare_openforis_small_tree_quadrats(raw), "firsttag")
})

test_that("the same quadrat twice is a warning, not a silent merge", {
  raw <- fake_plot_file()
  raw$quadrat <- c(9, 9, 9)
  expect_warning(
    .prepare_openforis_small_tree_quadrats(raw, quadrat_codes = quadrat_codes),
    "more than once"
  )
})


# =============================================================================
# .assign_small_tree_quadrats()
# =============================================================================

fake_quadrats <- data.frame(
  row_id = 1:3,
  parent_plot_name = c("mbalmayo001", "mbalmayo001", "mbalmayo001"),
  quadrat = c("20_20", "40_40", "60_60"),
  quadrat_code = c(9, 13, 17),
  plot_name = c("mbalmayo001_20_20", "mbalmayo001_40_40", "mbalmayo001_60_60"),
  firsttag = c(1, 105, 216),
  stringsAsFactors = FALSE
)

fake_trees <- function(tag, parent = "mbalmayo001", raw = "Mbalmayo1") {
  data.frame(
    plot_name_raw = rep_len(raw, length(tag)),
    parent_plot_name = rep_len(parent, length(tag)),
    tag = tag,
    stringsAsFactors = FALSE
  )
}

test_that("a stem lands in the quadrat whose firsttag range holds its tag", {
  res <- .assign_small_tree_quadrats(fake_trees(c(1, 104, 105, 215, 216, 400)),
                                     fake_quadrats)
  expect_equal(res$trees$plot_name,
               c("mbalmayo001_20_20", "mbalmayo001_20_20",
                 "mbalmayo001_40_40", "mbalmayo001_40_40",
                 "mbalmayo001_60_60", "mbalmayo001_60_60"))
  expect_null(res$unassigned)
})

test_that("firsttag is inclusive — the boundary tag opens the next quadrat", {
  res <- .assign_small_tree_quadrats(fake_trees(105), fake_quadrats)
  expect_equal(res$trees$plot_name, "mbalmayo001_40_40")
})

test_that("a tag below every firsttag is held out with a reason", {
  res <- .assign_small_tree_quadrats(fake_trees(c(0, 5)), fake_quadrats)
  expect_equal(nrow(res$trees), 1)
  expect_equal(nrow(res$unassigned), 1)
  expect_match(res$unassigned$reason, "below the first firsttag")
})

test_that("an unknown parent plot is held out, not guessed at", {
  res <- .assign_small_tree_quadrats(fake_trees(1, parent = "elsewhere001"),
                                     fake_quadrats)
  expect_equal(nrow(res$trees), 0)
  expect_match(res$unassigned$reason, "absent from the plot file")
})

test_that("a missing tag is held out", {
  res <- .assign_small_tree_quadrats(fake_trees(NA_real_), fake_quadrats)
  expect_equal(nrow(res$trees), 0)
  expect_match(res$unassigned$reason, "missing tag")
})

test_that("a quadrat with no firsttag takes no stems", {
  q <- fake_quadrats
  q$firsttag <- NA_real_
  res <- .assign_small_tree_quadrats(fake_trees(1), q)
  expect_equal(nrow(res$trees), 0)
  expect_match(res$unassigned$reason, "no quadrat of this plot has a firsttag")
})


# =============================================================================
# .flag_small_tree_name_mismatches()
# =============================================================================

test_that("a raw spelling split across two quadrats reports its minority", {
  # Four stems typed "Mbalmayo1", but the tags put one of them past firsttag
  trees <- fake_trees(c(1, 2, 3, 105))
  res <- .assign_small_tree_quadrats(trees, fake_quadrats)

  expect_equal(nrow(res$mismatches), 1)
  expect_equal(res$mismatches$tag, 105)
  expect_equal(res$mismatches$majority_plot_name, "mbalmayo001_20_20")
})

test_that("consistent spellings raise nothing", {
  trees <- rbind(fake_trees(c(1, 2), raw = "Mbalmayo1"),
                 fake_trees(c(105, 106), raw = "Mbalmayo1_"))
  expect_null(.assign_small_tree_quadrats(trees, fake_quadrats)$mismatches)
})


# =============================================================================
# .flag_oversized_small_trees()
# =============================================================================

test_that("stems at or above the threshold are reported", {
  trees <- data.frame(
    plot_name = "mbalmayo001_20_20", parent_plot_name = "mbalmayo001",
    tag = 1:3, stem_diameter = c(2.5, 10, 10.4), stringsAsFactors = FALSE
  )
  res <- suppressMessages(.flag_oversized_small_trees(trees, dbh_max = 10))
  expect_equal(res$tag, 2:3)
})

test_that("an all-small plot reports nothing", {
  trees <- data.frame(plot_name = "p", tag = 1, stem_diameter = 4,
                      stringsAsFactors = FALSE)
  expect_null(.flag_oversized_small_trees(trees, dbh_max = 10))
})

test_that("the check can be switched off", {
  trees <- data.frame(plot_name = "p", tag = 1, stem_diameter = 40,
                      stringsAsFactors = FALSE)
  expect_null(.flag_oversized_small_trees(trees, dbh_max = NULL))
})
