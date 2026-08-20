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


# =============================================================================
# .warn_unused_openforis_columns()
# =============================================================================

# angle and distance_to_next_stem are relevant only when the plot sets
# distance_stems = Yes, so every export seen so far leaves them empty. The
# warning exists for the day a team switches them on.

test_that("a populated column with nowhere to go is announced", {
  trees <- data.frame(angle = c(30, NA), distance_to_next_stem = c(NA, NA),
                      stringsAsFactors = FALSE)
  expect_message(.warn_unused_openforis_columns(trees), "angle")
})

test_that("an empty column says nothing", {
  trees <- data.frame(angle = c(NA_real_, NA_real_),
                      taxa_vernacular_name = c(NA_character_, ""),
                      stringsAsFactors = FALSE)
  expect_silent(.warn_unused_openforis_columns(trees))
})

test_that("a form without those fields at all says nothing", {
  expect_silent(.warn_unused_openforis_columns(data.frame(tag = 1)))
})


# =============================================================================
# .specimen_key() / .remap_specimen_column()
# =============================================================================

# A remap table is keyed on bare numbers (1 -> 4530), but the OpenForis export
# spells the same voucher two ways in two columns: specimen_nbr holds 107 and
# the calculated specimen_name holds "Pird 107". Both have to be reached, or
# the pair ends up naming two different collections.

test_that("a large number is not keyed in scientific notation", {
  expect_equal(.specimen_key(c(1234567, 1e6)), c("1234567", "1000000"))
})

test_that("numeric and character spellings of one number key alike", {
  expect_equal(.specimen_key(107), .specimen_key("107"))
})

test_that("a bare number is substituted", {
  r <- .remap_specimen_column(c(1, 2, 107), old = c(1, 2), new = c(4530, 4531))
  expect_equal(r$value, c("4530", "4531", "107"))
  expect_equal(r$matched, c(TRUE, TRUE, FALSE))
  expect_false(any(r$by_digits))
})

test_that("a prefixed number is reached and keeps its prefix", {
  r <- .remap_specimen_column(c("Pird 1", "PIRD 2"), old = c(1, 2),
                              new = c(4530, 4531))
  expect_equal(r$value, c("Pird 4530", "PIRD 4531"))
  expect_true(all(r$by_digits))
})

test_that("a literal match is preferred over the digit fallback", {
  # "1" as recorded maps to 4530; it must not be re-read as the digits of "1"
  r <- .remap_specimen_column("1", old = c("1", "01"), new = c(4530, 9999))
  expect_equal(r$value, "4530")
  expect_false(r$by_digits)
})

test_that("blanks and NAs are left alone", {
  r <- .remap_specimen_column(c(NA, ""), old = 1, new = 4530)
  expect_equal(r$value, c(NA, ""))
  expect_false(any(r$matched))
})

test_that("a number absent from the table is untouched", {
  r <- .remap_specimen_column("Pird 999", old = 1, new = 4530)
  expect_equal(r$value, "Pird 999")
  expect_false(r$matched)
})


# =============================================================================
# .remap_specimen_numbers()
# =============================================================================

write_remap <- function(old, new, dir = tempdir()) {
  f <- file.path(dir, paste0("remap-", basename(tempfile()), ".xlsx"))
  writexl::write_xlsx(data.frame(old = old, new = new), f)
  f
}

remap_trees <- function() {
  data.frame(
    specimen_number = c(1, 2, NA),
    herbarium_nbe_char = c("Pird 1", "Pird 2", NA),
    stringsAsFactors = FALSE
  )
}

test_that("both voucher columns are substituted and the originals kept", {
  f <- write_remap(c(1, 2), c(4530, 4531))
  out <- suppressMessages(.remap_specimen_numbers(remap_trees(), f))

  expect_equal(out$specimen_number, c("4530", "4531", NA))
  expect_equal(out$herbarium_nbe_char, c("Pird 4530", "Pird 4531", NA))
  expect_equal(out$specimen_number_original, c(1, 2, NA))
  expect_equal(out$herbarium_nbe_char_original, c("Pird 1", "Pird 2", NA))
})

test_that("a repeated replacement number is refused", {
  # Two vouchers renumbered onto one number would merge two collections
  f <- write_remap(c(1, 2), c(4530, 4530))
  expect_error(.remap_specimen_numbers(remap_trees(), f), "Duplicate new")
})

test_that("a repeated original number is refused", {
  f <- write_remap(c(1, 1), c(4530, 4531))
  expect_error(.remap_specimen_numbers(remap_trees(), f), "Duplicate old")
})

test_that("a missing file is an error naming it", {
  expect_error(.remap_specimen_numbers(remap_trees(), "no-such-file.xlsx"),
               "not found")
})

test_that("a bare filename is resolved against data_dir", {
  f <- write_remap(c(1, 2), c(4530, 4531))
  out <- suppressMessages(
    .remap_specimen_numbers(remap_trees(), basename(f), dirname(f))
  )
  expect_equal(out$specimen_number, c("4530", "4531", NA))
})

test_that("a table matching nothing is announced rather than applied quietly", {
  f <- write_remap(c(900, 901), c(4530, 4531))
  expect_message(.remap_specimen_numbers(remap_trees(), f),
                 "matched the remap table")
})

test_that("a stem with a number but no herbarium code is not called a split", {
  # That is a gap in the export, reported elsewhere -- not a half-applied remap
  trees <- data.frame(specimen_number = c(1, 2),
                      herbarium_nbe_char = c("Pird 1", NA),
                      stringsAsFactors = FALSE)
  f <- write_remap(c(1, 2), c(4530, 4531))
  msgs <- testthat::capture_messages(.remap_specimen_numbers(trees, f))
  expect_false(any(grepl("different collections", msgs)))
})
