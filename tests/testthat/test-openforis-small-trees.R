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

test_that("each row is a quadrat unit of the parent's small-tree plot", {
  q <- .prepare_openforis_small_tree_quadrats(fake_plot_file(),
                                              quadrat_codes = quadrat_codes)
  expect_equal(sort(q$quadrat_unit),
               c("mbalmayo001_20_20", "mbalmayo001_40_40", "mbalmayo012_20_20"))
  expect_equal(q$parent_plot_name[q$quadrat_unit == "mbalmayo001_40_40"],
               "mbalmayo001")
  expect_equal(sort(unique(q$plot_name)),
               c("mbalmayo001_smalltree", "mbalmayo012_smalltree"))
})

test_that("the small-tree plot takes the suffix given", {
  q <- .prepare_openforis_small_tree_quadrats(fake_plot_file(),
                                              plot_suffix = "_st")
  expect_setequal(q$plot_name, c("mbalmayo001_st", "mbalmayo012_st"))
})

test_that("without a code list the raw quadrat code names the quadrat", {
  q <- .prepare_openforis_small_tree_quadrats(fake_plot_file())
  expect_true("mbalmayo001_9" %in% q$quadrat_unit)
})

test_that("a quadrat with no parent plot name has no unit and no plot", {
  q <- .prepare_openforis_small_tree_quadrats(
    fake_plot_file(plot_name = c("mbalmayo01", NA, "mbalmayo12"))
  )
  expect_equal(sum(is.na(q$quadrat_unit)), 1)
  expect_equal(sum(is.na(q$plot_name)), 1)
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
  plot_name = "mbalmayo001_smalltree",
  quadrat_unit = c("mbalmayo001_20_20", "mbalmayo001_40_40", "mbalmayo001_60_60"),
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
  expect_equal(res$trees$quadrat_unit,
               c("mbalmayo001_20_20", "mbalmayo001_20_20",
                 "mbalmayo001_40_40", "mbalmayo001_40_40",
                 "mbalmayo001_60_60", "mbalmayo001_60_60"))
  expect_equal(res$trees$quadrat_label,
               c("20_20", "20_20", "40_40", "40_40", "60_60", "60_60"))
  expect_null(res$unassigned)
})

test_that("every stem of a parent plot lands in its one small-tree plot", {
  res <- .assign_small_tree_quadrats(fake_trees(c(1, 105, 216)), fake_quadrats)
  expect_equal(unique(res$trees$plot_name), "mbalmayo001_smalltree")
})

test_that("firsttag is inclusive — the boundary tag opens the next quadrat", {
  res <- .assign_small_tree_quadrats(fake_trees(105), fake_quadrats)
  expect_equal(res$trees$quadrat_unit, "mbalmayo001_40_40")
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
  expect_equal(res$mismatches$quadrat_by_tag, "40_40")
  expect_equal(res$mismatches$quadrat_by_name, "20_20")
  expect_equal(res$mismatches$keep, "tag")
})

test_that("consistent spellings raise nothing", {
  trees <- rbind(fake_trees(c(1, 2), raw = "Mbalmayo1"),
                 fake_trees(c(105, 106), raw = "Mbalmayo1_"))
  expect_null(.assign_small_tree_quadrats(trees, fake_quadrats)$mismatches)
})


# =============================================================================
# .apply_small_tree_quadrat_decisions()
# =============================================================================

# Tag 105 was typed with the first quadrat's spelling: a stem forgotten in
# 20_20 and tagged from the 40_40 range, or a mistyped underscore.
disputed <- function() {
  .assign_small_tree_quadrats(fake_trees(c(1, 2, 3, 105)), fake_quadrats)
}

decide <- function(res, decisions) {
  suppressMessages(.apply_small_tree_quadrat_decisions(
    res$trees, res$mismatches, decisions, fake_quadrats
  ))
}

test_that("keep = name moves the stem to the quadrat its name gives", {
  res <- disputed()
  dec <- res$mismatches
  dec$keep <- "name"

  out <- decide(res, dec)
  stem <- out$trees[out$trees$tag == 105, ]
  expect_equal(stem$quadrat_unit, "mbalmayo001_20_20")
  expect_equal(stem$quadrat_label, "20_20")
  expect_equal(stem$plot_name, "mbalmayo001_smalltree")
  expect_equal(out$mismatches$keep, "name")
})

test_that("keep = tag, or a blank keep, leaves the stem where its tag put it", {
  res <- disputed()
  for (k in c("tag", "", NA, " TAG ")) {
    dec <- res$mismatches
    dec$keep <- k
    out <- decide(res, dec)
    expect_equal(out$trees$quadrat_unit[out$trees$tag == 105],
                 "mbalmayo001_40_40")
    expect_equal(out$mismatches$keep, "tag")
  }
})

test_that("an edited quadrat_by_name sends the stem to that quadrat", {
  res <- disputed()
  dec <- res$mismatches
  dec$keep <- "name"
  dec$quadrat_by_name <- "60_60"

  out <- decide(res, dec)
  expect_equal(out$trees$quadrat_unit[out$trees$tag == 105],
               "mbalmayo001_60_60")
  expect_equal(out$mismatches$quadrat_by_name, "60_60")
})

test_that("a decision needs only plot, tag and keep", {
  res <- disputed()
  out <- decide(res, data.frame(parent_plot_name = "mbalmayo001",
                                tag = "105", keep = "name"))
  expect_equal(out$trees$quadrat_unit[out$trees$tag == 105],
               "mbalmayo001_20_20")
})

test_that("an unflagged stem can be moved when a quadrat is given", {
  res <- disputed()
  out <- decide(res, data.frame(parent_plot_name = "mbalmayo001", tag = 2,
                                keep = "name", quadrat_by_name = "40_40"))
  expect_equal(out$trees$quadrat_unit[out$trees$tag == 2], "mbalmayo001_40_40")
  # Recorded alongside the flagged one, which kept its tag
  expect_equal(out$mismatches$tag, c(2, 105))
  expect_equal(out$mismatches$quadrat_by_tag, c("20_20", "40_40"))
  expect_equal(out$mismatches$keep, c("name", "tag"))
})

test_that("mismatches left out of the decisions are announced", {
  res <- disputed()
  expect_message(
    .apply_small_tree_quadrat_decisions(
      res$trees, res$mismatches,
      data.frame(parent_plot_name = "mbalmayo001", tag = 1, keep = "tag"),
      fake_quadrats
    ),
    "not in"
  )
})

test_that("decisions that cannot be applied as written are errors", {
  res <- disputed()
  row <- function(...) {
    utils::modifyList(list(parent_plot_name = "mbalmayo001", tag = 105,
                           keep = "name"), list(...))
  }
  bad <- function(...) as.data.frame(row(...), stringsAsFactors = FALSE)

  expect_error(decide(res, bad(keep = "maybe")), "\"tag\" or \"name\"")
  expect_error(decide(res, bad(tag = 999)), "absent from the tree list")
  expect_error(decide(res, bad(quadrat_by_name = "99_99")), "no sampled quadrat")
  expect_error(decide(res, bad(tag = 2)), "no quadrat_by_name")
  expect_error(decide(res, rbind(bad(), bad())), "more than one row")
  expect_error(decide(res, data.frame(tag = 105, keep = "name")),
               "parent_plot_name")
})

test_that("a duplicated stem cannot be decided on", {
  res <- .assign_small_tree_quadrats(fake_trees(c(1, 2, 105, 105)),
                                     fake_quadrats)
  expect_error(
    decide(res, data.frame(parent_plot_name = "mbalmayo001", tag = 105,
                           keep = "name", quadrat_by_name = "20_20")),
    "two stems of the plot share"
  )
})

test_that("a tag used in two quadrats is told apart by its raw spelling", {
  # Quadrat 20_20 was tagged on to 106 after 40_40 had opened at 105: tags
  # 105-106 exist twice, and the range puts both copies in 40_40
  trees <- rbind(fake_trees(c(1, 2, 3, 105, 106), raw = "Mbalmayo1"),
                 fake_trees(c(105, 106, 107, 108), raw = "Mbalmayo1_"))
  res <- .assign_small_tree_quadrats(trees, fake_quadrats)
  expect_equal(res$mismatches$plot_name_raw, c("Mbalmayo1", "Mbalmayo1"))

  dec <- res$mismatches
  dec$keep <- "name"
  out <- decide(res, dec)

  unit <- function(raw, t) {
    out$trees$quadrat_unit[out$trees$plot_name_raw == raw & out$trees$tag == t]
  }
  expect_equal(unit("Mbalmayo1", 105), "mbalmayo001_20_20")
  expect_equal(unit("Mbalmayo1", 106), "mbalmayo001_20_20")
  expect_equal(unit("Mbalmayo1_", 105), "mbalmayo001_40_40")
  expect_equal(unit("Mbalmayo1_", 106), "mbalmayo001_40_40")
  expect_equal(out$mismatches$keep, c("name", "name"))
})

test_that("decisions are read from an xlsx, by path or by name in data_dir", {
  skip_if_not_installed("writexl")
  res <- disputed()
  dec <- res$mismatches
  dec$keep <- "name"
  dir <- withr::local_tempdir()
  writexl::write_xlsx(dec, file.path(dir, "reviewed.xlsx"))

  out <- suppressMessages(.apply_small_tree_quadrat_decisions(
    res$trees, res$mismatches, "reviewed.xlsx", fake_quadrats, data_dir = dir
  ))
  expect_equal(out$trees$quadrat_unit[out$trees$tag == 105],
               "mbalmayo001_20_20")
  expect_error(
    .apply_small_tree_quadrat_decisions(res$trees, res$mismatches,
                                        "nowhere.xlsx", fake_quadrats),
    "not found"
  )
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


# =============================================================================
# One small-tree plot per parent plot
# =============================================================================

fake_per_quadrat <- data.frame(
  quadrat_unit = c("mbalmayo001_20_20", "mbalmayo001_40_40", "mbalmayo001_60_60"),
  country = "Cameroon",
  method = c("small-tree quadrat", "small-tree quadrat", NA),
  team_leader = c("Ann", "Bob", "Ann"),
  additional_people = c("Cid; Dee", "Dee", NA),
  forest_type = c("terra firme", "terra firme, swamp", NA),
  date_y = c(2026L, 2026L, 2026L),
  date_m = c(3L, 2L, 3L),
  date_d = c(10L, 28L, 11L),
  stringsAsFactors = FALSE
)

test_that("the quadrats of a parent plot pool into one plot linked to it", {
  res <- .collapse_small_tree_plots(fake_per_quadrat, fake_quadrats,
                                    quadrat_area = 0.04)
  expect_equal(nrow(res), 1)
  expect_equal(res$plot_name, "mbalmayo001_smalltree")
  expect_equal(res$parent_plot_name, "mbalmayo001")
  expect_equal(res$parent_relation, "nested_subsample")
  expect_equal(res$quadrats, "20_20, 40_40, 60_60")
  expect_equal(res$plot_area, 0.12)
})

test_that("the area is left out when no quadrat area is given", {
  res <- .collapse_small_tree_plots(fake_per_quadrat, fake_quadrats)
  expect_false("plot_area" %in% names(res))
})

test_that("people and forest types are pooled, a constant is kept", {
  res <- .collapse_small_tree_plots(fake_per_quadrat, fake_quadrats)
  expect_equal(res$team_leader, "Ann, Bob")
  expect_equal(res$additional_people, "Cid, Dee")
  expect_equal(res$forest_type, "terra firme, swamp")
  expect_equal(res$country, "Cameroon")
  expect_equal(res$method, "small-tree quadrat")
})

test_that("the earliest date is kept and the disagreement announced", {
  expect_message(
    res <- .collapse_small_tree_plots(fake_per_quadrat, fake_quadrats),
    "disagree"
  )
  expect_equal(c(res$date_y, res$date_m, res$date_d), c(2026L, 2L, 28L))
})

test_that("a conflicting constant keeps its first value and is announced", {
  pq <- fake_per_quadrat
  pq$date_m <- 3L
  pq$date_d <- 10L
  pq$country <- c("Cameroon", "Gabon", "Cameroon")
  msgs <- testthat::capture_messages(
    res <- .collapse_small_tree_plots(pq, fake_quadrats)
  )
  expect_equal(res$country, "Cameroon")
  expect_true(any(grepl("country differs", msgs)))
  expect_false(any(grepl("different dates", msgs)))
})

test_that("two parent plots give two small-tree plots", {
  q <- rbind(fake_quadrats, data.frame(
    row_id = 4L, parent_plot_name = "mbalmayo012", quadrat = "20_20",
    quadrat_code = 9, plot_name = "mbalmayo012_smalltree",
    quadrat_unit = "mbalmayo012_20_20", firsttag = 1, stringsAsFactors = FALSE
  ))
  pq <- rbind(fake_per_quadrat, transform(fake_per_quadrat[1, ],
                                          quadrat_unit = "mbalmayo012_20_20"))
  res <- suppressMessages(.collapse_small_tree_plots(pq, q, quadrat_area = 0.04))
  expect_equal(res$plot_name, c("mbalmayo001_smalltree", "mbalmayo012_smalltree"))
  expect_equal(res$plot_area, c(0.12, 0.04))
})


# =============================================================================
# .combine_small_tree_quadrat() / .check_small_tree_plot_suffix()
# =============================================================================

test_that("quadrat and sub-quadrat are joined", {
  expect_equal(
    .combine_small_tree_quadrat(c("20_20", "20_20", "40_40", NA),
                                c("A", NA, " ", "B")),
    c("20_20_A", "20_20", "40_40", NA)
  )
})

test_that("no sub-quadrat column leaves the quadrat alone", {
  expect_equal(.combine_small_tree_quadrat("20_20", NULL), "20_20")
})

test_that("a usable suffix passes", {
  expect_silent(.check_small_tree_plot_suffix("_smalltree"))
})

test_that("an empty or digit-ending suffix is refused", {
  expect_error(.check_small_tree_plot_suffix(""), "non-empty")
  expect_error(.check_small_tree_plot_suffix(NA_character_), "non-empty")
  expect_error(.check_small_tree_plot_suffix(c("_a", "_b")), "non-empty")
  expect_error(.check_small_tree_plot_suffix("_d10"), "digit")
})


# =============================================================================
# process_openforis_small_trees() end to end
# =============================================================================

write_small_tree_export <- function(dir) {
  writexl::write_xlsx(data.frame(
    plot_name = c("Mbalmayo1", "Mbalmayo1", "Mbalmayo12"),
    quadrat = c(9, 13, 9),
    firsttag = c(1, 4, 1),
    date_year = 2026, date_month = 3, date_day = c(10, 11, 12),
    add_people = c("Ann", "Bob", "Ann")
  ), file.path(dir, "plot.xlsx"))

  writexl::write_xlsx(data.frame(
    plot_plot_name_old = c("Mbalmayo1", "Mbalmayo1", "Mbalmayo1",
                           "Mbalmayo1_", "Mbalmayo1_", "Mbalmayo12"),
    tag = c(1, 2, 3, 4, 5, 1),
    quadrat = c("A", "A", "B", "C", "D", "A"),
    dbh = c(2, 3, 4, 5, 6, 7),
    taxa_code = c(10, 10, 11, 12, 12, 13),
    taxa_scientific_name = c("Sp a", "Sp a", "Sp b", "Sp c", "Sp c", "Sp d"),
    multi_stem = c("no", "no", "yes", "no", "no", "no"),
    number_stem = c(NA, NA, 2, NA, NA, NA)
  ), file.path(dir, "tree_list.xlsx"))
}

test_that("an export becomes one small-tree plot per parent plot", {
  skip_if_not_installed("writexl")
  dir <- withr::local_tempdir()
  write_small_tree_export(dir)

  res <- suppressWarnings(suppressMessages(
    process_openforis_small_trees(data_dir = dir, method = "small-tree quadrat",
                                  country = "Cameroon")
  ))

  expect_equal(res$plots$plot_name,
               c("mbalmayo001_smalltree", "mbalmayo012_smalltree"))
  expect_equal(res$plots$parent_plot_name, c("mbalmayo001", "mbalmayo012"))
  expect_equal(res$plots$parent_relation, rep("nested_subsample", 2))
  expect_equal(res$plots$plot_area, c(0.08, 0.04))
  expect_equal(res$plots$date_d, c(10L, 12L))

  expect_equal(nrow(res$quadrats), 3)
  expect_equal(res$quadrats$n_stems, c(3L, 2L, 1L))

  ind <- res$individuals
  expect_equal(unique(ind$plot_name[ind$parent_plot_name == "mbalmayo001"]),
               "mbalmayo001_smalltree")
  expect_equal(ind$quadrat[ind$plot_name == "mbalmayo001_smalltree"],
               c("9_A", "9_A", "9_B", "13_C", "13_D"))
  expect_true(all(res$individuals_wide$plot_name %in% res$plots$plot_name))
})

test_that("a multi-stem group does not reach into the next quadrat", {
  skip_if_not_installed("writexl")
  dir <- withr::local_tempdir()
  write_small_tree_export(dir)

  res <- suppressWarnings(suppressMessages(
    process_openforis_small_trees(data_dir = dir)
  ))

  # Tag 3 opens a 2-stem group but is the last stem of its quadrat; tag 4
  # starts the next one and must not be its second stem.
  expect_equal(res$multi_stems$plot_name, "mbalmayo001_smalltree")
  expect_equal(res$multi_stems$tag, 3)
  expect_true(is.na(res$individuals$multi_tiges_id[
    res$individuals$plot_name == "mbalmayo001_smalltree" &
      res$individuals$tag == 4
  ]))
})

test_that("a reviewed mismatch file sends a stem to the quadrat its name gives", {
  skip_if_not_installed("writexl")
  dir <- withr::local_tempdir()
  write_small_tree_export(dir)
  # Tag 4 was typed with the first quadrat's spelling: forgotten in quadrat 9
  # and tagged once the team had moved on to quadrat 13
  trees <- as.data.frame(readxl::read_excel(file.path(dir, "tree_list.xlsx")))
  trees$plot_plot_name_old[trees$tag == 4] <- "Mbalmayo1"
  writexl::write_xlsx(trees, file.path(dir, "tree_list.xlsx"))
  mm_file <- file.path(dir, "mismatches.xlsx")

  first <- suppressWarnings(suppressMessages(
    process_openforis_small_trees(data_dir = dir,
                                  mismatch_file = "mismatches.xlsx")
  ))
  expect_true(file.exists(mm_file))
  expect_equal(first$name_mismatches$tag, 4)
  expect_equal(first$name_mismatches$keep, "tag")
  expect_equal(first$individuals$quadrat[first$individuals$tag == 4 &
                                            first$individuals$plot_name == "mbalmayo001_smalltree"],
               "13_C")

  reviewed <- as.data.frame(readxl::read_excel(mm_file))
  reviewed$keep <- "name"
  writexl::write_xlsx(reviewed, mm_file)

  second <- suppressWarnings(suppressMessages(
    process_openforis_small_trees(data_dir = dir,
                                  quadrat_decisions = "mismatches.xlsx")
  ))
  ind <- second$individuals[second$individuals$plot_name == "mbalmayo001_smalltree", ]
  expect_equal(ind$quadrat[ind$tag == 4], "9_C")
  expect_equal(second$name_mismatches$keep, "name")
  expect_equal(second$summary$n_placed_by_name, 1L)
  expect_equal(second$quadrats$n_stems, c(4L, 1L, 1L))
  # Now in the same quadrat as tag 3, tag 4 is the second stem of its group
  expect_equal(ind$multi_tiges_id[ind$tag == 4], 3)
})

test_that("a relative review-file path lives in data_dir", {
  expect_equal(.resolve_in_data_dir("mm.xlsx", "D:/mission"), "D:/mission/mm.xlsx")
  expect_equal(.resolve_in_data_dir("sub/mm.xlsx", "D:/mission"),
               "D:/mission/sub/mm.xlsx")
  expect_equal(.resolve_in_data_dir("D:/elsewhere/mm.xlsx", "D:/mission"),
               "D:/elsewhere/mm.xlsx")
  expect_equal(.resolve_in_data_dir("/tmp/mm.xlsx", "D:/mission"), "/tmp/mm.xlsx")
  expect_equal(.resolve_in_data_dir("~/mm.xlsx", "D:/mission"), "~/mm.xlsx")
  expect_equal(.resolve_in_data_dir("mm.xlsx", NULL), "mm.xlsx")
})

test_that("two stems sharing a tag keep their own quadrat in the individuals", {
  skip_if_not_installed("writexl")
  dir <- withr::local_tempdir()
  write_small_tree_export(dir)
  # Quadrat 9 was tagged on to 4 after quadrat 13 had opened at 4
  trees <- as.data.frame(readxl::read_excel(file.path(dir, "tree_list.xlsx")))
  trees <- rbind(trees, transform(trees[trees$tag == 3, ], tag = 4, quadrat = "D"))
  writexl::write_xlsx(trees, file.path(dir, "tree_list.xlsx"))

  first <- suppressWarnings(suppressMessages(
    process_openforis_small_trees(data_dir = dir,
                                  mismatch_file = "mismatches.xlsx")
  ))
  dec <- as.data.frame(readxl::read_excel(file.path(dir, "mismatches.xlsx")))
  expect_equal(dec$tag, 4)
  dec$keep <- "name"
  writexl::write_xlsx(dec, file.path(dir, "mismatches.xlsx"))

  res <- suppressWarnings(suppressMessages(
    process_openforis_small_trees(data_dir = dir,
                                  quadrat_decisions = "mismatches.xlsx")
  ))
  ind <- res$individuals[res$individuals$plot_name == "mbalmayo001_smalltree" &
                           res$individuals$tag == 4, ]
  expect_setequal(ind$quadrat, c("9_D", "13_C"))
})
