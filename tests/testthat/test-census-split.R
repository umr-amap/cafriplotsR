# Tests for R/census_split.R
# split_census_table() is pure when `existing` is supplied — no DB required.

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

make_existing <- function() {
  data.frame(
    plot_name     = rep("P1", 4),
    tag           = c("101", "102", "103", "110"),
    id_n          = c(11L, 12L, 13L, 20L),
    idtax_n       = c(500L, 500L, 600L, 700L),
    stem_grouping = c(NA, NA, NA, NA),
    last_status   = c("alive", "alive", "dead", "alive"),
    stringsAsFactors = FALSE
  )
}

make_census <- function() {
  data.frame(
    plot_name = rep("P1", 4),
    tag       = c("101", "102", "201", "110"),
    dbh       = c(21.1, 33.4, 10.2, 45.0),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# .normalize_tag()
# =============================================================================

test_that(".normalize_tag trims whitespace and blanks to NA", {
  expect_equal(.normalize_tag(c(" 101 ", "102", "", "  ")),
               c("101", "102", NA, NA))
})

test_that(".normalize_tag keeps large numeric tags out of scientific notation", {
  # as.character(100000) is "1e+05" — that would break every match
  expect_equal(.normalize_tag(100000), "100000")
  expect_equal(.normalize_tag(c(1, 250000, 1234567)),
               c("1", "250000", "1234567"))
})

test_that(".normalize_tag drops trailing zero decimals from Excel numerics", {
  expect_equal(.normalize_tag(c(101, 102)), c("101", "102"))
})

test_that(".normalize_tag preserves NA", {
  expect_equal(.normalize_tag(c("101", NA)), c("101", NA))
  expect_equal(.normalize_tag(c(101, NA)), c("101", NA))
})

test_that(".normalize_tag makes numeric and character tags comparable", {
  expect_equal(.normalize_tag(101L), .normalize_tag("101"))
})

test_that(".normalize_tag handles zero-length input", {
  expect_equal(.normalize_tag(character(0)), character(0))
})

# =============================================================================
# .tag_numeric()
# =============================================================================

test_that(".tag_numeric parses plain numbers only", {
  expect_equal(.tag_numeric(c("101", "1O3", "A12", NA, "12.5")),
               c(101, NA, NA, NA, 12.5))
})

# =============================================================================
# .is_adjacent_transposition()
# =============================================================================

test_that(".is_adjacent_transposition detects a swapped pair", {
  expect_true(.is_adjacent_transposition("1243", "1234"))
  expect_true(.is_adjacent_transposition("2134", "1234"))
})

test_that(".is_adjacent_transposition rejects non-transpositions", {
  expect_false(.is_adjacent_transposition("1234", "1234"))   # identical
  expect_false(.is_adjacent_transposition("1235", "1234"))   # substitution
  expect_false(.is_adjacent_transposition("4231", "1234"))   # non-adjacent swap
  expect_false(.is_adjacent_transposition("123", "1234"))    # length differs
  expect_false(.is_adjacent_transposition(NA, "1234"))
  expect_false(.is_adjacent_transposition("1", "2"))         # too short
})

# =============================================================================
# .nearest_tags()
# =============================================================================

test_that(".nearest_tags finds a distance-1 neighbour", {
  res <- .nearest_tags("1O3", c("101", "102", "103"), max_dist = 1L)
  expect_equal(res$nearest_tag, "103")
  expect_equal(res$distance, 1L)
  expect_false(res$transposed)
})

test_that(".nearest_tags reports nothing when everything is far away", {
  res <- .nearest_tags("9999", c("101", "102"), max_dist = 1L)
  expect_true(is.na(res$nearest_tag))
})

test_that(".nearest_tags catches transpositions beyond the distance threshold", {
  res <- .nearest_tags("1243", c("1234", "5678"), max_dist = 1L)
  expect_equal(res$nearest_tag, "1234")
  expect_true(res$transposed)
  expect_equal(res$distance, 2L)
})

test_that(".nearest_tags copes with an empty pool or no candidates", {
  expect_equal(nrow(.nearest_tags(character(0), c("101"))), 0)
  res <- .nearest_tags("101", character(0))
  expect_equal(nrow(res), 1)
  expect_true(is.na(res$nearest_tag))
})

test_that(".nearest_tags ignores NA candidates without erroring", {
  res <- .nearest_tags(c(NA, "1O3"), c("103"), max_dist = 1L)
  expect_true(is.na(res$nearest_tag[1]))
  expect_equal(res$nearest_tag[2], "103")
})

# =============================================================================
# split_census_table() — core classification
# =============================================================================

test_that("split_census_table separates remeasures from recruits", {
  res <- split_census_table(make_census(), existing = make_existing())

  expect_s3_class(res, "census_split")
  expect_equal(res$data$row_role, c("remeasure", "remeasure", "recruit", "remeasure"))
  expect_equal(nrow(res$remeasures), 3)
  expect_equal(nrow(res$recruits), 1)
  expect_equal(res$recruits$tag, "201")
})

test_that("split_census_table attaches id_n to remeasures only", {
  res <- split_census_table(make_census(), existing = make_existing())

  expect_equal(res$data$id_n, c(11L, 12L, NA_integer_, 20L))
  expect_true(all(is.na(res$recruits$id_n)))
})

test_that("split_census_table keeps every original column and row", {
  census <- make_census()
  res <- split_census_table(census, existing = make_existing())

  expect_equal(nrow(res$data), nrow(census))
  expect_true(all(names(census) %in% names(res$data)))
  expect_equal(res$data$dbh, census$dbh)
  expect_equal(res$data$row_id, seq_len(nrow(census)))
})

test_that("split_census_table matches numeric file tags against character db tags", {
  census <- data.frame(plot_name = "P1", tag = 101, stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = make_existing())

  expect_equal(res$data$row_role, "remeasure")
  expect_equal(res$data$id_n, 11L)
})

test_that("split_census_table treats everything as a recruit when the plot is empty", {
  empty <- make_existing()[0, ]
  res <- split_census_table(make_census(), plot_names = "P1", existing = empty)

  expect_true(all(res$data$row_role == "recruit"))
  expect_equal(nrow(res$missing_stems), 0)
})

# =============================================================================
# split_census_table() — typo guard
# =============================================================================

test_that("split_census_table holds a likely mistyped tag for review", {
  census <- data.frame(
    plot_name = c("P1", "P1"),
    tag       = c("1O3", "201"),   # letter O for zero, then a real recruit
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())

  expect_equal(res$data$row_role, c("review", "recruit"))
  expect_equal(nrow(res$possible_typos), 1)
  expect_equal(res$possible_typos$nearest_tag, "103")
  expect_equal(res$possible_typos$nearest_id_n, 13L)
  expect_match(res$data$split_note[1], "103")
})

test_that("review rows are excluded from recruits", {
  census <- data.frame(plot_name = "P1", tag = "1O3", stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = make_existing())

  expect_equal(nrow(res$recruits), 0)
  expect_equal(nrow(res$review), 1)
})

test_that("a tag continuing the plot's numbering stays a recruit", {
  # 111 is edit-distance 1 from the existing 110, but it is the next number up
  census <- data.frame(plot_name = "P1", tag = "111", stringsAsFactors = FALSE)

  expect_equal(split_census_table(census, existing = make_existing())$data$row_role,
               "recruit")

  # ...unless the caller says tags are not sequential
  res <- split_census_table(census, existing = make_existing(),
                            assume_new_block = FALSE)
  expect_equal(res$data$row_role, "review")
})

test_that("a tag falling back inside the used range is still reviewed", {
  # 104 is below the maximum existing tag (110), so it cannot be a continuation
  census <- data.frame(plot_name = "P1", tag = "104", stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = make_existing())

  expect_equal(res$data$row_role, "review")
})

test_that("typo_max_dist = 0 disables the guard", {
  census <- data.frame(plot_name = "P1", tag = "1O3", stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = make_existing(), typo_max_dist = 0)

  expect_equal(res$data$row_role, "recruit")
  expect_equal(nrow(res$possible_typos), 0)
})

test_that("the typo guard only compares tags within the same plot", {
  existing <- data.frame(
    plot_name = c("P1", "P2"), tag = c("103", "555"),
    id_n = c(13L, 55L), stringsAsFactors = FALSE
  )
  census <- data.frame(plot_name = "P1", tag = "554", stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = existing)

  # 554 is distance 1 from P2's 555, but that is another plot
  expect_equal(res$data$row_role, "recruit")
})

# =============================================================================
# split_census_table() — invalid rows, duplicates, scope
# =============================================================================

test_that("split_census_table flags rows without a usable tag or plot", {
  census <- data.frame(
    plot_name = c("P1", "P1", NA),
    tag       = c("101", NA, "999"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, plot_names = "P1", existing = make_existing())

  expect_equal(res$data$row_role, c("remeasure", "invalid", "invalid"))
  expect_equal(nrow(res$invalid), 2)
  expect_match(res$data$split_note[2], "tag")
  expect_match(res$data$split_note[3], "plot")
})

test_that("split_census_table reports repeated plot + tag combinations", {
  census <- data.frame(
    plot_name = c("P1", "P1", "P1"),
    tag       = c("101", "101", "102"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())

  expect_equal(nrow(res$duplicates), 1)
  expect_equal(res$duplicates$tag, "101")
  expect_equal(res$duplicates$n, 2L)
})

test_that("split_census_table sets aside rows for plots outside the split", {
  census <- data.frame(
    plot_name = c("P1", "P9"),
    tag       = c("101", "700"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, plot_names = "P1", existing = make_existing())

  expect_equal(nrow(res$out_of_scope), 1)
  expect_equal(res$data$row_role, c("remeasure", "out_of_scope"))
  expect_equal(nrow(res$recruits), 0)
})

# =============================================================================
# split_census_table() — taxon drift and missing stems
# =============================================================================

test_that("split_census_table reports a remeasure carrying a new taxon", {
  census <- data.frame(
    plot_name = c("P1", "P1"),
    tag       = c("101", "102"),
    idtax_n   = c(500L, 999L),   # 102 is 500 in the database
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())

  expect_equal(nrow(res$taxon_drift), 1)
  expect_equal(res$taxon_drift$tag, "102")
  expect_equal(res$taxon_drift$idtax_file, "999")
  expect_equal(res$taxon_drift$idtax_db, "500")
})

test_that("taxon drift ignores rows with no taxon in the file", {
  census <- data.frame(
    plot_name = c("P1", "P1"), tag = c("101", "102"),
    idtax_n = c(NA, 500L), stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())
  expect_equal(nrow(res$taxon_drift), 0)
})

test_that("split_census_table lists recorded stems absent from the table", {
  # existing has 101, 102, 103 (dead), 110; the census covers 101, 102, 110
  res <- split_census_table(make_census(), existing = make_existing())

  # 103 is excluded because it is already recorded dead
  expect_equal(nrow(res$missing_stems), 0)
})

test_that("missing stems are reported when they are not already dead", {
  existing <- make_existing()
  existing$last_status <- c("alive", "alive", "alive", "alive")
  res <- split_census_table(make_census(), existing = existing)

  expect_equal(res$missing_stems$tag, "103")
  expect_equal(res$missing_stems$id_n, 13L)
  expect_equal(res$missing_stems$last_status, "alive")
})

test_that("exclude_status is configurable", {
  res <- split_census_table(make_census(), existing = make_existing(),
                            exclude_status = character(0))
  expect_equal(res$missing_stems$tag, "103")
})

test_that("missing stems work when no status is recorded at all", {
  existing <- make_existing()
  existing$last_status <- NA_character_
  res <- split_census_table(make_census(), existing = existing)

  expect_equal(res$missing_stems$tag, "103")
})

# =============================================================================
# split_census_table() — summary
# =============================================================================

test_that("split_census_table summarises per plot", {
  census <- data.frame(
    plot_name = c("P1", "P1", "P2", "P2"),
    tag       = c("101", "201", "301", "302"),
    stringsAsFactors = FALSE
  )
  existing <- data.frame(
    plot_name = c("P1", "P1", "P2"),
    tag       = c("101", "102", "301"),
    id_n      = c(11L, 12L, 31L),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = existing)

  expect_equal(nrow(res$summary), 2)
  p1 <- res$summary[res$summary$plot_name == "P1", ]
  expect_equal(p1$n_rows, 2L)
  expect_equal(p1$n_remeasure, 1L)
  expect_equal(p1$n_recruit, 1L)
  expect_equal(p1$n_missing, 1L)   # tag 102 not in the census
  expect_equal(p1$n_in_db, 2L)

  p2 <- res$summary[res$summary$plot_name == "P2", ]
  expect_equal(p2$n_remeasure, 1L)
  expect_equal(p2$n_recruit, 1L)
})

test_that("the summary accounts for every in-scope row", {
  res <- split_census_table(make_census(), existing = make_existing())
  totals <- with(res$summary,
                 sum(n_remeasure + n_recruit + n_review + n_invalid))
  expect_equal(totals, nrow(res$data))
})

# =============================================================================
# split_census_table() — argument checking
# =============================================================================

test_that("split_census_table rejects bad input", {
  census <- make_census()

  expect_error(split_census_table("not a data frame"), "data frame")
  expect_error(split_census_table(census[, "dbh", drop = FALSE],
                                  existing = make_existing()),
               "missing column")
  expect_error(split_census_table(census), "`existing` or `con`")
  expect_error(split_census_table(census, existing = make_existing(),
                                  typo_max_dist = -1), "non-negative")
})

test_that("split_census_table requires plot_name and tag in `existing`", {
  bad <- data.frame(plot_name = "P1", id_n = 1L, stringsAsFactors = FALSE)
  expect_error(split_census_table(make_census(), existing = bad),
               "`plot_name` and `tag`")
})

test_that("split_census_table tolerates `existing` without optional columns", {
  minimal <- data.frame(
    plot_name = c("P1", "P1"), tag = c("101", "102"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(make_census(), existing = minimal)

  expect_equal(res$data$row_role[1:2], c("remeasure", "remeasure"))
  expect_true(all(is.na(res$data$id_n)))
  expect_equal(nrow(res$taxon_drift), 0)
})

test_that("custom column names are honoured", {
  census <- data.frame(
    parcelle = "P1", no_arbre = "101", stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing(),
                            plot_col = "parcelle", tag_col = "no_arbre")

  expect_equal(res$data$row_role, "remeasure")
  expect_equal(res$data$id_n, 11L)
})

# =============================================================================
# print.census_split()
# =============================================================================

test_that("print.census_split reports the counts and returns invisibly", {
  res <- split_census_table(make_census(), existing = make_existing())

  # cli writes to stderr under Rscript, so capture that stream explicitly
  out <- paste(capture.output(print(res), type = "message"), collapse = " ")
  expect_match(out, "remeasure")
  expect_match(out, "recruit")

  expect_invisible(print(res))
})

test_that("print.census_split surfaces the review and duplicate warnings", {
  census <- data.frame(
    plot_name = c("P1", "P1", "P1"),
    tag       = c("1O3", "101", "101"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())

  out <- paste(capture.output(print(res), type = "message"), collapse = " ")
  expect_match(out, "review")
  expect_match(out, "more than once")
})

# =============================================================================
# .fetch_plot_individuals() — exercised against a real SQL engine
#
# data_individuals has no plot_name column; it only reaches one through
# id_table_liste_plots_n. A query that forgets the join runs clean against a
# mock and returns nothing against the real database, so these tests use
# SQLite rather than a stub.
# =============================================================================

make_sqlite_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "data_liste_plots", data.frame(
    id_liste_plots = c(1L, 2L),
    plot_name      = c("P1", "P2"),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "data_individuals", data.frame(
    id_n                   = c(11L, 12L, 21L),
    id_table_liste_plots_n = c(1L, 1L, 2L),
    tag                    = c("101", "102", "301"),
    idtax_n                = c(500L, 600L, 700L),
    stem_grouping          = c(NA_integer_, 11L, NA_integer_),
    stringsAsFactors = FALSE
  ))
  con
}

test_that(".fetch_plot_individuals resolves plot_name through the plots table", {
  skip_if_not_installed("RSQLite")

  con <- make_sqlite_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- suppressMessages(.fetch_plot_individuals("P1", con))

  expect_equal(nrow(res), 2)
  expect_equal(sort(res$tag), c("101", "102"))
  expect_true(all(res$plot_name == "P1"))
  expect_equal(sort(res$id_n), c(11L, 12L))
  expect_true(all(c("idtax_n", "stem_grouping", "last_status") %in% names(res)))
})

test_that(".fetch_plot_individuals restricts to the plots asked for", {
  skip_if_not_installed("RSQLite")

  con <- make_sqlite_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(nrow(suppressMessages(.fetch_plot_individuals("P2", con))), 1)
  expect_equal(nrow(suppressMessages(.fetch_plot_individuals(c("P1", "P2"), con))), 3)
  expect_equal(nrow(suppressMessages(.fetch_plot_individuals("P9", con))), 0)
})

test_that(".fetch_plot_individuals survives a failing status query", {
  skip_if_not_installed("RSQLite")

  # No data_traits_measures table here, so the status lookup must fail softly
  con <- make_sqlite_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_message(res <- .fetch_plot_individuals("P1", con), "stem status")
  expect_equal(nrow(res), 2)
  expect_true(all(is.na(res$last_status)))
})

test_that(".fetch_plot_individuals skips the status query when asked", {
  skip_if_not_installed("RSQLite")

  con <- make_sqlite_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_silent(res <- .fetch_plot_individuals("P1", con, with_status = FALSE))
  expect_true(all(is.na(res$last_status)))
})

test_that(".fetch_plot_individuals returns an empty frame without touching the DB", {
  res <- .fetch_plot_individuals(character(0), con = NULL)

  expect_equal(nrow(res), 0)
  expect_true(all(c("plot_name", "id_n", "tag", "idtax_n",
                    "stem_grouping", "last_status") %in% names(res)))
})

test_that("split_census_table fetches from a connection when `existing` is absent", {
  skip_if_not_installed("RSQLite")

  con <- make_sqlite_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  census <- data.frame(
    plot_name = c("P1", "P1"),
    tag       = c("101", "201"),
    stringsAsFactors = FALSE
  )
  res <- suppressMessages(
    split_census_table(census, plot_names = "P1", con = con)
  )

  expect_equal(res$data$row_role, c("remeasure", "recruit"))
  expect_equal(res$data$id_n, c(11L, NA_integer_))
  expect_equal(res$missing_stems$tag, "102")
})

# =============================================================================
# A tag that names more than one recorded stem
#
# Plots that numbered tags per quadrat carry repeated plot + tag pairs — 989
# of the 2166 plots in the production database hold at least one. match()
# returns the first of them without a word, which would attach a census to the
# wrong tree. The columns that once told those stems apart are no longer
# maintained, so the split reports the collision rather than resolving it.
# =============================================================================

make_shared_tag_existing <- function() {
  data.frame(
    plot_name   = rep("P1", 3),
    tag         = c("5", "5", "6"),
    id_n        = c(11L, 12L, 13L),
    idtax_n     = c(500L, 600L, 700L),
    last_status = rep("alive", 3),
    stringsAsFactors = FALSE
  )
}

test_that("a tag matching several stems is held for review, not guessed at", {
  census <- data.frame(plot_name = "P1", tag = "5", dbh = 12,
                       stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())

  expect_equal(res$data$row_role, "review")
  expect_true(is.na(res$data$id_n))
  expect_equal(nrow(res$ambiguous), 1)
  expect_equal(res$ambiguous$n_matches, 2L)
  expect_equal(res$ambiguous$id_n_candidates, "11, 12")
  expect_match(res$data$split_note, "cannot tell which one")
})

test_that("an unambiguous tag in the same plot is unaffected", {
  census <- data.frame(plot_name = c("P1", "P1"), tag = c("5", "6"),
                       stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())

  expect_equal(res$data$row_role, c("review", "remeasure"))
  expect_equal(res$data$id_n, c(NA_integer_, 13L))
})

test_that("ambiguous rows are not counted as typos", {
  census <- data.frame(plot_name = "P1", tag = "5", stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())

  expect_equal(nrow(res$possible_typos), 0)
  expect_equal(nrow(res$review), 1)
})

test_that("the summary counts ambiguous rows alongside review", {
  census <- data.frame(plot_name = c("P1", "P1"), tag = c("5", "6"),
                       stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())

  expect_equal(res$summary$n_ambiguous, 1L)
  expect_equal(res$summary$n_review, 1L)
})

test_that("the summary still accounts for every in-scope row", {
  census <- data.frame(plot_name = rep("P1", 3), tag = c("5", "6", "900"),
                       stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())
  totals <- with(res$summary,
                 sum(n_remeasure + n_recruit + n_review + n_invalid))
  expect_equal(totals, nrow(res$data))
})

test_that("print reports ambiguity separately from typos", {
  census <- data.frame(plot_name = "P1", tag = "5", stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = make_shared_tag_existing())

  out <- paste(capture.output(print(res), type = "message"), collapse = " ")
  expect_match(out, "more than one recorded stem")
})

test_that("a repeated tag with no file row is not reported as ambiguous", {
  census <- data.frame(plot_name = "P1", tag = "6", stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())

  expect_equal(nrow(res$ambiguous), 0)
  expect_equal(sort(res$missing_stems$id_n), c(11L, 12L))
})

test_that("idtax_n never decides whether a row is a remeasure", {
  # An identification revised between censuses must not turn the stem into a
  # recruit — it is reported as drift and matched on the tag as usual
  census <- data.frame(plot_name = "P1", tag = "6", idtax_n = 999L,
                       stringsAsFactors = FALSE)

  res <- split_census_table(census, existing = make_shared_tag_existing())

  expect_equal(res$data$row_role, "remeasure")
  expect_equal(res$data$id_n, 13L)
  expect_equal(nrow(res$taxon_drift), 1)
  expect_equal(res$taxon_drift$idtax_file, "999")
  expect_equal(res$taxon_drift$idtax_db, "700")
})

# =============================================================================
# export_census_split()
# =============================================================================

test_that("export_census_split writes the expected files", {
  skip_if_not_installed("writexl")

  census <- data.frame(
    plot_name = c("P1", "P1", "P1"),
    tag       = c("101", "201", "1O3"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())

  dir <- file.path(tempdir(), paste0("census_split_", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  written <- export_census_split(res, dir = dir)
  files <- basename(written)

  expect_true(any(grepl("01_recruits", files)))
  expect_true(any(grepl("02_measurements", files)))
  expect_true(any(grepl("03_review", files)))
  expect_true(all(file.exists(written)))
})

test_that("export_census_split keeps review rows out of the recruit file", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  census <- data.frame(
    plot_name = c("P1", "P1"), tag = c("201", "1O3"),
    stringsAsFactors = FALSE
  )
  res <- split_census_table(census, existing = make_existing())

  dir <- file.path(tempdir(), paste0("census_split_rev_", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  written <- export_census_split(res, dir = dir)

  recruits <- readxl::read_excel(grep("01_recruits", written, value = TRUE))
  expect_equal(nrow(recruits), 1)
  expect_equal(as.character(recruits$tag), "201")
})

test_that("export_census_split skips empty pieces", {
  skip_if_not_installed("writexl")

  census <- data.frame(plot_name = "P1", tag = "101", stringsAsFactors = FALSE)
  res <- split_census_table(census, existing = make_existing())

  dir <- file.path(tempdir(), paste0("census_split_empty_", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  written <- export_census_split(res, dir = dir)

  # No recruits and no review rows in this split
  expect_false(any(grepl("01_recruits", basename(written))))
  expect_false(any(grepl("03_review", basename(written))))
})

test_that("export_census_split does not overwrite unless asked", {
  skip_if_not_installed("writexl")

  res <- split_census_table(make_census(), existing = make_existing())
  dir <- file.path(tempdir(), paste0("census_split_ow_", as.integer(Sys.time())))
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  first <- export_census_split(res, dir = dir)
  second <- suppressMessages(export_census_split(res, dir = dir))
  expect_equal(length(second), 0)

  third <- export_census_split(res, dir = dir, overwrite = TRUE)
  expect_equal(sort(third), sort(first))
})

test_that("export_census_split rejects anything that is not a census_split", {
  expect_error(export_census_split(data.frame(a = 1)), "census_split")
})
