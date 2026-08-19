# Which rows repeat a measurement the database already holds
#
# The comparison cannot be the same for every row. A diameter taken during
# census 3 repeats only a diameter already recorded for census 3; the same
# stem measured in census 2 is a different measurement, which is what a census
# is for. A position or a quadrat carries no census at all - the census link
# policy keeps those off a campaign - so there is nothing to narrow the
# comparison to, and any recorded value of that feature for that individual is
# the same claim being made twice.
#
# Both branches are pinned here because the wrong one silently under- or
# over-reports, and the count drives a checkbox that removes rows.

individuals <- data.frame(
  stringsAsFactors = FALSE,
  tag = c(1, 2, 3),
  id_table_liste_plots_n = c(10, 10, 10),
  id_n = c(101, 102, 103)
)

measures <- function(...) {
  m <- rbind(...)
  if (is.null(m)) {
    return(data.frame(id_data_individuals = numeric(), traitid = numeric(),
                      id_sub_plots = numeric(), stringsAsFactors = FALSE))
  }
  m
}

measure <- function(id_n, traitid, census = NA) {
  data.frame(id_data_individuals = id_n, traitid = traitid,
             id_sub_plots = census, stringsAsFactors = FALSE)
}

import_rows <- function(tags, traitid = 5, census = NA) {
  data.frame(
    stringsAsFactors = FALSE,
    tag = tags,
    id_liste_plots = 10,
    traitid = traitid,
    id_sub_plots = census
  )
}


test_that("a row with a census matches only the same census", {
  db <- measures(measure(101, 5, census = 77))

  same <- .existing_measurement_rows(import_rows(1, census = 77), individuals, db)
  expect_equal(same$with_census, 1L)
  expect_equal(same$without_census, integer(0))

  other <- .existing_measurement_rows(import_rows(1, census = 88), individuals, db)
  expect_equal(other$with_census, integer(0))
  expect_equal(other$without_census, integer(0))
})

test_that("a row with a census is not matched by an unlinked measurement", {
  # A stored value with no census is not part of any campaign, so importing
  # this feature for a named census is a new measurement, not a repeat.
  db <- measures(measure(101, 5, census = NA))

  res <- .existing_measurement_rows(import_rows(1, census = 77), individuals, db)
  expect_equal(res$with_census, integer(0))
  expect_equal(res$without_census, integer(0))
})

test_that("a row with no census matches a stored value with no census", {
  # The case that matters for positions and quadrats.
  db <- measures(measure(101, 5, census = NA))

  res <- .existing_measurement_rows(import_rows(1, census = NA), individuals, db)
  expect_equal(res$without_census, 1L)
  expect_equal(res$with_census, integer(0))
})

test_that("a row with no census also matches a value stored under a census", {
  # The tree has one position. If it was recorded during a campaign, importing
  # it again without a census is still the same claim made twice.
  db <- measures(measure(101, 5, census = 77))

  res <- .existing_measurement_rows(import_rows(1, census = NA), individuals, db)
  expect_equal(res$without_census, 1L)
})

test_that("the two kinds of match are reported separately", {
  db <- measures(
    measure(101, 5, census = 77),   # tag 1, in census 77
    measure(102, 5, census = NA)    # tag 2, no census
  )
  data <- rbind(
    import_rows(1, census = 77),    # row 1: matches within the census
    import_rows(2, census = NA),    # row 2: matches on the individual
    import_rows(3, census = 77)     # row 3: nothing recorded
  )

  res <- .existing_measurement_rows(data, individuals, db)
  expect_equal(res$with_census, 1L)
  expect_equal(res$without_census, 2L)
})

test_that("a different feature is never a match", {
  db <- measures(measure(101, 5, census = NA))

  res <- .existing_measurement_rows(
    import_rows(1, traitid = 6, census = NA), individuals, db)
  expect_equal(res$without_census, integer(0))
})

test_that("an individual the plot does not have is never a match", {
  db <- measures(measure(101, 5, census = NA))

  res <- .existing_measurement_rows(import_rows(99, census = NA), individuals, db)
  expect_equal(res$without_census, integer(0))
})

test_that("a repeated tag matches when any of its individuals has the value", {
  # Duplicate tags within a plot are reported elsewhere, but they must not
  # make this check miss a genuine repeat.
  inds <- rbind(individuals,
                data.frame(tag = 1, id_table_liste_plots_n = 10, id_n = 104,
                           stringsAsFactors = FALSE))
  db <- measures(measure(104, 5, census = NA))

  res <- .existing_measurement_rows(import_rows(1, census = NA), inds, db)
  expect_equal(res$without_census, 1L)
})

test_that("tags compare by value, not by storage type", {
  # Tags arrive as numeric from the database and often as character from a
  # spreadsheet.
  inds <- data.frame(tag = "0012", id_table_liste_plots_n = "10", id_n = "101",
                     stringsAsFactors = FALSE)
  db <- measures(measure(101, 5, census = NA))

  res <- .existing_measurement_rows(
    data.frame(tag = "0012", id_liste_plots = 10, traitid = 5,
               id_sub_plots = NA, stringsAsFactors = FALSE),
    inds, db)
  expect_equal(res$without_census, 1L)
})

test_that("rows with no tag, plot or feature are skipped, not matched", {
  db <- measures(measure(101, 5, census = NA))
  data <- data.frame(
    stringsAsFactors = FALSE,
    tag = c(NA, 1, 1),
    id_liste_plots = c(10, NA, 10),
    traitid = c(5, 5, NA),
    id_sub_plots = NA
  )

  res <- .existing_measurement_rows(data, individuals, db)
  expect_equal(res$with_census, integer(0))
  expect_equal(res$without_census, integer(0))
})

test_that("row numbers survive the skipped rows", {
  # The result indexes the caller's data, which is what the removal uses.
  db <- measures(measure(103, 5, census = NA))
  data <- data.frame(
    stringsAsFactors = FALSE,
    tag = c(NA, 2, 3),
    id_liste_plots = 10,
    traitid = 5,
    id_sub_plots = NA
  )

  res <- .existing_measurement_rows(data, individuals, db)
  expect_equal(res$without_census, 3L)
})

test_that("empty inputs are answered, not errored", {
  none <- measures(measure(101, 5, census = NA))[0, , drop = FALSE]

  for (res in list(
    .existing_measurement_rows(import_rows(1), individuals, none),
    .existing_measurement_rows(import_rows(1)[0, , drop = FALSE], individuals,
                               measures(measure(101, 5))),
    .existing_measurement_rows(import_rows(1), individuals[0, , drop = FALSE],
                               measures(measure(101, 5))),
    .existing_measurement_rows(NULL, individuals, measures(measure(101, 5)))
  )) {
    expect_equal(res$with_census, integer(0))
    expect_equal(res$without_census, integer(0))
  }
})

test_that("data without an id_sub_plots column is treated as carrying no census", {
  db <- measures(measure(101, 5, census = 77))
  data <- data.frame(tag = 1, id_liste_plots = 10, traitid = 5,
                     stringsAsFactors = FALSE)

  res <- .existing_measurement_rows(data, individuals, db)
  expect_equal(res$without_census, 1L)
})
