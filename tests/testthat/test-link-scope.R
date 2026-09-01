# Tests for the scope of specimen link types
#
# A link type declares, through linktypelist.scope, which end of the link it
# points at: an 'individual' type fills id_n, a 'plot' type fills
# id_liste_plots. These tests cover the reconstruction of that column on a
# database where the migration has not run, and the coherence check applied
# before links are written.

fake_linktypes <- function(with_scope = TRUE) {
  out <- dplyr::tibble(
    id_linktype = 1:3,
    linktype = c("type_individual", "referenced_individual", "reference_plot"),
    priority = c(100L, 50L, 10L)
  )
  if (with_scope) {
    out$scope <- c("individual", "individual", "plot")
  }
  out
}

# =============================================================================
# .ensure_linktype_scope()
# =============================================================================

test_that(".ensure_linktype_scope() leaves an existing scope column untouched", {
  input <- fake_linktypes(with_scope = TRUE)
  input$scope <- c("plot", "plot", "plot")  # deliberately not the default guess

  result <- .ensure_linktype_scope(input)

  expect_identical(result$scope, c("plot", "plot", "plot"))
})

test_that(".ensure_linktype_scope() reconstructs scope from the link type names", {
  result <- .ensure_linktype_scope(fake_linktypes(with_scope = FALSE))

  expect_true("scope" %in% names(result))
  expect_identical(
    result$scope,
    c("individual", "individual", "plot")
  )
})

test_that(".ensure_linktype_scope() treats an unknown link type as individual", {
  input <- dplyr::tibble(
    id_linktype = 9L, linktype = "some_future_type", priority = 0L
  )

  result <- .ensure_linktype_scope(input)

  expect_identical(result$scope, "individual")
})

test_that(".ensure_linktype_scope() handles a zero-row table", {
  input <- dplyr::tibble(
    id_linktype = integer(0), linktype = character(0), priority = integer(0)
  )

  result <- .ensure_linktype_scope(input)

  expect_true("scope" %in% names(result))
  expect_equal(nrow(result), 0)
})

# =============================================================================
# .check_link_scope()
# =============================================================================

test_that(".check_link_scope() accepts links that match their type's scope", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) fake_linktypes()
  )

  links <- dplyr::tibble(
    id_specimen    = c(1L, 2L, 3L),
    id_n           = c(10L, 11L, NA_integer_),
    id_liste_plots = c(NA_integer_, NA_integer_, 500L),
    id_linktype    = c(1L, 2L, 3L)
  )

  expect_length(.check_link_scope(links, con = NULL), 0)
})

test_that(".check_link_scope() flags an individual-level link with no id_n", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) fake_linktypes()
  )

  links <- dplyr::tibble(
    id_specimen    = 1L,
    id_n           = NA_integer_,
    id_liste_plots = 500L,
    id_linktype    = 1L
  )

  errors <- .check_link_scope(links, con = NULL)

  expect_length(errors, 1)
  expect_match(errors, "type_individual")
  expect_match(errors, "no id_n")
})

test_that(".check_link_scope() flags a plot-level link with no id_liste_plots", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) fake_linktypes()
  )

  links <- dplyr::tibble(
    id_specimen    = 1L,
    id_n           = NA_integer_,
    id_liste_plots = NA_integer_,
    id_linktype    = 3L
  )

  errors <- .check_link_scope(links, con = NULL)

  expect_length(errors, 1)
  expect_match(errors, "reference_plot")
  expect_match(errors, "no id_liste_plots")
})

test_that(".check_link_scope() flags a plot-level link that also carries an individual", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) fake_linktypes()
  )

  links <- dplyr::tibble(
    id_specimen    = 1L,
    id_n           = 10L,
    id_liste_plots = 500L,
    id_linktype    = 3L
  )

  errors <- .check_link_scope(links, con = NULL)

  expect_length(errors, 1)
  expect_match(errors, "also carry an id_n")
})

test_that(".check_link_scope() ignores links whose type is unknown", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) fake_linktypes()
  )

  links <- dplyr::tibble(
    id_specimen    = 1L,
    id_n           = NA_integer_,
    id_liste_plots = NA_integer_,
    id_linktype    = NA_integer_
  )

  expect_length(.check_link_scope(links, con = NULL), 0)
})

test_that(".check_link_scope() returns nothing when linktypelist is unavailable", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) stop("no such table: linktypelist")
  )

  links <- dplyr::tibble(
    id_specimen    = 1L,
    id_n           = NA_integer_,
    id_liste_plots = NA_integer_,
    id_linktype    = 1L
  )

  expect_length(.check_link_scope(links, con = NULL), 0)
})

test_that(".check_link_scope() handles zero links and a missing id_linktype column", {
  empty <- dplyr::tibble(
    id_specimen = integer(0), id_n = integer(0),
    id_liste_plots = integer(0), id_linktype = integer(0)
  )
  expect_length(.check_link_scope(empty, con = NULL), 0)

  no_type <- dplyr::tibble(id_specimen = 1L, id_n = 10L, id_liste_plots = NA_integer_)
  expect_length(.check_link_scope(no_type, con = NULL), 0)
})

# =============================================================================
# Duplicate key for links
#
# A plot-level link has a NULL id_n, and dplyr matches NA to NA by default.
# The plot must therefore be part of the duplicate key, or two links to
# different plots look like the same link.
# =============================================================================

test_that("the duplicate key keeps links to different plots apart", {
  dup_key <- c("id_n", "id_specimen", "id_linktype", "id_liste_plots")

  new_links <- dplyr::tibble(
    id_n           = c(NA_integer_, NA_integer_),
    id_specimen    = c(1L, 1L),
    id_linktype    = c(3L, 3L),
    id_liste_plots = c(500L, 600L)
  )
  existing <- dplyr::tibble(
    id_n = NA_integer_, id_specimen = 1L, id_linktype = 3L, id_liste_plots = 500L
  )

  remaining <- dplyr::anti_join(new_links, existing, by = dup_key)

  expect_equal(nrow(remaining), 1)
  expect_equal(remaining$id_liste_plots, 600L)

  # ... while the key without the plot would have dropped both
  narrow_key <- c("id_n", "id_specimen", "id_linktype")
  expect_equal(nrow(dplyr::anti_join(new_links, existing, by = narrow_key)), 0)
})

# =============================================================================
# Validation no longer treats a missing individual as a missing ID
# =============================================================================

test_that("dropping NA before setdiff() stops NA being reported as a missing ID", {
  id_n <- c(10L, NA_integer_)
  existing <- 10L

  # the old behaviour: NA survives setdiff() and is reported as missing
  expect_true(any(is.na(setdiff(unique(id_n), existing))))

  # the new behaviour: nothing is missing
  expect_length(setdiff(unique(id_n[!is.na(id_n)]), existing), 0)
})

# =============================================================================
# Mixed batches: one specimen carrying individual links and a plot link
#
# A specimen can be linked to several individuals and, separately, to a plot.
# The three link types then coexist for that specimen. What follows checks that
# a single batch carrying all of them validates, deduplicates and orders the way
# it should.
# =============================================================================

mixed_batch <- function() {
  dplyr::tibble(
    id_specimen    = c(42L, 42L, 42L),
    id_n           = c(100L, 200L, NA_integer_),
    id_liste_plots = c(NA_integer_, NA_integer_, 1115L),
    id_linktype    = c(1L, 2L, 3L)
  )
}

test_that("a batch mixing individual and plot links passes the scope check", {
  testthat::local_mocked_bindings(
    get_linktypes = function(...) fake_linktypes()
  )

  expect_length(.check_link_scope(mixed_batch(), con = NULL), 0)
})

test_that("a mixed batch splits cleanly into individual and plot IDs to validate", {
  batch <- mixed_batch()

  # what .add_link_specimens() collects for its batch FK queries
  ind_ids  <- unique(batch$id_n[!is.na(batch$id_n)])
  plot_ids <- unique(batch$id_liste_plots[!is.na(batch$id_liste_plots)])

  expect_identical(ind_ids, c(100L, 200L))
  expect_identical(plot_ids, 1115L)

  # no link is left attached to nothing
  orphans <- batch %>% dplyr::filter(is.na(.data$id_n) & is.na(.data$id_liste_plots))
  expect_equal(nrow(orphans), 0)
})

test_that("the three links of one specimen are distinct under the duplicate key", {
  dup_key <- c("id_n", "id_specimen", "id_linktype", "id_liste_plots")
  batch <- mixed_batch()

  expect_equal(nrow(dplyr::distinct(batch, dplyr::pick(dplyr::all_of(dup_key)))), 3)

  # re-adding the batch when all three already exist leaves nothing to write
  expect_equal(nrow(dplyr::anti_join(batch, batch, by = dup_key)), 0)

  # the plot link alone is new when only the two individual links exist
  existing <- batch[1:2, ]
  remaining <- dplyr::anti_join(batch, existing, by = dup_key)
  expect_equal(nrow(remaining), 1)
  expect_equal(remaining$id_liste_plots, 1115L)
})

test_that("priority orders links within one individual, never across the plot link", {
  # An individual's candidate set is the links carrying its id_n. The plot link
  # has none, so it is absent before priority is consulted - not outranked by it.
  batch <- mixed_batch() %>%
    dplyr::left_join(fake_linktypes(), by = "id_linktype")

  candidates_100 <- batch %>% dplyr::filter(.data$id_n %in% 100L)
  expect_equal(nrow(candidates_100), 1)
  expect_identical(candidates_100$linktype, "type_individual")

  # the plot link belongs to no individual's candidate set
  expect_equal(nrow(batch %>% dplyr::filter(!is.na(.data$id_n) & .data$scope == "plot")), 0)

  # sorting the specimen's own links by priority puts the plot link last
  by_priority <- batch %>% dplyr::arrange(dplyr::desc(.data$priority))
  expect_identical(
    by_priority$linktype,
    c("type_individual", "referenced_individual", "reference_plot")
  )
})

test_that("two individual links of equal priority both stay in the candidate set", {
  # priority only picks a winner; it never drops a link from the table
  batch <- dplyr::tibble(
    id_specimen    = c(42L, 43L),
    id_n           = c(100L, 100L),
    id_liste_plots = c(NA_integer_, NA_integer_),
    id_linktype    = c(2L, 2L)
  ) %>%
    dplyr::left_join(fake_linktypes(), by = "id_linktype")

  expect_equal(nrow(batch %>% dplyr::filter(.data$id_n == 100L)), 2)
  expect_true(all(batch$priority == 50L))
})
