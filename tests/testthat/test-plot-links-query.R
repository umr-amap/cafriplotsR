# query_plots(extract_plot_links = TRUE): the parent a plot sits in and the
# plots sitting in it, plus the warning that fires whether or not links were
# asked for, when a result holds both ends of a link.

# One edge: plot 7 (P1_regen) sits inside plot 1 (P1) as a nested_subsample.
edges_raw <- function() {
  data.frame(
    plot_id          = c(1L, 7L),
    plot_name        = c("P1", "P1_regen"),
    role             = c("child", "parent"),
    linked_plot_id   = c(7L, 1L),
    linked_plot_name = c("P1_regen", "P1"),
    parent_relation  = c("nested_subsample", "nested_subsample"),
    stringsAsFactors = FALSE
  )
}

mock_links_con <- function(edges = NULL, hierarchy = TRUE, env = parent.frame()) {
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbListFields = function(conn, name, ...) {
      if (hierarchy) {
        c("id_liste_plots", "plot_name", "id_parent_plot", "parent_relation")
      } else {
        c("id_liste_plots", "plot_name")
      }
    },
    dbGetQuery = function(con, sql, ...) {
      if (is.null(edges)) return(data.frame())
      edges
    },
    .env = env
  )
  structure(list(), class = "mock_connection")
}


# ── Reading the edges ────────────────────────────────────────────────────────

test_that("no links are fetched on an unmigrated database", {
  con <- mock_links_con(edges_raw(), hierarchy = FALSE)

  out <- CafriplotsR:::.plot_link_edges(c(1L, 7L), con)

  expect_equal(nrow(out), 0L)
  # The empty frame still has the full shape, so callers need no special case.
  expect_true(all(c("plot_id", "role", "linked_plot_id", "linked_in_query")
                  %in% names(out)))
})

test_that("no links are fetched for an empty plot set", {
  con <- mock_links_con(edges_raw())

  expect_equal(nrow(CafriplotsR:::.plot_link_edges(integer(0), con)), 0L)
})

test_that("both directions of a link are reported", {
  con <- mock_links_con(edges_raw())

  out <- CafriplotsR:::.plot_link_edges(c(1L, 7L), con)

  expect_equal(nrow(out), 2L)
  expect_setequal(out$role, c("parent", "child"))
  # Both ends were queried, so both rows say so.
  expect_true(all(out$linked_in_query))
})

test_that("a link to a plot outside the query is flagged as such", {
  # Only the parent was queried; its child was not.
  con <- mock_links_con(edges_raw()[1, , drop = FALSE])

  out <- CafriplotsR:::.plot_link_edges(1L, con)

  expect_equal(out$role, "child")
  expect_false(out$linked_in_query)
  expect_equal(out$linked_plot_name, "P1_regen")
})


# ── The metadata columns ─────────────────────────────────────────────────────

test_that("a plot gains its parent's name and its child count", {
  plots <- dplyr::tibble(
    id_liste_plots = c(1L, 7L),
    plot_name      = c("P1", "P1_regen")
  )
  edges <- CafriplotsR:::.plot_link_edges(
    c(1L, 7L), mock_links_con(edges_raw())
  )

  out <- CafriplotsR:::.enrich_plot_links(plots, edges)

  expect_equal(out$parent_plot_name, c(NA_character_, "P1"))
  expect_equal(out$parent_relation, c(NA_character_, "nested_subsample"))
  expect_equal(out$n_child_plots, c(1L, 0L))
})

test_that("a plot with no links gets zero children, not NA", {
  # 0 and NA say different things, and a count that reads NA cannot be summed
  # or filtered on without a special case.
  plots <- dplyr::tibble(id_liste_plots = 3L, plot_name = "P3")

  out <- CafriplotsR:::.enrich_plot_links(
    plots, CafriplotsR:::.plot_link_edges(3L, mock_links_con(NULL))
  )

  expect_equal(out$n_child_plots, 0L)
  expect_true(is.na(out$parent_plot_name))
})

test_that("a stale parent_relation column is replaced, not duplicated", {
  # A query that carried its own parent_relation must not end up with
  # parent_relation.x / parent_relation.y disagreeing with parent_plot_name.
  plots <- dplyr::tibble(
    id_liste_plots  = 7L,
    plot_name       = "P1_regen",
    parent_relation = "block_member"
  )
  edges <- CafriplotsR:::.plot_link_edges(7L, mock_links_con(edges_raw()[2, , drop = FALSE]))

  out <- CafriplotsR:::.enrich_plot_links(plots, edges)

  expect_equal(sum(names(out) == "parent_relation"), 1L)
  expect_equal(out$parent_relation, "nested_subsample")
})


# ── The double-counting warning ──────────────────────────────────────────────

test_that("holding both ends of a link warns about counting ground twice", {
  edges <- CafriplotsR:::.plot_link_edges(c(1L, 7L), mock_links_con(edges_raw()))

  expect_warning(
    expect_message(CafriplotsR:::.warn_overlapping_plot_links(edges),
                   "inside another returned plot"),
    regexp = NA
  )
})

test_that("the warning names the pair once, not once per direction", {
  edges <- CafriplotsR:::.plot_link_edges(c(1L, 7L), mock_links_con(edges_raw()))

  reported <- CafriplotsR:::.warn_overlapping_plot_links(edges)

  expect_equal(nrow(reported), 1L)
  expect_equal(reported$linked_plot_name, "P1_regen")
})

test_that("a link to a plot outside the result is not a double count", {
  # The child was not returned, so nothing in the result overlaps.
  edges <- CafriplotsR:::.plot_link_edges(1L, mock_links_con(edges_raw()[1, , drop = FALSE]))

  expect_null(CafriplotsR:::.warn_overlapping_plot_links(edges))
})

test_that("no links means nothing is said", {
  edges <- CafriplotsR:::.plot_link_edges(3L, mock_links_con(NULL))

  expect_null(CafriplotsR:::.warn_overlapping_plot_links(edges))
})


# ── Surviving the output styles ──────────────────────────────────────────────

test_that("the link columns survive a style whose allow-list omits them", {
  # Every built-in style but "full" lists its metadata columns explicitly, so
  # a column added after the fact is dropped unless it is named.
  meta <- dplyr::tibble(
    id_liste_plots   = c(1L, 7L),
    plot_name        = c("P1", "P1_regen"),
    country          = "Gabon",
    ddlat            = 0.5,
    ddlon            = 11.5,
    parent_plot_name = c(NA, "P1"),
    parent_relation  = c(NA, "nested_subsample"),
    n_child_plots    = c(1L, 0L)
  )
  config <- CafriplotsR:::.plot_output_styles$standard

  without <- CafriplotsR:::.extract_metadata_table(
    data = meta, meta_data = meta, style_config = config,
    extract_individuals = FALSE
  )
  with_extras <- CafriplotsR:::.extract_metadata_table(
    data = meta, meta_data = meta, style_config = config,
    extract_individuals = FALSE,
    extra_columns = c("parent_plot_name", "parent_relation", "n_child_plots")
  )

  expect_false("parent_plot_name" %in% names(without))
  expect_true(all(c("parent_plot_name", "parent_relation", "n_child_plots")
                  %in% names(with_extras)))
})

test_that("plot_links is passed through the style layer untouched", {
  links <- CafriplotsR:::.plot_link_edges(c(1L, 7L), mock_links_con(edges_raw()))
  data <- list(
    extract    = dplyr::tibble(id_liste_plots = c(1L, 7L),
                               plot_name = c("P1", "P1_regen")),
    meta_data  = dplyr::tibble(id_liste_plots = c(1L, 7L),
                               plot_name = c("P1", "P1_regen")),
    plot_links = links
  )

  out <- CafriplotsR:::.apply_output_style(
    data = data, style = "standard", extract_individuals = FALSE
  )

  expect_true("plot_links" %in% names(out))
  expect_equal(nrow(out$plot_links), 2L)
})


# ── The argument reaches the implementation ──────────────────────────────────

test_that("query_plots() accepts extract_plot_links and passes it down", {
  expect_true("extract_plot_links" %in% names(formals(query_plots)))
  expect_true("extract_plot_links" %in% names(formals(CafriplotsR:::.query_plots_impl)))
  expect_false(formals(query_plots)$extract_plot_links)
})
