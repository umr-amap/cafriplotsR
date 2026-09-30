# query_plot_authors(): the co-author list of a set of plots, gathered from the
# two places people are recorded - plot-level features and the people attached
# to a census (or any other subplot observation).

# ── Fixtures ─────────────────────────────────────────────────────────────────

people_types_raw <- function() {
  data.frame(
    id_subplotype = c(247L, 248L, 249L, 250L),
    type = c("principal_investigator", "data_manager",
             "additional_people", "team_leader"),
    stringsAsFactors = FALSE
  )
}

plots_raw <- function() {
  data.frame(
    id_liste_plots = c(10L, 11L, 12L),
    plot_name      = c("p010", "p011", "p012"),
    stringsAsFactors = FALSE
  )
}

# Plot-level people. The last row repeats the first: the same person recorded
# twice on the same plot in the same role must collapse to one.
plot_people_raw <- function() {
  data.frame(
    id_liste_plots  = c(10L, 10L, 11L, 10L),
    id_table_colnam = c(1L, 4L, 1L, 1L),
    role = c("principal_investigator", "additional_people",
             "principal_investigator", "principal_investigator"),
    stringsAsFactors = FALSE
  )
}

# People attached to a subplot observation. Row 4 hangs off a soil sample, not
# a census, which is what `subplot_type` is there to exclude.
subplot_people_raw <- function() {
  data.frame(
    id_liste_plots  = c(10L, 10L, 11L, 11L),
    id_table_colnam = c(2L, 3L, 2L, 5L),
    role   = c("team_leader", "data_manager", "team_leader", "team_leader"),
    source = c("census", "census", "census", "soil_sample"),
    census_number = c(1L, 2L, 1L, 7L),
    census_year   = c(2010L, 2015L, 2012L, 1999L),
    stringsAsFactors = FALSE
  )
}

colnam_raw <- function() {
  data.frame(
    id_table_colnam = 1:5,
    colnam      = c("A One", "B Two", "C Three", "D Four", "E Five"),
    surname     = c("A", "B", "C", "D", "E"),
    family_name = c("One", "Two", "Three", "Four", "Five"),
    contact     = c("a@x.org", NA, "c@x.org", "d@x.org", "e@x.org"),
    institute   = c("IRD", "ENS", "IRD", "ENS", "IRD"),
    nationality = c("FR", "CM", "BE", "GA", "FR"),
    stringsAsFactors = FALSE
  )
}

mock_authors_con <- function(people_types   = people_types_raw(),
                             plot_people    = plot_people_raw(),
                             subplot_people = subplot_people_raw(),
                             colnam         = colnam_raw(),
                             plots          = plots_raw(),
                             env = parent.frame()) {
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(conn, statement, ...) {
      # Order matters: the two people queries also name subplotype_list and
      # the string 'table_colnam', so they have to be matched first.
      if (grepl("FROM data_subplot_feat", statement))    return(subplot_people)
      if (grepl("FROM data_liste_sub_plots", statement)) return(plot_people)
      if (grepl("FROM subplotype_list", statement))      return(people_types)
      if (grepl("FROM table_colnam", statement))         return(colnam)
      if (grepl("FROM data_liste_plots", statement))     return(plots)
      stop("unexpected query: ", statement)
    },
    .env = env
  )
  structure(list(), class = "mock_connection")
}

authors <- function(...) {
  suppressMessages(query_plot_authors(..., verbose = FALSE))
}


# ── Resolving which plots to work on ─────────────────────────────────────────

test_that("plot ids are taken from a query_plots() result", {
  expect_equal(
    CafriplotsR:::.resolve_author_plot_ids(
      NULL, list(extract = data.frame(id_liste_plots = c(7, 8)))
    ),
    c(7L, 8L)
  )
})

test_that("a bare data frame of plots is accepted, under either id name", {
  expect_equal(
    CafriplotsR:::.resolve_author_plot_ids(NULL, data.frame(id_liste_plots = 7)),
    7L
  )
  expect_equal(
    CafriplotsR:::.resolve_author_plot_ids(
      NULL, data.frame(id_table_liste_plots = c(2, 2, 5))
    ),
    c(2L, 5L)
  )
})

test_that("plots stripped of their ids are refused with the remedy named", {
  expect_error(
    CafriplotsR:::.resolve_author_plot_ids(NULL, data.frame(plot_name = "p010")),
    "remove_ids"
  )
})

test_that("naming no plots at all is an error", {
  expect_error(CafriplotsR:::.resolve_author_plot_ids(NULL, NULL), "required")
  expect_error(CafriplotsR:::.resolve_author_plot_ids(integer(0), NULL), "no plot")
})


# ── Collecting from both routes ──────────────────────────────────────────────

test_that("people are collected from plot features and from censuses alike", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_setequal(out$by_plot$source, c("plot", "census", "soil_sample"))
  # Five distinct people across the two routes.
  expect_equal(nrow(out$authors_all), 5L)
  expect_setequal(
    out$authors_all$colnam,
    c("A One", "B Two", "C Three", "D Four", "E Five")
  )
})

test_that("the same person on the same plot in the same role gives one row", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  pi_rows <- out$by_plot[out$by_plot$colnam == "A One" &
                           out$by_plot$id_liste_plots == 10L, ]
  expect_equal(nrow(pi_rows), 1L)
})

test_that("a person's roles, sources and plots are gathered onto one row", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)
  b <- out$authors_all[out$authors_all$colnam == "B Two", ]

  expect_equal(b$n_plots, 2L)
  expect_equal(b$roles, "team_leader")
  expect_equal(b$sources, "census")
  expect_equal(b$plot_names, "p010, p011")
})

test_that("the author table is ordered by how many plots each person carries", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_true(!is.unsorted(rev(out$authors_all$n_plots)))
})


# ── core vs complete list ────────────────────────────────────────────────────

test_that("the core list drops additional_people and keeps the three roles", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  # D Four is only ever additional_people
  expect_true("D Four" %in% out$authors_all$colnam)
  expect_false("D Four" %in% out$authors_core$colnam)
  expect_setequal(
    out$authors_core$colnam,
    c("A One", "B Two", "C Three", "E Five")
  )
})

test_that("core_roles is honoured when it is narrowed", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con,
                 core_roles = "principal_investigator")

  expect_equal(out$authors_core$colnam, "A One")
})

test_that("a core_roles value that is not a feature type is reported", {
  con <- mock_authors_con()

  expect_message(
    query_plot_authors(plot_ids = 10L, con = con, verbose = FALSE,
                       core_roles = c("principal_investigator", "nonesuch")),
    "nonesuch"
  )
})

test_that("both author tables share one set of columns", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_identical(names(out$authors_core), names(out$authors_all))
  expect_true(all(c("id_table_colnam", "colnam", "contact", "institute",
                    "roles", "n_plots", "has_contact") %in% names(out$authors_all)))
})


# ── Choosing which route to follow ───────────────────────────────────────────

test_that("the plot-level route can be switched off", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con,
                 include_plot_features = FALSE)

  expect_false("plot" %in% out$by_plot$source)
  expect_false("D Four" %in% out$authors_all$colnam)
})

test_that("the census route can be switched off", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con,
                 include_subplot_features = FALSE)

  expect_equal(unique(out$by_plot$source), "plot")
  expect_setequal(out$authors_all$colnam, c("A One", "D Four"))
})

test_that("switching off both routes is an error, not an empty answer", {
  con <- mock_authors_con()

  expect_error(
    query_plot_authors(plot_ids = 10L, con = con,
                       include_plot_features = FALSE,
                       include_subplot_features = FALSE),
    "both"
  )
})

test_that("subplot_type keeps only people hanging off that kind of subplot", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con, subplot_type = "census")

  expect_false("soil_sample" %in% out$by_plot$source)
  # E Five was only ever on a soil sample
  expect_false("E Five" %in% out$authors_all$colnam)
})

test_that("census number and year are carried, and only for censuses", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  census <- out$by_plot[out$by_plot$source == "census", ]
  expect_true(all(!is.na(census$census_number)))
  expect_setequal(census$census_year, c(2010L, 2015L, 2012L))

  # A soil sample's typevalue is not a census number, so it is not reported
  soil <- out$by_plot[out$by_plot$source == "soil_sample", ]
  expect_true(all(is.na(soil$census_number)))
  expect_true(all(is.na(soil$census_year)))

  plot_level <- out$by_plot[out$by_plot$source == "plot", ]
  expect_true(all(is.na(plot_level$census_number)))
})


# ── Contacts ─────────────────────────────────────────────────────────────────

test_that("people with no contact are kept but flagged", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)
  b <- out$authors_all[out$authors_all$colnam == "B Two", ]

  expect_false(b$has_contact)
  expect_true(all(out$authors_all$has_contact[out$authors_all$colnam != "B Two"]))
})

test_that("require_contact drops the people who cannot be invited", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con, require_contact = TRUE)

  expect_false("B Two" %in% out$authors_all$colnam)
  expect_true(all(out$authors_all$has_contact))
})

test_that("a blank contact counts as no contact", {
  con <- mock_authors_con(
    colnam = transform(colnam_raw(), contact = c("a@x.org", "   ", "", NA, "e@x.org"))
  )

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_setequal(
    out$authors_all$colnam[!out$authors_all$has_contact],
    c("B Two", "C Three", "D Four")
  )
})


# ── Gaps in the data ─────────────────────────────────────────────────────────

test_that("a queried plot with nobody recorded is named, not silently missing", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_equal(out$plots_without_people$plot_name, "p012")
})

test_that("a person id absent from table_colnam is dropped with a warning", {
  broken <- rbind(
    plot_people_raw(),
    data.frame(id_liste_plots = 11L, id_table_colnam = 99L,
               role = "data_manager", stringsAsFactors = FALSE)
  )
  con <- mock_authors_con(plot_people = broken)

  expect_message(
    out <- query_plot_authors(plot_ids = c(10L, 11L, 12L), con = con,
                              verbose = FALSE),
    "99"
  )
  expect_false(99L %in% out$authors_all$id_table_colnam)
  expect_false(any(is.na(out$authors_all$colnam)))
})

test_that("no people at all returns the full shape rather than nothing", {
  empty_plot <- plot_people_raw()[0, ]
  empty_sub  <- subplot_people_raw()[0, ]
  con <- mock_authors_con(plot_people = empty_plot, subplot_people = empty_sub)

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_equal(nrow(out$authors_all), 0L)
  expect_equal(nrow(out$authors_core), 0L)
  expect_equal(nrow(out$by_plot), 0L)
  expect_equal(nrow(out$plots_without_people), 3L)
  expect_true(all(c("roles", "n_plots", "has_contact") %in% names(out$authors_all)))
})

test_that("a database with no table_colnam feature type is an error", {
  con <- mock_authors_con(people_types = people_types_raw()[0, ])

  expect_error(
    query_plot_authors(plot_ids = 10L, con = con, verbose = FALSE),
    "table_colnam"
  )
})


# ── The detail table ─────────────────────────────────────────────────────────

test_that("by_plot names who, on which plot, in which role and from where", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  expect_true(all(c("colnam", "role", "source", "plot_name", "id_liste_plots",
                    "census_number", "census_year") %in% names(out$by_plot)))

  a_on_10 <- out$by_plot[out$by_plot$colnam == "A One" &
                           out$by_plot$plot_name == "p010", ]
  expect_equal(a_on_10$role, "principal_investigator")
  expect_equal(a_on_10$source, "plot")
})

test_that("roles_found counts people and plots per role and route", {
  con <- mock_authors_con()

  out <- authors(plot_ids = c(10L, 11L, 12L), con = con)

  tl <- out$roles_found[out$roles_found$role == "team_leader" &
                          out$roles_found$source == "census", ]
  expect_equal(tl$n_people, 1L)
  expect_equal(tl$n_plots, 2L)
})
