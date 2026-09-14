# Taxa traits are linked to the source they come from: the import wizard has a
# citation step where an existing entry of table_citations is picked, or a new
# one created — written to the database there and then, so it exists before the
# import runs. These tests pin the module and the id that reaches the import.

.cit_i18n <- function() {
  shiny::reactive(list(t = function(x) x))
}

.cit_rows <- function() {
  data.frame(
    id_citation = c(1L, 2L),
    citation_key = c("Dauby2022", "TRY_2020"),
    authors = c("Dauby G., Someone E.", NA),
    year = c(2022L, 2020L),
    title = c("A trait dataset", "TRY enhanced coverage"),
    journal = c("Sci Data", NA), doi = c(NA, NA), url = c(NA, NA),
    dataset_name = c("CoForTraits", "TRY"),
    stringsAsFactors = FALSE
  )
}

.cit_pool <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  # Shaped like the real table: an auto-numbered id and the date_modif_*
  # columns add_citation() writes.
  DBI::dbExecute(con, "CREATE TABLE table_citations (
      id_citation INTEGER PRIMARY KEY AUTOINCREMENT,
      citation_key TEXT, authors TEXT, year INTEGER, title TEXT,
      journal TEXT, doi TEXT, url TEXT, dataset_name TEXT,
      date_modif_d INTEGER, date_modif_m INTEGER, date_modif_y INTEGER)")
  DBI::dbAppendTable(con, "table_citations", .cit_rows())
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}


test_that(".citation_labels() shows key, first author, year and dataset", {
  labels <- .citation_labels(.cit_rows())

  expect_equal(labels[1], "Dauby2022 — Dauby G. et al. (2022) [CoForTraits]")
  # No authors recorded: the label still reads
  expect_equal(labels[2], "TRY_2020 —  (2020) [TRY]")
})

test_that("the citation step offers every citation and selects none by default", {
  con <- .cit_pool()

  shiny::testServer(
    mod_trait_citation_server,
    args = list(pool = shiny::reactive(con), i18n = .cit_i18n()),
    {
      choices <- citation_choices()
      expect_equal(unname(choices[1]), "")            # -- None --
      expect_setequal(unname(choices[-1]), c("1", "2"))

      res <- session$returned()
      expect_true(is.na(res$id_citation))
      expect_null(res$citation)
    }
  )
})

test_that("the citation step returns the selected id and its row", {
  con <- .cit_pool()

  shiny::testServer(
    mod_trait_citation_server,
    args = list(pool = shiny::reactive(con), i18n = .cit_i18n()),
    {
      session$setInputs(selected_citation = "2")

      res <- session$returned()
      expect_equal(res$id_citation, 2L)
      expect_equal(res$citation$citation_key, "TRY_2020")
    }
  )
})

test_that("a new citation is written to the database and then selected", {
  con <- .cit_pool()

  shiny::testServer(
    mod_trait_citation_server,
    args = list(pool = shiny::reactive(con), i18n = .cit_i18n()),
    {
      session$setInputs(
        btn_add_citation = 1,
        new_cit_key = "Nieto2026", new_cit_title = "New trait compilation",
        new_cit_authors = "Nieto A.", new_cit_year = 2026,
        new_cit_journal = "", new_cit_doi = "", new_cit_url = "",
        new_cit_dataset = "CAFRI",
        confirm_add_citation = 1
      )

      stored <- DBI::dbGetQuery(con, "SELECT * FROM table_citations WHERE citation_key = 'Nieto2026'")
      expect_equal(nrow(stored), 1L)
      expect_equal(stored$title, "New trait compilation")

      # The list now offers it, and it is the current choice
      expect_true("Nieto2026" %in% citations_df()$citation_key)
      session$setInputs(selected_citation = as.character(stored$id_citation))
      expect_equal(session$returned()$citation$citation_key, "Nieto2026")
    }
  )
})

test_that("a duplicate citation key is refused rather than silently skipped", {
  con <- .cit_pool()

  shiny::testServer(
    mod_trait_citation_server,
    args = list(pool = shiny::reactive(con), i18n = .cit_i18n()),
    {
      session$setInputs(
        btn_add_citation = 1,
        new_cit_key = "TRY_2020", new_cit_title = "Another title",
        new_cit_year = 2026, confirm_add_citation = 1
      )

      # Nothing added, the existing row is untouched
      rows <- DBI::dbGetQuery(con, "SELECT * FROM table_citations WHERE citation_key = 'TRY_2020'")
      expect_equal(nrow(rows), 1L)
      expect_equal(rows$title, "TRY enhanced coverage")
    }
  )
})

test_that("a citation without key or title is not written", {
  con <- .cit_pool()

  shiny::testServer(
    mod_trait_citation_server,
    args = list(pool = shiny::reactive(con), i18n = .cit_i18n()),
    {
      session$setInputs(btn_add_citation = 1, new_cit_key = "NoTitle",
                        new_cit_title = "  ", new_cit_year = 2026,
                        confirm_add_citation = 1)

      expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM table_citations")), 2L)
    }
  )
})


test_that("the chosen citation is written onto every imported measurement", {
  captured <- NULL
  local_mocked_bindings(add_sp_traits_measures = function(...) {
    captured <<- list(...)
    list(list_traits_add = list(data.frame()))
  })

  df <- data.frame(idtax_n = c(1, 2), wood_density = c(0.5, 0.6))
  mapping <- list(valid = TRUE, format = "wide", idtax_col = "idtax_n",
                  trait_cols = c(wood_density = "wood_density"),
                  metadata_cols = character(0), feature_cols = NULL,
                  available_traits = data.frame(trait = "wood_density",
                                                valuetype = "numeric",
                                                stringsAsFactors = FALSE))

  .execute_trait_import(df, mapping, pool = NULL, basis_resolved = "traitDatabase",
                        id_citation = 7L)
  expect_equal(captured$new_data$id_citation, c(7L, 7L))

  # No citation chosen: the column is NA, and add_sp_traits_measures() stores
  # no link
  .execute_trait_import(df, mapping, pool = NULL, basis_resolved = "traitDatabase",
                        id_citation = NA_integer_)
  expect_true(all(is.na(captured$new_data$id_citation)))
})

test_that("step 6 recalls the citation chosen in step 4", {
  df <- data.frame(idtax_n = 1, wood_density = 0.5)
  mapping <- list(valid = TRUE, format = "wide", idtax_col = "idtax_n",
                  trait_cols = c(wood_density = "wood_density"),
                  metadata_cols = character(0), feature_cols = NULL,
                  available_traits = data.frame(trait = "wood_density",
                                                valuetype = "numeric",
                                                stringsAsFactors = FALSE))
  chosen <- list(id_citation = 2L, citation = .cit_rows()[2, , drop = FALSE])

  shiny::testServer(
    mod_trait_preview_import_server,
    args = list(data = shiny::reactive(df), mapping = shiny::reactive(mapping),
                pool = shiny::reactive(NULL), i18n = .cit_i18n(),
                citation = shiny::reactive(chosen)),
    {
      expect_equal(selected_id_citation(), 2L)
      expect_match(as.character(output$citation_summary$html), "TRY_2020")
    }
  )
})

test_that("step 6 works when no citation step is wired in", {
  df <- data.frame(idtax_n = 1, wood_density = 0.5)
  mapping <- list(valid = TRUE, format = "wide", idtax_col = "idtax_n",
                  trait_cols = c(wood_density = "wood_density"),
                  metadata_cols = character(0), feature_cols = NULL,
                  available_traits = data.frame(trait = "wood_density",
                                                valuetype = "numeric",
                                                stringsAsFactors = FALSE))

  shiny::testServer(
    mod_trait_preview_import_server,
    args = list(data = shiny::reactive(df), mapping = shiny::reactive(mapping),
                pool = shiny::reactive(NULL), i18n = .cit_i18n()),
    {
      expect_true(is.na(selected_id_citation()))
    }
  )
})
