# The preview between column selection and matching exists so a wrong column
# is caught before a long run. These tests pin what it reports: the counts
# must be the ones the matching pipeline will actually work from, and the
# signals that betray a mis-selected column must fire.

.preview_i18n <- function() {
  shiny::reactive(list(t = function(x) x))
}

test_that(".summarise_names_to_match() counts rows, blanks and distinct names", {
  values <- c("Garcinia kola", "Garcinia kola", "Gilbertiodendron dewevrei",
              NA, "", "   ")

  s <- .summarise_names_to_match(values)

  expect_equal(s$n_rows, 6L)
  expect_equal(s$n_missing, 3L)
  expect_equal(s$n_unique, 2L)
})

test_that(".summarise_names_to_match() orders by frequency, most frequent first", {
  values <- c("Garcinia kola", "Garcinia kola", "Garcinia kola",
              "Cola nitida", "Cola nitida",
              "Brachystegia laurentii")

  s <- .summarise_names_to_match(values)

  expect_equal(s$names$name, c("Garcinia kola", "Cola nitida",
                               "Brachystegia laurentii"))
  expect_equal(s$names$n, c(3L, 2L, 1L))
})

test_that(".summarise_names_to_match() reports the normalised form actually searched", {
  s <- .summarise_names_to_match(c("Garcinia sp.", "Cola  cf.  nitida"))

  searched <- stats::setNames(s$names$searched, s$names$name)

  expect_equal(unname(searched[["Garcinia sp."]]), "Garcinia")
  expect_equal(unname(searched[["Cola  cf.  nitida"]]), "Cola nitida")
})

test_that(".summarise_names_to_match() reports the rank the parser detects", {
  s <- .summarise_names_to_match(c("Garcinia kola", "Brachystegia",
                                   "Fabaceae"))

  rank <- stats::setNames(s$names$rank, s$names$name)

  expect_equal(unname(rank[["Garcinia kola"]]), "species")
  expect_equal(unname(rank[["Brachystegia"]]), "genus")
  expect_equal(unname(rank[["Fabaceae"]]), "family")

  expect_true(s$parsed)
  expect_equal(unname(s$rank_counts[["species"]]), 1L)
  expect_equal(unname(s$rank_counts[["genus"]]), 1L)
  expect_equal(unname(s$rank_counts[["family"]]), 1L)
})

test_that(".summarise_names_to_match() skips rank detection past max_parse", {
  # The parse is the only part that grows with the list, so it is capped; the
  # counts must survive the cap intact.
  values <- paste("Genus", sprintf("species%03d", 1:20))

  s <- .summarise_names_to_match(values, max_parse = 5L)

  expect_false(s$parsed)
  expect_equal(s$n_unique, 20L)
  expect_true(all(is.na(s$names$rank)))
  expect_length(s$rank_counts, 0L)
})

test_that(".summarise_names_to_match() copes with an entirely empty column", {
  s <- .summarise_names_to_match(c(NA_character_, "", "  "))

  expect_equal(s$n_rows, 3L)
  expect_equal(s$n_missing, 3L)
  expect_equal(s$n_unique, 0L)
  expect_equal(nrow(s$names), 0L)
})

test_that("the preview module summarises the selected column", {
  df <- data.frame(
    taxon = c("Garcinia kola", "Garcinia kola", "Brachystegia laurentii", NA),
    other = c("a", "b", "c", "d"),
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    mod_name_preview_server,
    args = list(
      data = shiny::reactive(df),
      column_name = shiny::reactive("taxon"),
      i18n = .preview_i18n()
    ),
    {
      s <- session$getReturned()()

      expect_equal(s$n_rows, 4L)
      expect_equal(s$n_unique, 2L)
      expect_equal(s$n_missing, 1L)
    }
  )
})

test_that("the preview module waits for a column that exists in the data", {
  df <- data.frame(taxon = "Garcinia kola", stringsAsFactors = FALSE)

  # The combined column is built by the selection module, so it can lag a
  # frame behind a change of mode. Blocking beats erroring on the way through.
  shiny::testServer(
    mod_name_preview_server,
    args = list(
      data = shiny::reactive(df),
      column_name = shiny::reactive("taxonomic_name_combined"),
      i18n = .preview_i18n()
    ),
    {
      expect_error(session$getReturned()(), class = "shiny.silent.error")
    }
  )
})
