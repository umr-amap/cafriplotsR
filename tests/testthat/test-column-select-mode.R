# Multi-column mode lets any of the three components be left at "(none)".
# Shiny's `req()` treats "" as missing, so requiring the three selectors
# outright halted the module silently, and everything downstream that waits on
# the selected column - the Start Matching button first of all - never showed
# up. These tests pin the combinations that must work.

.select_i18n <- function() {
  shiny::reactive(list(t = function(x) x))
}

.select_data <- function() {
  data.frame(
    genus   = c("Gilbertiodendron", "Garcinia", NA),
    epithet = c("dewevrei", "kola", NA),
    family  = c("Fabaceae", "Clusiaceae", "Annonaceae"),
    stringsAsFactors = FALSE
  )
}

.select_info <- function(...) {
  out <- NULL
  shiny::testServer(
    mod_column_select_server,
    args = list(data = shiny::reactive(.select_data()), i18n = .select_i18n()),
    {
      session$setInputs(include_authors = FALSE, ...)
      out <<- session$getReturned()()
    }
  )
  out
}

test_that("genus and epithet without a family still yield a name column", {
  info <- .select_info(
    column_mode    = "multiple",
    genus_column   = "genus",
    species_column = "epithet",
    family_column  = ""
  )

  expect_equal(info$column, "taxonomic_name_combined")
  expect_equal(
    info$data$taxonomic_name_combined,
    c("Gilbertiodendron dewevrei", "Garcinia kola", NA)
  )
  expect_equal(info$mode, "multiple")
  expect_equal(info$family_column, "")
})

test_that("a genus column on its own is enough", {
  info <- .select_info(
    column_mode    = "multiple",
    genus_column   = "genus",
    species_column = "",
    family_column  = ""
  )

  expect_equal(
    info$data$taxonomic_name_combined,
    c("Gilbertiodendron", "Garcinia", NA)
  )
})

test_that("a family column on its own is enough", {
  info <- .select_info(
    column_mode    = "multiple",
    genus_column   = "",
    species_column = "",
    family_column  = "family"
  )

  expect_equal(
    info$data$taxonomic_name_combined,
    c("Fabaceae", "Clusiaceae", "Annonaceae")
  )
})

test_that("selecting no column at all leaves the module waiting", {
  expect_error(
    .select_info(
      column_mode    = "multiple",
      genus_column   = "",
      species_column = "",
      family_column  = ""
    ),
    class = "shiny.silent.error"
  )
})

test_that("single-column mode is unaffected", {
  info <- .select_info(column_mode = "single", column_name = "genus")

  expect_equal(info$column, "genus")
  expect_equal(info$mode, "single")
  expect_equal(info$genus_column, "")
})
