# Tests for update_specimen_fields() and its helpers
# (R/updates_tables_functions.R)

mock_con <- function() structure(list(), class = "mock_con")

specimen_row <- function(...) {
  defaults <- list(
    id_specimen       = 42L,
    colnbr            = 1234,
    suffix            = NA_character_,
    coly              = 2001L,
    colm              = 5L,
    cold              = 12L,
    add_col           = NA_character_,
    locality          = "Mont Bela",
    country           = "Gabon",
    ddlat             = 0.123,
    ddlon             = 11.456,
    description       = NA_character_,
    original_tax_name = NA_character_,
    idtax_n           = 7L
  )
  overrides <- list(...)
  defaults[names(overrides)] <- overrides
  as.data.frame(defaults, stringsAsFactors = FALSE)
}

test_that(".specimen_editable_fields() excludes identification columns", {
  fields <- .specimen_editable_fields()

  expect_type(fields, "character")
  expect_true(all(nzchar(names(fields))))
  expect_true(all(fields %in% c("character", "numeric", "integer")))
  expect_true(all(c("ddlat", "ddlon", "locality", "coly", "colm", "cold",
                    "description", "suffix") %in% names(fields)))
  expect_false(any(c("idtax_n", "detby", "detd", "detm", "dety", "detvalue")
                   %in% names(fields)))
})

test_that("the manual app section only exposes editable specimen fields", {
  expect_true(all(names(.SPECID_MANUAL_FIELDS) %in%
                    names(.specimen_editable_fields())))
  expect_true(all(.SPECID_MANUAL_FIELDS %in% c("numeric", "text")))
})

test_that(".coerce_specimen_value() maps blanks and NA to typed NA", {
  expect_identical(.coerce_specimen_value("", "character", "locality"),
                   NA_character_)
  expect_identical(.coerce_specimen_value("   ", "character", "locality"),
                   NA_character_)
  expect_identical(.coerce_specimen_value(NA, "numeric", "ddlat"), NA_real_)
  expect_identical(.coerce_specimen_value(NA, "integer", "coly"), NA_integer_)
  expect_identical(.coerce_specimen_value(NA, "character", "locality"),
                   NA_character_)
})

test_that(".coerce_specimen_value() coerces to the declared type", {
  expect_identical(.coerce_specimen_value("2020", "integer", "coly"), 2020L)
  expect_identical(.coerce_specimen_value(2020, "integer", "coly"), 2020L)
  expect_equal(.coerce_specimen_value("-1.25", "numeric", "ddlat"), -1.25)
  expect_identical(.coerce_specimen_value(12, "character", "locality"), "12")
})

test_that(".coerce_specimen_value() rejects invalid values", {
  expect_error(.coerce_specimen_value("abc", "numeric", "ddlat"), "not numeric")
  expect_error(.coerce_specimen_value(2020.5, "integer", "coly"),
               "whole number")
  expect_error(.coerce_specimen_value(c(1, 2), "numeric", "ddlat"),
               "length 1")
})

test_that(".same_specimen_value() treats missing values as equivalent", {
  expect_true(.same_specimen_value(NA, NA_character_))
  expect_true(.same_specimen_value(NA_real_, NA_real_))
  expect_false(.same_specimen_value(NA_character_, "Gabon"))
  expect_false(.same_specimen_value("Gabon", NA_character_))
  expect_true(.same_specimen_value("Gabon", "Gabon"))
  expect_false(.same_specimen_value("Gabon", "Congo"))
  expect_true(.same_specimen_value(1L, 1))
  expect_true(.same_specimen_value("2001", 2001L))
  expect_false(.same_specimen_value(0.123, 0.124))
})

test_that("update_specimen_fields() rejects fields that are not editable", {
  expect_error(
    update_specimen_fields(id_speci = 42, new_values = list(idtax_n = 3),
                           con = mock_con()),
    "not editable"
  )
  expect_error(
    update_specimen_fields(id_speci = 42, new_values = list(nonsense = 3),
                           con = mock_con()),
    "not editable"
  )
})

test_that("update_specimen_fields() validates id and new_values", {
  expect_error(
    update_specimen_fields(id_speci = c(1, 2), new_values = list(ddlat = 1),
                           con = mock_con()),
    "single, non-missing"
  )
  expect_error(
    update_specimen_fields(id_speci = 42, new_values = "ddlat",
                           con = mock_con()),
    "named list"
  )
  expect_error(
    update_specimen_fields(id_speci = 42, new_values = list(1),
                           con = mock_con()),
    "must be named"
  )
})

test_that("update_specimen_fields() returns an empty result when given nothing", {
  res <- update_specimen_fields(id_speci = 42, new_values = list(),
                                con = mock_con())

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)

  # NULL elements mean "leave this field alone"
  res_null <- update_specimen_fields(id_speci = 42,
                                     new_values = list(ddlat = NULL),
                                     con = mock_con())
  expect_equal(nrow(res_null), 0)
})

test_that("update_specimen_fields() reports an unknown specimen without writing", {
  written <- 0L
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(conn, statement, ...) specimen_row()[0, ]
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .write_specimen_fields = function(...) {
      written <<- written + 1L
      1L
    }
  )

  res <- update_specimen_fields(id_speci = 999, new_values = list(ddlat = 1),
                                ask_before_update = FALSE,
                                show_results = FALSE, con = mock_con())

  expect_equal(nrow(res), 0)
  expect_equal(written, 0L)
})

test_that("update_specimen_fields() skips values identical to the stored ones", {
  written <- 0L
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(conn, statement, ...) specimen_row()
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .write_specimen_fields = function(...) {
      written <<- written + 1L
      1L
    }
  )

  res <- update_specimen_fields(
    id_speci = 42,
    new_values = list(locality = "Mont Bela", ddlat = 0.123, coly = 2001,
                      description = NA),
    ask_before_update = FALSE, show_results = FALSE, con = mock_con()
  )

  expect_equal(nrow(res), 0)
  expect_equal(written, 0L)
})

test_that("update_specimen_fields() writes only the changed fields", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(conn, statement, ...) specimen_row()
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .write_specimen_fields = function(con, id_speci, coerced, current_record,
                                      modif_type, add_backup = TRUE) {
      captured <<- list(id_speci = id_speci, coerced = coerced,
                        modif_type = modif_type, add_backup = add_backup)
      1L
    }
  )

  res <- update_specimen_fields(
    id_speci = 42,
    new_values = list(
      locality    = "Mont Bela",       # unchanged
      ddlat       = -0.5,              # changed
      coly        = "1999",            # changed, coerced to integer
      description = "collected in flower"  # currently NA -> set
    ),
    ask_before_update = FALSE, show_results = FALSE, con = mock_con()
  )

  expect_equal(nrow(res), 3)
  expect_setequal(res$field, c("ddlat", "coly", "description"))
  expect_equal(res$current[res$field == "description"], NA_character_)
  expect_equal(res$new[res$field == "ddlat"], "-0.5")

  expect_equal(captured$id_speci, 42L)
  expect_setequal(names(captured$coerced), c("ddlat", "coly", "description"))
  expect_identical(captured$coerced$coly, 1999L)
  expect_identical(captured$coerced$ddlat, -0.5)
  expect_true(grepl("ddlat__", captured$modif_type, fixed = TRUE))
})

test_that("update_specimen_fields() clears a field when given NA", {
  captured <- NULL
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(conn, statement, ...) specimen_row()
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .write_specimen_fields = function(con, id_speci, coerced, current_record,
                                      modif_type, add_backup = TRUE) {
      captured <<- coerced
      1L
    }
  )

  res <- update_specimen_fields(
    id_speci = 42,
    new_values = list(locality = "", ddlon = NA),
    ask_before_update = FALSE, show_results = FALSE, con = mock_con()
  )

  expect_equal(nrow(res), 2)
  expect_true(all(is.na(res$new)))
  expect_identical(captured$locality, NA_character_)
  expect_identical(captured$ddlon, NA_real_)
})

test_that("update_specimen_fields() honours a declined confirmation", {
  written <- 0L
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbGetQuery = function(conn, statement, ...) specimen_row()
  )
  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    choose_prompt = function(message) FALSE,
    .write_specimen_fields = function(...) {
      written <<- written + 1L
      1L
    }
  )

  res <- update_specimen_fields(id_speci = 42,
                                new_values = list(locality = "Elsewhere"),
                                ask_before_update = TRUE,
                                show_results = FALSE, con = mock_con())

  expect_equal(nrow(res), 1)
  expect_equal(written, 0L)
})
