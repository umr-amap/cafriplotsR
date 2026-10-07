# Tests for R/specimen_duplicate_check.R
#
# The specimens table has no unique constraint on collector + number + suffix,
# so the import wizard must flag rows already in the database and rows repeated
# within the uploaded file.

upload <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

existing_specimens <- function() {
  data.frame(
    id_specimen = c(10L, 11L, 12L, 13L),
    id_colnam = c(1L, 1L, 1L, 2L),
    colnbr = c(100L, 101L, 101L, 100L),
    suffix = c(NA, NA, NA, "b"),
    stringsAsFactors = FALSE
  )
}


test_that(".specimen_key() treats equivalent spellings as equal", {
  expect_identical(
    .specimen_key(c(1L, 1L), c("123", 123), c("A ", "a")),
    rep("1|123|a", 2)
  )
  # empty suffix and missing suffix are the same specimen
  expect_identical(
    .specimen_key(c(1L, 1L), c(5, 5), c("", NA)),
    rep("1|5|", 2)
  )
  # no suffix column at all
  expect_identical(.specimen_key(1L, 5), "1|5|")
  # large numbers are not written in scientific notation
  expect_identical(.specimen_key(1L, 100000), "1|100000|")
})

test_that(".specimen_key() returns NA when collector or number is missing", {
  key <- .specimen_key(c(NA, 1L, 1L), c(1, NA, ""), NULL)
  expect_true(all(is.na(key)))
})

test_that(".flag_specimen_duplicates() finds rows already in the database", {
  data <- upload(
    id_colnam = c(1L, 1L, 2L, 2L),
    num = c(100, 200, 100, 100),
    suf = c(NA, NA, "B", NA)
  )
  flagged <- .flag_specimen_duplicates(data, "num", "suf", existing_specimens())

  expect_identical(flagged$in_db, c(TRUE, FALSE, TRUE, FALSE))
  expect_identical(flagged$existing_id_specimen, c("10", NA, "13", NA))
  expect_false(any(flagged$dup_in_file))
  expect_identical(flagged$is_duplicate, flagged$in_db)
})

test_that("existing duplicates in the database are all reported", {
  data <- upload(id_colnam = 1L, num = 101)
  flagged <- .flag_specimen_duplicates(data, "num", NULL, existing_specimens())
  expect_identical(flagged$existing_id_specimen, "11, 12")
})

test_that(".flag_specimen_duplicates() flags every copy repeated in the file", {
  data <- upload(
    id_colnam = c(1L, 1L, 1L, 1L, NA),
    num = c(5, 6, 5, 5, 5),
    suf = c("a", NA, "A", "b", "a")
  )
  flagged <- .flag_specimen_duplicates(data, "num", "suf", NULL)

  # header is line 1, so data rows start at line 2
  expect_identical(flagged$file_line, 2:6)
  expect_identical(flagged$dup_in_file, c(TRUE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(flagged$dup_file_lines, c("2, 4", NA, "2, 4", NA, NA))
  expect_false(any(flagged$in_db))
})

test_that("a row can be both in the database and repeated in the file", {
  data <- upload(id_colnam = c(1L, 1L), num = c(100, 100))
  flagged <- .flag_specimen_duplicates(data, "num", NULL, existing_specimens())
  expect_true(all(flagged$in_db & flagged$dup_in_file))
  expect_true(all(flagged$is_duplicate))
})

test_that(".flag_specimen_duplicates() handles edge cases", {
  # no collector matched: nothing can be flagged
  data <- upload(id_colnam = c(NA_integer_, NA_integer_), num = c(1, 1))
  flagged <- .flag_specimen_duplicates(data, "num", NULL, existing_specimens())
  expect_false(any(flagged$is_duplicate))

  # zero rows
  empty <- upload(id_colnam = integer(), num = numeric())
  flagged <- .flag_specimen_duplicates(empty, "num", NULL, existing_specimens())
  expect_identical(nrow(flagged), 0L)
  expect_true(all(c("in_db", "dup_in_file", "is_duplicate") %in% names(flagged)))

  # empty existing table
  flagged <- .flag_specimen_duplicates(
    upload(id_colnam = 1L, num = 100), "num", NULL, existing_specimens()[0, ]
  )
  expect_false(flagged$in_db)
})


test_that("the import step reports duplicates and leaves them out of the import", {
  local_mocked_bindings(
    .fetch_existing_specimen_keys = function(con, id_colnam) existing_specimens()
  )
  data <- upload(
    collector = c("Dauby", "Dauby", "Dauby", "Dauby"),
    num = c(100, 300, 300, 400),
    idtax_n = c(5L, 5L, 5L, 5L),
    id_colnam = c(1L, 1L, 1L, 1L)
  )

  shiny::testServer(
    mod_specimen_import_server,
    args = list(
      matched_data = shiny::reactive(data),
      mappings = shiny::reactive(list(collector = "collector", colnbr = "num",
                                      idtax_n = "idtax_n")),
      matching_complete = shiny::reactive(TRUE),
      con = shiny::reactive(structure(list(), class = "fake_con")),
      i18n = shiny::reactive(list(t = function(x) x))
    ),
    {
      report <- duplicates_report()
      expect_identical(report$line, 2:4)
      expect_identical(report$collector, rep("Dauby", 3))
      expect_match(report$problem[1], "id_specimen 10")
      expect_match(report$problem[2], "lines 3, 4")

      session$setInputs(dry_run = TRUE, import_btn = 1)
      results <- session$returned$results()
      expect_identical(results$n_specimens, 1L)
      expect_identical(results$n_skipped_dup, 3L)
      expect_identical(results$preview$colnbr, 400)
    }
  )
})
