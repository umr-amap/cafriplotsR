# Data update app, section 4b: observation features of a plot feature record
# (data_subplot_feat). The row builder and the type grouping are pure.

existing_obs <- data.frame(
  id_subplot_feat  = c(1L, 2L, 3L),
  id_sub_plots     = c(10L, 10L, 11L),
  id_type_sub_plot = c(250L, 249L, 249L),
  typevalue        = c(5, 6, 7),
  typevalue_char   = NA_character_
)

test_that("people ids go to typevalue, one row each", {
  rows <- CafriplotsR:::.upd_obs_feature_rows(
    10L, 249L, "table_colnam", c("8", "9", "8", "")
  )
  expect_equal(nrow(rows), 2L)
  expect_equal(rows$typevalue, c(8, 9))
  expect_true(all(is.na(rows$typevalue_char)))
  expect_equal(unique(rows$id_sub_plots), 10L)
  expect_equal(unique(rows$id_type_sub_plot), 249L)
  expect_false(anyNA(rows$date_modif_y))
})

test_that("a person already recorded for that subplot and type is dropped", {
  rows <- CafriplotsR:::.upd_obs_feature_rows(
    10L, 249L, "table_colnam", c("6", "7"), existing = existing_obs
  )
  # 6 is already additional_people of subplot 10; 7 is so only of subplot 11
  expect_equal(rows$typevalue, 7)

  none <- CafriplotsR:::.upd_obs_feature_rows(
    10L, 250L, "table_colnam", "5", existing = existing_obs
  )
  expect_equal(nrow(none), 0L)
})

test_that("character types go to typevalue_char", {
  rows <- CafriplotsR:::.upd_obs_feature_rows(10L, 300L, "character", " good ")
  expect_equal(rows$typevalue_char, "good")
  expect_true(is.na(rows$typevalue))
})

test_that("a numeric type refuses text and accepts numbers", {
  expect_error(
    CafriplotsR:::.upd_obs_feature_rows(10L, 301L, "numeric", "abc"),
    "Not a number"
  )
  rows <- CafriplotsR:::.upd_obs_feature_rows(10L, 301L, "numeric", 12.5)
  expect_equal(rows$typevalue, 12.5)
})

test_that("empty values give zero rows with the table's columns", {
  rows <- CafriplotsR:::.upd_obs_feature_rows(10L, 249L, "table_colnam", NA)
  expect_equal(nrow(rows), 0L)
  expect_true(all(c("id_sub_plots", "id_type_sub_plot", "typevalue",
                    "typevalue_char") %in% names(rows)))
  expect_equal(
    nrow(CafriplotsR:::.upd_obs_feature_rows(10L, 249L, "table_colnam", NULL)),
    0L
  )
})

test_that("feature types are grouped people, used, other", {
  types <- data.frame(
    id_subplotype = c(1L, 249L, 250L, 300L, 301L),
    type = c("census", "additional_people", "team_leader", "remark", "slope"),
    valuetype = c("numeric", "table_colnam", "table_colnam", "character", "numeric")
  )

  short <- CafriplotsR:::.upd_group_obs_feature_types(types, used = c(249L, 300L))
  expect_equal(short$type, c("additional_people", "team_leader", "remark"))
  expect_equal(short$group, c("people", "people", "used"))

  all <- CafriplotsR:::.upd_group_obs_feature_types(types, used = 300L,
                                                     all_types = TRUE)
  expect_equal(all$type, c("additional_people", "team_leader", "remark",
                           "census", "slope"))
})

stored_obs <- tibble::tibble(
  id_subplot_feat = c(1L, 2L, 3L), id_sub_plots = c(10L, 10L, 11L),
  id_type_sub_plot = c(250L, 249L, 249L), type = c("team_leader",
  "additional_people", "additional_people"), valuetype = "table_colnam",
  typevalue = c(5, 6, 7), typevalue_char = NA_character_,
  colnam = c("Person A", "Person B", "Person C")
)

test_that("staged additions are labelled and checked against stored rows", {
  staged <- CafriplotsR:::.upd_obs_label_rows(
    CafriplotsR:::.upd_obs_feature_rows(10L, 249L, "table_colnam", "8"),
    "additional_people", people = c("Person D" = "8")
  )
  expect_equal(staged$.label, "Person D")
  expect_equal(staged$.key, 1L)

  # 8 is now staged, 6 is stored: neither may be added twice
  existing <- CafriplotsR:::.upd_obs_existing(stored_obs, staged)
  rows <- CafriplotsR:::.upd_obs_feature_rows(
    10L, 249L, "table_colnam", c("6", "8", "9"), existing = existing
  )
  expect_equal(rows$typevalue, 9)
  more <- CafriplotsR:::.upd_obs_label_rows(rows, "additional_people",
                                            people = NULL, staged = staged)
  expect_equal(more$.key, 2L)
  expect_equal(more$.label, "9")

  expect_null(CafriplotsR:::.upd_obs_existing(NULL, NULL))
  expect_null(CafriplotsR:::.upd_obs_existing(tibble::tibble(), NULL))
})

test_that(".upd_obs_view() marks each row stored, to remove or to add", {
  staged <- CafriplotsR:::.upd_obs_label_rows(
    CafriplotsR:::.upd_obs_feature_rows(10L, 249L, "table_colnam", "8"),
    "additional_people", people = c("Person D" = "8")
  )
  v <- CafriplotsR:::.upd_obs_view(stored_obs, staged, drop = 2L, id_sub_plots = 10L)
  expect_equal(v$label, c("Person A", "Person B", "Person D"))
  expect_equal(v$status, c("stored", "remove", "add"))
  expect_equal(v$key, c(NA, NA, 1L))

  empty <- CafriplotsR:::.upd_obs_view(tibble::tibble(), NULL, integer(0), 10L)
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("status", "label") %in% names(empty)))
})

test_that(".upd_apply_obs_features removes, then inserts the table columns only", {
  executed <- NULL
  appended <- NULL
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbExecute = function(con, sql, ...) { executed <<- as.character(sql); 2L }
  )
  testthat::local_mocked_bindings(
    .db_append_table = function(con, table, data, ...) {
      appended <<- data
      nrow(data)
    },
    .package = "CafriplotsR"
  )
  staged <- CafriplotsR:::.upd_obs_label_rows(
    CafriplotsR:::.upd_obs_feature_rows(10L, 249L, "table_colnam", "8"),
    "additional_people"
  )
  con <- DBI::ANSI()  # quotes like a real connection; dbExecute is mocked

  res <- CafriplotsR:::.upd_apply_obs_features(
    list(add = staged, remove = c(1L, 2L)), con
  )
  expect_equal(res, list(n_added = 1L, n_removed = 2L))
  expect_match(executed, "DELETE FROM data_subplot_feat WHERE id_subplot_feat IN [(]1, 2[)]")
  expect_false(any(startsWith(names(appended), ".")))
  expect_equal(appended$typevalue, 8)

  expect_equal(CafriplotsR:::.upd_apply_obs_features(NULL, con),
               list(n_added = 0L, n_removed = 0L))
})
