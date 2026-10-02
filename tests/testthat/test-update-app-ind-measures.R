# Data update app - adding a missing measurement to an individual (section 4b)

ind <- data.frame(id_n = 501L, id_table_liste_plots_n = 7L,
                  id_diconame_n = 33L, id_specimen = NA_integer_)
dbh <- data.frame(id_trait = 1L, trait = "ddiam", valuetype = "numeric",
                  minallowedvalue = 1, maxallowedvalue = 500)
note <- data.frame(id_trait = 9L, trait = "remark", valuetype = "character")
censuses <- data.frame(id_sub_plots = c(100L, 101L, 102L), census = c(1, 2, 3),
                       year = c(2010L, 2015L, 2020L), month = NA, day = NA)

test_that(".upd_ind_trait_choices puts the features already measured first", {
  traits <- data.frame(id_trait = 1:4, trait = c("zeta", "ddiam", "alpha", NA),
                       valuetype = "numeric")
  out <- CafriplotsR:::.upd_ind_trait_choices(traits, measured = "zeta")
  expect_equal(out$trait, c("zeta", "alpha", "ddiam"))
  expect_equal(out$group, c("measured", "other", "other"))

  none <- CafriplotsR:::.upd_ind_trait_choices(traits)
  expect_true(all(none$group == "other"))
})

test_that(".upd_ind_measure_row routes the value and copies the individual's links", {
  row <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 23.5, id_sub_plots = 102L,
                                            plot_name = "mbalmayo_1")
  expect_equal(nrow(row), 1L)
  expect_equal(row$traitvalue, 23.5)
  expect_true(is.na(row$traitvalue_char))
  expect_equal(row$id_data_individuals, 501L)
  expect_equal(row$id_table_liste_plots, 7L)
  expect_equal(row$id_diconame, 33L)
  expect_equal(row$id_sub_plots, 102L)
  expect_equal(row$traitid, 1L)
  expect_equal(row$original_plot_name, "mbalmayo_1")
  expect_true(is.na(row$year))
  expect_true(is.na(row$issue))
  expect_equal(row$date_modif_y, as.integer(format(Sys.Date(), "%Y")))

  chr <- CafriplotsR:::.upd_ind_measure_row(ind, note, "  broken top ")
  expect_equal(chr$traitvalue_char, "broken top")
  expect_true(is.na(chr$traitvalue))
  expect_true(is.na(chr$id_sub_plots))

  people <- data.frame(id_trait = 5L, valuetype = "table_colnam")
  expect_equal(CafriplotsR:::.upd_ind_measure_row(ind, people, "12")$traitvalue, 12)
})

test_that(".upd_ind_measure_row refuses empty and non-numeric values", {
  expect_error(CafriplotsR:::.upd_ind_measure_row(ind, dbh, NA), "No value")
  expect_error(CafriplotsR:::.upd_ind_measure_row(ind, dbh, ""), "No value")
  expect_error(CafriplotsR:::.upd_ind_measure_row(ind, dbh, NULL), "No value")
  expect_error(CafriplotsR:::.upd_ind_measure_row(ind, dbh, "abc"), "Not a number")
  expect_true(is.na(
    CafriplotsR:::.upd_ind_measure_row(ind, dbh, 10, issue = "  ")$issue
  ))
})

test_that(".upd_ind_measure_check requires a census unless the feature has none", {
  row <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 20)
  expect_equal(CafriplotsR:::.upd_ind_measure_check(row, dbh)$errors, "census_missing")

  linked <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 20, id_sub_plots = 101L)
  never <- CafriplotsR:::.upd_ind_measure_check(linked, dbh, policy = "never")
  expect_length(never$errors, 0)
  expect_true(is.na(never$row$id_sub_plots))
})

test_that(".upd_ind_measure_check refuses a second value for the same census", {
  stored <- data.frame(id_trait_measures = 1L, traitid = 1L, id_sub_plots = 101L,
                       traitvalue = 20, traitvalue_char = NA)
  same <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 21, id_sub_plots = 101L)
  other <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 25, id_sub_plots = 102L)

  expect_equal(CafriplotsR:::.upd_ind_measure_check(same, dbh, stored = stored)$errors,
               "duplicate")
  expect_length(CafriplotsR:::.upd_ind_measure_check(other, dbh, stored = stored)$errors, 0)

  # Already staged counts too.
  expect_equal(CafriplotsR:::.upd_ind_measure_check(other, dbh, staged = other)$errors,
               "duplicate")

  # A feature with no census allows a single value in all.
  loose <- data.frame(id_trait_measures = 2L, traitid = 9L, id_sub_plots = NA,
                      traitvalue = NA, traitvalue_char = "x")
  remark <- CafriplotsR:::.upd_ind_measure_row(ind, note, "y")
  expect_equal(
    CafriplotsR:::.upd_ind_measure_check(remark, note, "never", stored = loose)$errors,
    "duplicate"
  )
})

test_that(".upd_ind_measure_check enforces the allowed range", {
  low <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 0.5, id_sub_plots = 101L)
  high <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 900, id_sub_plots = 101L)
  expect_equal(CafriplotsR:::.upd_ind_measure_check(low, dbh)$errors, "below_min")
  expect_equal(CafriplotsR:::.upd_ind_measure_check(high, dbh)$errors, "above_max")

  # No bounds declared: nothing to enforce.
  free <- data.frame(id_trait = 1L, valuetype = "numeric")
  expect_length(CafriplotsR:::.upd_ind_measure_check(high, free)$errors, 0)
})

test_that(".upd_ind_measure_check flags a value lower than an earlier census", {
  stored <- data.frame(id_trait_measures = 1:2, traitid = 1L,
                       id_sub_plots = c(100L, 101L), traitvalue = c(18, 22),
                       traitvalue_char = NA)
  lower <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 20, id_sub_plots = 102L,
                                              issue = "checked")
  res <- CafriplotsR:::.upd_ind_measure_check(lower, dbh, stored = stored,
                                              censuses = censuses)
  expect_length(res$errors, 0)
  expect_equal(res$warnings, "lower_than_previous")
  expect_equal(res$row$issue, "checked|value lower than previous census")

  # Only earlier censuses count: a missing census 1 value lower than census 2
  # is not a decrease.
  first <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 10, id_sub_plots = 100L)
  later <- stored[2, , drop = FALSE]
  ok <- CafriplotsR:::.upd_ind_measure_check(first, dbh, stored = later,
                                             censuses = censuses)
  expect_length(ok$warnings, 0)
  expect_true(is.na(ok$row$issue))
})

test_that(".upd_ind_measure_label numbers staged rows and names the value", {
  row <- CafriplotsR:::.upd_ind_measure_row(ind, dbh, 23.5, id_sub_plots = 102L)
  a <- CafriplotsR:::.upd_ind_measure_label(row, "ddiam", "census 3")
  expect_equal(a$.label, "23.5")
  expect_equal(a$.key, 1L)
  b <- CafriplotsR:::.upd_ind_measure_label(row, "ddiam", "census 3", staged = a)
  expect_equal(b$.key, 2L)

  person <- CafriplotsR:::.upd_ind_measure_label(row, "observer", label = "Dauby G.")
  expect_equal(person$.label, "Dauby G.")
  expect_true(is.na(person$.census))
})

test_that(".upd_apply_ind_measures inserts the table columns only", {
  appended <- NULL
  table_name <- NULL
  testthat::local_mocked_bindings(
    .db_append_table = function(con, table, data, ...) {
      appended <<- data
      table_name <<- table
      nrow(data)
    },
    .package = "CafriplotsR"
  )
  row <- CafriplotsR:::.upd_ind_measure_label(
    CafriplotsR:::.upd_ind_measure_row(ind, dbh, 23.5, id_sub_plots = 102L),
    "ddiam", "census 3"
  )
  expect_equal(CafriplotsR:::.upd_apply_ind_measures(row, NULL), 1L)
  expect_equal(table_name, "data_traits_measures")
  expect_false(any(startsWith(names(appended), ".")))
  expect_equal(appended$traitvalue, 23.5)

  expect_equal(CafriplotsR:::.upd_apply_ind_measures(NULL, NULL), 0L)
})
