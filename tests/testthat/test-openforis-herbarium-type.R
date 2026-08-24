test_that(".flag_type_individuals() names each specimen exactly once", {

  voucher <- c("A1", "A1", "A1", NA, "A2")

  res <- .flag_type_individuals(voucher)

  expect_equal(res, c("A1", NA, NA, NA, "A2"))
  expect_false(anyDuplicated(stats::na.omit(res)) > 0)
})


test_that(".flag_type_individuals() prefers the stem carrying a collection number", {

  res <- .flag_type_individuals(
    voucher = c("A1", "A1", "A1"),
    specimen_number = c(NA, "A1", NA)
  )

  expect_equal(res, c(NA, "A1", NA))
})


test_that(".flag_type_individuals() prefers the stem flagged as collected", {

  res <- .flag_type_individuals(
    voucher = c("A1", "A1", "A1"),
    specimen_number = c("A1", NA, NA),
    collected_flag = c(NA, 1, NA)
  )

  expect_equal(res, c(NA, "A1", NA))
})


test_that(".flag_type_individuals() ignores blank vouchers", {

  res <- .flag_type_individuals(c("", "  ", NA))

  expect_true(all(is.na(res)))
})


test_that(".flag_type_individuals() handles zero-length and single inputs", {

  expect_equal(.flag_type_individuals(character(0)), character(0))
  expect_equal(.flag_type_individuals("A1"), "A1")
  expect_equal(.flag_type_individuals(NA_character_), NA_character_)
})


test_that(".prepare_openforis_new_plot_individuals() repeats the voucher but not the type", {

  trees <- data.frame(
    plot_name = c("P1", "P1", "P1", "P2"),
    tag = c(5, 2, 9, 1),
    herbarium_nbe_char = c("12", "12", NA, "13"),
    specimen_number = c("12", NA, NA, "13"),
    species_scientific_name = "Sp a",
    stringsAsFactors = FALSE
  )

  res <- .prepare_openforis_new_plot_individuals(trees, specimen_prefix = "PIRD")

  # Every stem identified as the species of a voucher keeps the reference
  expect_equal(sum(!is.na(res$herbarium_nbe_char)), 3L)

  # ... but each specimen is the type of a single individual
  types <- stats::na.omit(res$herbarium_nbe_type)
  expect_equal(sort(as.character(types)), c("PIRD 12", "PIRD 13"))
  expect_equal(res$herbarium_nbe_type[res$plot_name == "P1" & res$tag == 5], "PIRD 12")
  expect_true(is.na(res$herbarium_nbe_type[res$plot_name == "P1" & res$tag == 2]))
})


test_that(".prepare_openforis_new_plot_individuals() falls back on the lowest tag", {

  trees <- data.frame(
    plot_name = c("P1", "P1"),
    tag = c(7, 3),
    herbarium_nbe_char = c("12", "12"),
    species_scientific_name = "Sp a",
    stringsAsFactors = FALSE
  )

  res <- .prepare_openforis_new_plot_individuals(trees)

  expect_equal(res$herbarium_nbe_type, c("12", NA))
  expect_equal(res$tag, c(3, 7))
})
