test_that(".format_label_date() handles complete and partial dates", {
  out <- CafriplotsR:::.format_label_date(
    day   = c(12, NA, 5, NA, NA, 40, 3),
    month = c(3, 11, NA, NA, NA, 2, 13),
    year  = c(2024, 2023, 2022, 2021, NA, 2020, NA)
  )
  expect_equal(
    out,
    c("12 mar 2024", "nov 2023", "2022", "2021", "", "feb 2020", "")
  )
})

test_that(".format_label_date() coerces character input and junk to NA", {
  out <- CafriplotsR:::.format_label_date(c("7", "x"), c("6", ""),
                                          c("2019", "NA"))
  expect_equal(out, c("7 jun 2019", ""))
})

test_that(".format_label_date() works on a single row", {
  expect_equal(CafriplotsR:::.format_label_date(1, 1, 2000), "1 jan 2000")
})

test_that("generate_specimen_labels() renders date and cropped extras", {
  df <- data.frame(
    Specimen = c("IRD plot 2508", "IRD plot 2509", "IRD plot 2510"),
    Family   = c("Fabaceae", "Annonaceae", NA),
    Name     = c("Amphimas pterocarpoides", "Uvariastrum pierreanum", NA),
    Locality = c("mpem008", "mpem008", NA),
    cold = c(12, NA, NA),
    colm = c(3, 11, NA),
    coly = c(2024, 2023, NA),
    description = c(
      paste(rep("Tree 25 m, bark smooth, slash red with white latex.", 6),
            collapse = " "),
      NA,
      "Averyveryveryveryveryveryveryveryveryveryveryverylongsingleword"
    )
  )
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)

  res <- generate_specimen_labels(
    df, out, include_date = TRUE, extra_cols = "description",
    extra_max_lines = 3, device = "pdf", family = "sans"
  )
  expect_identical(res, out)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("generate_specimen_labels() renders determination and several extras", {
  df <- data.frame(
    Specimen = c("IRD 1", "IRD 2", "IRD 3", "IRD 4"),
    Family = "Fabaceae", Name = "X y", Locality = "loc",
    detby = c("G. Dauby", NA, "NA", NA),
    detd = c(4, NA, NA, NA), detm = c(5, NA, 2, NA),
    dety = c(2025, NA, 2020, NA),
    Determination = c(NA, "conf. J. Doe 2019", NA, NA),
    description = c("Tree 25 m", NA, "Shrub", NA),
    habitat = c("Swamp forest", "Terra firme", NA, NA)
  )
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  expect_no_error(generate_specimen_labels(
    df, out, include_determination = TRUE, include_date = TRUE,
    extra_cols = c("description", "habitat"),
    device = "pdf", family = "sans"
  ))
  expect_gt(file.size(out), 0)
})

test_that("generate_specimen_labels() tolerates missing date columns", {
  df <- data.frame(Specimen = "IRD 1", Family = "Fabaceae",
                   Name = "X y", Locality = "loc")
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  expect_no_error(generate_specimen_labels(
    df, out, include_date = TRUE, extra_cols = "description",
    device = "pdf", family = "sans"
  ))
})
