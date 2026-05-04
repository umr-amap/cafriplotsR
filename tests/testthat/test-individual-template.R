test_that("get_individual_template returns only the individuals sheet when features are disabled", {
  con <- structure(list(id = "provided"), class = "mock_connection")

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .get_individual_columns_from_db = function(con, method = NULL) {
      tibble::tibble(
        column_name = c("plot_name", "idtax_n"),
        data_type = c("character", "integer"),
        required = c(TRUE, TRUE),
        description = c("Plot", "Taxon"),
        validation = c("Required", "Required"),
        example = c("P1", "1001")
      )
    },
    .build_individuals_sheet = function(column_defs, method = NULL) {
      tibble::tibble(plot_name = "P1", idtax_n = 1001L)
    }
  )

  sheets <- get_individual_template(
    method = "1ha-IRD",
    include_features = FALSE,
    con = con,
    return_data = TRUE
  )

  expect_equal(names(sheets), "individuals")
  expect_equal(names(sheets$individuals), c("plot_name", "idtax_n"))
  expect_equal(sheets$individuals$plot_name, "P1")
})

test_that("get_individual_template writes both sheets when features are enabled", {
  written <- NULL

  testthat::local_mocked_bindings(
    .package = "CafriplotsR",
    .get_individual_columns_from_db = function(con, method = NULL) {
      tibble::tibble(
        column_name = "plot_name",
        data_type = "character",
        required = TRUE,
        description = "Plot",
        validation = "Required",
        example = "P1"
      )
    },
    .build_individuals_sheet = function(column_defs, method = NULL) {
      tibble::tibble(plot_name = "P1")
    },
    .build_features_sheet = function(con) {
      tibble::tibble(trait = "stem_diameter")
    },
    call.mydb = function() structure(list(id = "auto"), class = "mock_connection")
  )
  testthat::local_mocked_bindings(
    .package = "writexl",
    write_xlsx = function(x, path) {
      written <<- list(x = x, path = path)
      invisible(path)
    }
  )
  testthat::local_mocked_bindings(
    .package = "DBI",
    dbDisconnect = function(con) TRUE
  )

  expect_no_error(
    get_individual_template(
      include_features = TRUE,
      output_file = "template.xlsx",
      con = NULL,
      return_data = FALSE
    )
  )

  expect_equal(names(written$x), c("individuals", "features"))
  expect_equal(written$path, "template.xlsx")
  expect_equal(written$x$features$trait, "stem_diameter")
})

test_that(".get_individual_columns_from_db marks tag as mandatory for selected methods", {
  columns <- CafriplotsR:::.get_individual_columns_from_db(
    con = structure(list(), class = "mock_connection"),
    method = "1ha-IRD"
  )

  tag_row <- columns[columns$column_name == "tag", , drop = FALSE]
  plot_row <- columns[columns$column_name == "plot_name", , drop = FALSE]

  expect_equal(tag_row$method_note, "MANDATORY for method: 1ha-IRD")
  expect_true(is.na(plot_row$method_note))
})
