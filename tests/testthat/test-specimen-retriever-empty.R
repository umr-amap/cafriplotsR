# Tests for R/mod_specimen_retriever.R
#
# Collector/number combinations parsed from the herbarium columns often do not
# exist in the specimens table yet. query_specimens() then returns its raw,
# un-enriched (sometimes zero-column) result, which used to blow up the join
# and mutate that follow ("In argument: `specimen_idtax_n = idtax_f`").
# The step must report the absence instead.

fake_parsed_data <- function() {
  data.frame(
    id_n = c(1L, 2L),
    tag = c(101, 102),
    code_individu = c("A1", "A2"),
    plot_name = c("PLOT1", "PLOT1"),
    idtax_n = c(500L, 501L),
    extracted_collector = c("Dauby", "Dauby"),
    extracted_number = c(3000L, 3001L),
    link_type = c("type_individual", "referenced_individual"),
    source_column = c("herbarium_nbe_type", "herbarium_nbe_char"),
    original_value = c("Dauby 3000", "Dauby 3001"),
    stringsAsFactors = FALSE
  )
}

retriever_args <- function(parsed = fake_parsed_data(),
                           matches = list(collector = list(Dauby = "42"))) {
  list(
    parsed_data = shiny::reactive(parsed),
    collector_matches = shiny::reactive(matches),
    con = shiny::reactive(structure(list(), class = "fake_con")),
    i18n = shiny::reactive(list(t = function(x) x))
  )
}


test_that(".normalize_retrieved_specimens() fills in what an empty query omits", {
  # what query_specimens() hands back when nothing matched: no enrichment,
  # so no colnam/idtax_f, and possibly no columns at all
  normalized <- .normalize_retrieved_specimens(data.frame())

  expect_true(all(c("id_specimen", "id_colnam", "colnbr",
                    "idtax_n", "idtax_f", "colnam") %in% names(normalized)))
  expect_equal(nrow(normalized), 0L)
  expect_identical(normalized, .empty_retrieved_specimens())

  expect_identical(.normalize_retrieved_specimens(NULL), .empty_retrieved_specimens())
  expect_identical(.normalize_retrieved_specimens("not a table"),
                   .empty_retrieved_specimens())
})

test_that(".normalize_retrieved_specimens() keeps rows and adds missing columns as NA", {
  # collector enrichment failed, so colnam is absent from a non-empty result
  partial <- data.frame(
    id_specimen = 7L,
    id_colnam = 42L,
    colnbr = 3000,
    idtax_n = 500L,
    idtax_f = 500L,
    stringsAsFactors = FALSE
  )

  normalized <- .normalize_retrieved_specimens(partial)

  expect_equal(nrow(normalized), 1L)
  expect_equal(normalized$id_specimen, 7L)
  expect_true("colnam" %in% names(normalized))
  expect_true(is.na(normalized$colnam))
  expect_type(normalized$colnam, "character")
})

test_that("absent specimens are reported, not raised as an error", {
  skip_if_not_installed("shiny")

  testthat::local_mocked_bindings(
    # the shape query_specimens() returns when the criteria match nothing
    query_specimens = function(...) data.frame(),
    .package = "CafriplotsR"
  )

  shiny::testServer(mod_specimen_retriever_server, args = retriever_args(), {
    session$setInputs(retrieve_specimens = 1)

    links <- retrieved_specimens()

    expect_false(is.null(links))
    expect_equal(nrow(links), 2L)
    expect_true(all(links$match_status == "not_found"))
    expect_true(all(is.na(links$id_specimen)))
    expect_true(all(is.na(links$specimen_idtax_n)))
    expect_equal(links$individual_idtax_n, c(500L, 501L))

    # the step stays incomplete, so the wizard cannot advance on nothing
    expect_false(retrieval_complete())
  })
})

test_that("found specimens are linked and the step completes", {
  skip_if_not_installed("shiny")

  testthat::local_mocked_bindings(
    query_specimens = function(id_colnam, number_min, number_max, ...) {
      data.frame(
        id_specimen = 7L,
        id_colnam = id_colnam,
        colnbr = 3000,
        idtax_n = 500L,
        idtax_f = 555L,
        colnam = "Dauby",
        stringsAsFactors = FALSE
      )
    },
    .package = "CafriplotsR"
  )

  shiny::testServer(mod_specimen_retriever_server, args = retriever_args(), {
    session$setInputs(retrieve_specimens = 1)

    links <- retrieved_specimens()

    expect_equal(nrow(links), 2L)
    expect_equal(links$match_status, c("found", "not_found"))
    expect_equal(links$specimen_idtax_n, c(555L, NA_integer_))
    expect_equal(links$individual_idtax_n, c(500L, 501L))
    expect_equal(links$collector_name, c("Dauby", NA_character_))
    expect_true(retrieval_complete())
  })
})

test_that("an unmatched collector stops short of querying, with a warning", {
  skip_if_not_installed("shiny")

  queried <- FALSE
  testthat::local_mocked_bindings(
    query_specimens = function(...) {
      queried <<- TRUE
      data.frame()
    },
    .package = "CafriplotsR"
  )

  # nothing applied in the collector matching step
  args <- retriever_args(matches = list())

  shiny::testServer(mod_specimen_retriever_server, args = args, {
    session$setInputs(retrieve_specimens = 1)

    expect_false(queried)
    expect_false(retrieval_complete())

    links <- retrieved_specimens()
    expect_equal(nrow(links), 2L)
    expect_true(all(links$match_status == "not_found"))
    expect_true(all(is.na(links$id_colnam)))
  })
})
