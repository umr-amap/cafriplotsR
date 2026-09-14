# The taxa traits import accepts long-format tables (one row per measurement,
# a trait-name column and a value column). Step 2 spreads them into one column
# per trait so the validation, preview and import steps work unchanged. These
# tests pin that reshaping and the step 2 module that drives it.

.tt_i18n <- function() {
  shiny::reactive(list(t = function(x) x))
}

.tt_traitlist <- function() {
  data.frame(
    id_trait = 1:3,
    trait = c("wood_density", "max_height", "leaf_phenology"),
    valuetype = c("numeric", "numeric", "categorical"),
    traitdescription = NA_character_, category = NA_character_,
    expectedunit = NA_character_, minallowedvalue = NA_real_,
    maxallowedvalue = NA_real_, factorlevels = NA_character_,
    stringsAsFactors = FALSE
  )
}

.tt_pool <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "traitlist", .tt_traitlist())
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

.tt_long <- function() {
  data.frame(
    idtax_n = c(10, 10, 11, 12, 12),
    trait = c("wood_density", "leaf_phenology", "wood_density",
              "max_height", "not_a_trait"),
    value = c(0.61, NA, 0.55, 35, 1),
    value_char = c(NA, "deciduous", NA, NA, NA),
    reference = c("A", "A", "B", "C", "C"),
    stringsAsFactors = FALSE
  )
}


test_that(".long_traits_to_wide() puts each value in its trait column", {
  name_map <- c(wood_density = "wood_density", leaf_phenology = "leaf_phenology",
                max_height = "max_height", not_a_trait = "")
  vt <- stats::setNames(.tt_traitlist()$valuetype, .tt_traitlist()$trait)

  out <- .long_traits_to_wide(.tt_long(), "trait", "value", "value_char",
                              name_map, vt)

  # The unmapped name is dropped, the key columns are gone, metadata stays
  expect_equal(nrow(out), 4L)
  expect_setequal(names(out), c("idtax_n", "reference", "wood_density",
                                "leaf_phenology", "max_height"))
  expect_equal(out$wood_density, c(0.61, NA, 0.55, NA))
  expect_equal(out$max_height, c(NA, NA, NA, 35))
  # Categorical traits read the character column
  expect_equal(out$leaf_phenology, c(NA, "deciduous", NA, NA))
  expect_type(out$wood_density, "double")
  expect_equal(out$reference, c("A", "A", "B", "C"))
})

test_that(".long_traits_to_wide() merges names mapped to the same trait", {
  df <- data.frame(idtax_n = 1:3, trait = c("WD", "wood density", "WD"),
                   value = c(0.5, 0.6, 0.7))
  name_map <- c(WD = "wood_density", `wood density` = "wood_density")

  out <- .long_traits_to_wide(df, "trait", "value", NULL, name_map,
                              c(wood_density = "numeric"))

  expect_equal(out$wood_density, c(0.5, 0.6, 0.7))
})

test_that(".long_traits_to_wide() works with a single, mixed value column", {
  df <- data.frame(idtax_n = 1:2, trait = c("wood_density", "leaf_phenology"),
                   value = c("0.5", "evergreen"))
  name_map <- c(wood_density = "wood_density", leaf_phenology = "leaf_phenology")

  out <- .long_traits_to_wide(df, "trait", "", "value", name_map,
                              c(wood_density = "numeric",
                                leaf_phenology = "categorical"))

  expect_equal(out$wood_density, c("0.5", NA))
  expect_equal(out$leaf_phenology, c(NA, "evergreen"))
})

test_that(".long_traits_to_wide() requires a value column", {
  expect_error(
    .long_traits_to_wide(.tt_long(), "trait", "", NULL, c(wood_density = "wood_density")),
    "value column"
  )
})

test_that(".coalesce_values() fills gaps and treats blanks as missing", {
  expect_equal(.coalesce_values(c(1, NA, 3), c(9, 2, 9)), c(1, 2, 3))
  expect_equal(.coalesce_values(c(1, NA), c(NA, "x")), c("1", "x"))
  expect_equal(.coalesce_values(c("a", " "), c(NA, "b")), c("a", "b"))
  expect_equal(.coalesce_values(NULL, c(1, 2)), c(1, 2))
  expect_equal(.coalesce_values(c(NA, NA), c(1, 2)), c(1, 2))
  expect_null(.coalesce_values(NULL, NULL))
})

test_that(".guess_long_trait_columns() recognises usual column names", {
  g <- .guess_long_trait_columns(c("idtax_n", "Trait", "Value", "value_char"))
  expect_equal(g, list(name = "Trait", value_num = "Value", value_char = "value_char"))

  g <- .guess_long_trait_columns(c("idtax_n", "wood_density"))
  expect_equal(g, list(name = "", value_num = "", value_char = ""))
})


test_that("step 2 detects a long table and returns it spread by trait", {
  con <- .tt_pool()

  shiny::testServer(
    mod_trait_column_mapping_server,
    args = list(data = shiny::reactive(.tt_long()),
                pool = shiny::reactive(con), i18n = .tt_i18n()),
    {
      expect_equal(data_format(), "long")
      expect_equal(long_cols()$name, "trait")
      expect_equal(long_cols()$value_num, "value")
      expect_equal(long_cols()$value_char, "value_char")

      mr <- mapping_rows()
      expect_equal(mr$key, sort(unique(.tt_long()$trait)))
      # Exact names are auto-mapped
      auto <- stats::setNames(mr$auto, mr$key)
      expect_equal(unname(auto["wood_density"]), "trait:wood_density")

      # Map as the rendered selectize inputs would
      ids <- stats::setNames(mr$input_id, mr$key)
      do.call(session$setInputs, stats::setNames(
        as.list(c("trait:leaf_phenology", "trait:max_height", "", "trait:wood_density")),
        ids[c("leaf_phenology", "max_height", "not_a_trait", "wood_density")]
      ))

      res <- session$returned()
      expect_true(res$valid)
      expect_equal(res$format, "long")
      expect_setequal(unname(res$trait_cols),
                      c("wood_density", "leaf_phenology", "max_height"))
      expect_equal(names(res$trait_cols), unname(res$trait_cols))
      expect_equal(nrow(res$data), 4L)
      expect_false(any(c("trait", "value", "value_char") %in% names(res$data)))
    }
  )
})

test_that("step 2 long format reports a missing value column", {
  con <- .tt_pool()

  shiny::testServer(
    mod_trait_column_mapping_server,
    args = list(data = shiny::reactive(.tt_long()),
                pool = shiny::reactive(con), i18n = .tt_i18n()),
    {
      session$setInputs(data_format = "long", long_name_col = "trait",
                        long_value_num_col = "", long_value_char_col = "")
      expect_false(session$returned()$valid)
      expect_true("Select at least one value column" %in% mapping_state()$errors)
    }
  )
})

test_that("step 2 long format refuses a trait that would overwrite a column", {
  con <- .tt_pool()
  df <- .tt_long()
  df$wood_density <- "kept metadata"

  shiny::testServer(
    mod_trait_column_mapping_server,
    args = list(data = shiny::reactive(df),
                pool = shiny::reactive(con), i18n = .tt_i18n()),
    {
      ids <- stats::setNames(mapping_rows()$input_id, mapping_rows()$key)
      do.call(session$setInputs, stats::setNames(list("trait:wood_density"),
                                           ids["wood_density"]))
      expect_false(session$returned()$valid)
      expect_true(any(grepl("wood_density", mapping_state()$errors)))
    }
  )
})

test_that("step 2 keeps the wide behaviour for one column per trait", {
  con <- .tt_pool()
  df <- data.frame(idtax_n = 1:2, wood_density = c(0.5, 0.6),
                   max_height = c(30, 40))

  shiny::testServer(
    mod_trait_column_mapping_server,
    args = list(data = shiny::reactive(df),
                pool = shiny::reactive(con), i18n = .tt_i18n()),
    {
      expect_equal(data_format(), "wide")
      ids <- stats::setNames(mapping_rows()$input_id, mapping_rows()$key)
      do.call(session$setInputs, stats::setNames(
        list("", "trait:wood_density", "trait:max_height"), ids))

      res <- session$returned()
      expect_true(res$valid)
      expect_equal(res$format, "wide")
      expect_null(res$data)
      expect_equal(res$trait_cols,
                   c(wood_density = "wood_density", max_height = "max_height"))
    }
  )
})


.tt_spread <- function() {
  tl <- .tt_traitlist()
  .long_traits_to_wide(
    .tt_long(), "trait", "value", "value_char",
    c(wood_density = "wood_density", leaf_phenology = "leaf_phenology",
      max_height = "max_height", not_a_trait = ""),
    stats::setNames(tl$valuetype, tl$trait)
  )
}

.tt_mapping <- function(format) {
  list(valid = TRUE, format = format, idtax_col = "idtax_n",
       trait_cols = c(wood_density = "wood_density",
                      leaf_phenology = "leaf_phenology",
                      max_height = "max_height"),
       metadata_cols = c(reference = "reference"), feature_cols = NULL,
       available_traits = .tt_traitlist())
}

test_that("validation does not flag spread long data as mostly missing", {
  run <- function(format) {
    result <- NULL
    shiny::testServer(
      mod_trait_validation_server,
      args = list(data = shiny::reactive(.tt_spread()),
                  mapping = shiny::reactive(.tt_mapping(format)),
                  pool = shiny::reactive(NULL), i18n = .tt_i18n()),
      {
        session$setInputs(run_validation = 1)
        result <<- session$returned()
      }
    )
    result
  }

  long <- run("long")
  expect_true(long$valid)
  expect_false("high_na" %in% long$warnings$check)

  # The same table uploaded wide still gets the warning
  expect_true("high_na" %in% run("wide")$warnings$check)
})

test_that("the import receives one column per trait from a long upload", {
  captured <- NULL
  local_mocked_bindings(add_sp_traits_measures = function(...) {
    captured <<- list(...)
    list(list_traits_add = list(data.frame()))
  })

  .execute_trait_import(.tt_spread(), .tt_mapping("long"), pool = NULL,
                        basis_resolved = "traitDatabase")

  expect_setequal(captured$traits_field,
                  c("wood_density", "leaf_phenology", "max_height"))
  expect_equal(captured$idtax, "idtax_n")
  nd <- captured$new_data
  # One non-missing trait value per row, as add_sp_traits_measures() expects
  per_row <- rowSums(!is.na(nd[, captured$traits_field]))
  expect_equal(unname(per_row), rep(1, nrow(nd)))
})


.tt_state <- function(...) {
  state <- list(trait_mapping = NULL, metadata_mapping = NULL,
                validation = NULL, import = NULL)
  utils::modifyList(state, list(...))
}

test_that(".wizard_state_update() stores a valid result and drops later steps", {
  state <- .tt_state(trait_mapping = list(valid = TRUE, format = "long"),
                     metadata_mapping = list(valid = TRUE),
                     validation = list(valid = TRUE), import = list(done = TRUE))
  res <- list(valid = TRUE, format = "wide")

  out <- .wizard_state_update(state, "trait_mapping", res, showing = TRUE)

  expect_equal(out$trait_mapping, res)
  expect_null(out$metadata_mapping)
  expect_null(out$validation)
  expect_null(out$import)
  # The entries are emptied, not removed
  expect_equal(names(out), names(state))
})

test_that(".wizard_state_update() clears the step the user is looking at", {
  state <- .tt_state(trait_mapping = list(valid = TRUE, format = "long"),
                     validation = list(valid = TRUE))

  out <- .wizard_state_update(state, "trait_mapping", list(valid = FALSE),
                              showing = TRUE)

  expect_null(out$trait_mapping)
  expect_null(out$validation)
})

test_that(".wizard_state_update() ignores an invalid result from a step left behind", {
  state <- .tt_state(trait_mapping = list(valid = TRUE, format = "wide"),
                     validation = list(valid = TRUE))

  out <- .wizard_state_update(state, "trait_mapping", list(valid = FALSE),
                              showing = FALSE)

  expect_equal(out, state)
})

test_that(".wizard_state_update() keeps later steps when the result is unchanged", {
  res <- list(valid = TRUE, format = "wide")
  state <- .tt_state(trait_mapping = res, validation = list(valid = TRUE))

  out <- .wizard_state_update(state, "trait_mapping", res, showing = TRUE)

  expect_equal(out, state)
})

test_that(".wizard_state_update() clears only the steps after the one given", {
  state <- .tt_state(trait_mapping = list(valid = TRUE),
                     metadata_mapping = list(valid = TRUE),
                     validation = list(valid = TRUE))

  out <- .wizard_state_update(state, "metadata_mapping",
                              list(valid = TRUE, idtax_col = "idtax_n"),
                              showing = TRUE)

  expect_equal(out$trait_mapping, state$trait_mapping)
  expect_equal(out$metadata_mapping$idtax_col, "idtax_n")
  expect_null(out$validation)
})

test_that("switching step 2 from long to wide cannot carry the long state forward", {
  long <- list(valid = TRUE, format = "long", data = .tt_spread())
  state <- .tt_state(trait_mapping = long,
                     metadata_mapping = list(valid = TRUE),
                     validation = list(valid = TRUE, cleaned_data = .tt_spread()))

  # Switching the format empties the mapping until the columns are mapped
  state <- .wizard_state_update(state, "trait_mapping", list(valid = FALSE),
                                showing = TRUE)
  expect_null(state$trait_mapping)      # Next is disabled
  expect_null(state$validation)         # the long cleaned table is gone

  wide <- list(valid = TRUE, format = "wide", data = NULL)
  state <- .wizard_state_update(state, "trait_mapping", wide, showing = TRUE)
  expect_equal(state$trait_mapping, wide)
  expect_null(state$metadata_mapping)
})


test_that("the sheet selector keeps the chosen sheet when it is redrawn", {
  testthat::skip_if_not_installed("openxlsx")
  path <- withr::local_tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(list(readme = data.frame(a = 1),
                           traits = data.frame(idtax_n = 1:3)), path)

  server <- function(input, output, session) {
    table <- .xlsx_sheet_server(input, output, session, "file_upload", NULL)
    read <- shiny::reactiveVal(NULL)
    shiny::observeEvent(table(), read(table()))
  }

  shiny::testServer(server, {
    session$setInputs(file_upload = data.frame(name = "x.xlsx", datapath = path))
    session$setInputs(file_upload_sheet = "readme")
    expect_equal(names(read()), "a")

    session$setInputs(file_upload_sheet = "traits")
    expect_equal(names(read()), "idtax_n")

    # The wizard redraws this from the last rendered HTML
    html <- as.character(output$file_upload_sheet_ui$html)
    expect_match(html, '<option value="traits" selected>', fixed = TRUE)
  })
})
