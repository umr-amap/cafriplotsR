# Tests for measurement metadata columns in the feature wizard
#
# A long-format import set height_of_stem_diameter as features_field, yet the
# diameters reached the database without it. The role was lost in step 3: the
# mapping block is redrawn when a key column or the feature list changes, and
# was redrawn with its defaults. Step 6 then wrote what it was given, silently.
# Both the redraw and the warning before the write are pinned down here.

fake_i18n <- list(t = function(x) x)

feature_traits <- data.frame(
  stringsAsFactors = FALSE,
  id_trait  = 1:3,
  trait     = c("stem_diameter", "basal_stem_diameter", "height_of_stem_diameter"),
  valuetype = "numeric"
)

long_raw <- data.frame(
  stringsAsFactors = FALSE,
  plot_name  = "P1",
  tag        = c(1, 1, 2),
  trait_type = c("stem_diameter", "basal_stem_diameter", "stem_diameter"),
  traitvalue = c(11.8, 6.8, 12.3),
  height_of_stem_diameter = c(1.3, 0.3, 1.3)
)

long_input <- function(...) {
  c(list(map_plot_name = "plot_name", map_tag = "tag",
         map_trait_type = "trait_type", map_value_num = "traitvalue",
         map_value_char = ""), list(...))
}

checked_value <- function(html, id) {
  # The value of the radio button rendered as checked for input `id`
  m <- regmatches(html, regexpr(
    sprintf('<input type="radio" name="%s" value="[^"]*" checked', id), html))
  sub('.*value="([^"]*)".*', "\\1", m)
}

test_that(".kept_choice falls back to the default on a first draw", {
  expect_equal(.kept_choice(list(), "x", c("meta", "feature"), "meta"), "meta")
})

test_that(".kept_choice keeps what the user chose", {
  input <- list(x = "feature")
  expect_equal(.kept_choice(input, "x", c("meta", "feature"), "meta"), "feature")
})

test_that(".kept_choice keeps a dropdown deliberately set to skip", {
  # "" is how a feature dropdown records "skip this", and an auto-match must
  # not put the feature back
  expect_equal(
    .kept_choice(list(x = ""), "x", c(a = "stem_diameter"), "stem_diameter"),
    ""
  )
})

test_that(".kept_choice drops a value that is no longer offered", {
  grouped <- list(Size = c(d = "stem_diameter"), Other = c(q = "quadrat"))
  expect_equal(.kept_choice(list(x = "quadrat"), "x", grouped, ""), "quadrat")
  expect_equal(.kept_choice(list(x = "gone"), "x", grouped, ""), "")
})

test_that("a redrawn long mapping keeps a column set as features_field", {
  html <- as.character(.render_long_trait_mapping_ui(
    shiny::NS("m"), fake_i18n, long_raw, feature_traits,
    long_input(meta_role_height_of_stem_diameter = "feature")
  ))

  expect_equal(checked_value(html, "m-meta_role_height_of_stem_diameter"), "feature")
})

test_that("a long mapping drawn for the first time leaves extra columns out", {
  html <- as.character(.render_long_trait_mapping_ui(
    shiny::NS("m"), fake_i18n, long_raw, feature_traits, long_input()
  ))

  expect_equal(checked_value(html, "m-meta_role_height_of_stem_diameter"), "meta")
  # and says so, rather than calling it metadata that is kept
  expect_match(html, "Ignore this column", fixed = TRUE)
  expect_match(html, "Named like a database feature", fixed = TRUE)
})

test_that("a redrawn long mapping keeps a feature mapped by hand", {
  html <- as.character(.render_long_trait_mapping_ui(
    shiny::NS("m"), fake_i18n, long_raw, feature_traits,
    long_input(trait_map_stem_diameter = "basal_stem_diameter")
  ))

  select <- regmatches(html, regexpr(
    '(?s)<select[^>]*id="m-trait_map_stem_diameter".*?</select>', html, perl = TRUE))
  expect_match(select, '<option value="basal_stem_diameter" selected>', fixed = TRUE)
})

test_that("a redrawn wide mapping keeps a column set as measurement metadata", {
  wide_raw <- data.frame(
    plot_name = "P1", tag = 1, stem_diameter = 11.8, height_of_stem_diameter = 1.3
  )

  html <- as.character(.render_wide_trait_mapping_ui(
    shiny::NS("m"), fake_i18n, wide_raw, feature_traits,
    list(map_plot_name = "plot_name", map_tag = "tag",
         col_role_height_of_stem_diameter = "feature")
  ))

  expect_equal(checked_value(html, "m-col_role_height_of_stem_diameter"), "feature")
  expect_equal(checked_value(html, "m-col_role_stem_diameter"), "trait")
})

test_that("the long mapping passes a features_field column on to the import", {
  df <- long_raw
  df$id_liste_plots <- 1L

  out <- .apply_long_mapping(
    df, feature_traits, census_map = NULL,
    input = long_input(meta_role_height_of_stem_diameter = "feature"),
    ns = shiny::NS("m"), i18n = fake_i18n
  )

  expect_equal(out$config$features_field, "height_of_stem_diameter")
  expect_equal(out$data$height_of_stem_diameter, c(1.3, 0.3, 1.3))
})

# .check_feature_columns ---------------------------------------------------

traitlist_con <- function() {
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWriteTable(con, "traitlist", feature_traits)
  con
}

test_that(".check_feature_columns names a column named like a feature but left out", {
  con <- traitlist_con()
  on.exit(DBI::dbDisconnect(con))

  config <- list(metadata_columns = c("height_of_stem_diameter", "remark"),
                 features_field = NULL)

  out <- .check_feature_columns(config, con)

  expect_equal(out$left_out, "height_of_stem_diameter")
  expect_length(out$missing, 0)
})

test_that(".check_feature_columns names a features_field column absent from traitlist", {
  con <- traitlist_con()
  on.exit(DBI::dbDisconnect(con))

  config <- list(metadata_columns = c("height_of_stem_diameter", "pom"),
                 features_field = c("height_of_stem_diameter", "pom"))

  out <- .check_feature_columns(config, con)

  expect_equal(out$missing, "pom")
  expect_length(out$left_out, 0)
})

test_that(".check_feature_columns looks up the mapped feature in wide format", {
  con <- traitlist_con()
  on.exit(DBI::dbDisconnect(con))

  config <- list(features_field = "POM",
                 features_field_mappings = list(POM = "height_of_stem_diameter"))

  expect_length(.check_feature_columns(config, con)$missing, 0)
})

test_that(".check_feature_columns stays quiet when traitlist cannot be read", {
  con <- traitlist_con()
  DBI::dbDisconnect(con)

  out <- suppressMessages(.check_feature_columns(
    list(metadata_columns = "height_of_stem_diameter"), con))

  expect_length(out$missing, 0)
  expect_length(out$left_out, 0)
})

# Step 3 module ------------------------------------------------------------

test_that("changing a role after Apply withdraws the prepared data", {
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("writexl")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "traitlist", transform(
    feature_traits, traitdescription = NA_character_, category = NA_character_,
    expectedunit = NA_character_, minallowedvalue = NA_real_,
    maxallowedvalue = NA_real_, factorlevels = NA_character_
  ))

  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(long_raw, path)

  shiny::testServer(
    mod_feat_step3_measurements_server,
    args = list(
      selected_plots = shiny::reactive(data.frame(plot_name = "P1", id_liste_plots = 1L)),
      operation_mode = shiny::reactive("add_measurements"),
      con = shiny::reactive(con),
      i18n = shiny::reactive(fake_i18n)
    ),
    {
      session$setInputs(
        xlsx_file = data.frame(name = "m.xlsx", datapath = path,
                               stringsAsFactors = FALSE),
        xlsx_file_sheet = "Sheet1",
        data_format = "long"
      )
      session$setInputs(!!!long_input(
        trait_map_stem_diameter = "stem_diameter",
        trait_map_basal_stem_diameter = "basal_stem_diameter",
        meta_role_height_of_stem_diameter = "meta"
      ))
      session$setInputs(apply_mapping = 1)
      expect_false(is.null(session$returned()))
      expect_null(session$returned()$config$features_field)

      # The user notices and switches the column after Apply
      session$setInputs(meta_role_height_of_stem_diameter = "feature")
      expect_null(session$returned())

      session$setInputs(apply_mapping = 2)
      expect_equal(session$returned()$config$features_field,
                   "height_of_stem_diameter")
    }
  )
})
