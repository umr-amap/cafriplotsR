# Tier 1: construction smoke tests — no browser, no DB required.
# Each test calls the app-builder function and asserts it returns a shiny.appobj
# without error. The UI is evaluated; the server function is stored but not run.
# Catches: broken module signatures, missing exports, i18n load failures, bad
# argument defaults.

# ── shiny_app_query_plots ────────────────────────────────────────────────────

test_that("shiny_app_query_plots() constructs without error", {
  expect_no_warning(
    app <- shiny_app_query_plots(pool_main = NULL, language = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("shiny_app_query_plots() accepts language = 'en'", {
  expect_no_warning(
    app <- shiny_app_query_plots(pool_main = NULL, language = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("shiny_app_query_plots() rejects unsupported language", {
  expect_error(
    shiny_app_query_plots(language = "de"),
    "should be one of"
  )
})

# ── shiny_app_taxo_backbone ──────────────────────────────────────────────────

test_that("shiny_app_taxo_backbone() constructs without error", {
  expect_no_warning(
    app <- shiny_app_taxo_backbone(pool_taxa = NULL, language = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("shiny_app_taxo_backbone() accepts language = 'en'", {
  expect_no_warning(
    app <- shiny_app_taxo_backbone(pool_taxa = NULL, language = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("shiny_app_taxo_backbone() rejects unsupported language", {
  expect_error(
    shiny_app_taxo_backbone(language = "de"),
    "should be one of"
  )
})

# ── app_taxonomic_match (internal builder for launch_taxonomic_match_app) ────

test_that("app_taxonomic_match() constructs without error", {
  expect_no_warning(
    app <- CafriplotsR:::app_taxonomic_match(pool_taxa = NULL, language = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("app_taxonomic_match() accepts language = 'en'", {
  expect_no_warning(
    app <- CafriplotsR:::app_taxonomic_match(pool_taxa = NULL, language = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("app_taxonomic_match() rejects unsupported language", {
  expect_error(
    CafriplotsR:::app_taxonomic_match(language = "de"),
    "should be one of"
  )
})

test_that("app_taxonomic_match() rejects unsupported mode", {
  expect_error(
    CafriplotsR:::app_taxonomic_match(mode = "unknown"),
    "should be one of"
  )
})

# ── launch_feature_wizard ────────────────────────────────────────────────────

test_that("launch_feature_wizard() constructs without error", {
  expect_no_warning(
    app <- launch_feature_wizard(launch_browser = FALSE, language = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_feature_wizard() accepts language = 'en'", {
  expect_no_warning(
    app <- launch_feature_wizard(launch_browser = FALSE, language = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_feature_wizard() rejects unsupported language", {
  expect_error(
    launch_feature_wizard(language = "de"),
    "should be one of"
  )
})

# ── launch_import_wizard ─────────────────────────────────────────────────────

test_that("launch_import_wizard() constructs without error", {
  expect_no_warning(
    app <- launch_import_wizard(launch_browser = FALSE, language = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_import_wizard() accepts language = 'en'", {
  expect_no_warning(
    app <- launch_import_wizard(launch_browser = FALSE, language = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_import_wizard() rejects unsupported language", {
  expect_error(
    launch_import_wizard(language = "de"),
    "should be one of"
  )
})

# ── launch_taxa_traits_import ────────────────────────────────────────────────

test_that("launch_taxa_traits_import() constructs without error", {
  expect_no_warning(
    app <- launch_taxa_traits_import(launch_browser = FALSE, language = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_taxa_traits_import() accepts language = 'en'", {
  expect_no_warning(
    app <- launch_taxa_traits_import(launch_browser = FALSE, language = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_taxa_traits_import() rejects unsupported language", {
  expect_error(
    launch_taxa_traits_import(language = "de"),
    "should be one of"
  )
})

# ── launch_individual_specimen_linking_app ───────────────────────────────────

test_that("launch_individual_specimen_linking_app() constructs without error (fr)", {
  expect_no_warning(
    app <- launch_individual_specimen_linking_app(lang = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_individual_specimen_linking_app() constructs without error (en)", {
  expect_no_warning(
    app <- launch_individual_specimen_linking_app(lang = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

# ── launch_specimen_import_wizard ────────────────────────────────────────────

test_that("launch_specimen_import_wizard() constructs without error (fr)", {
  expect_no_warning(
    app <- launch_specimen_import_wizard(lang = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_specimen_import_wizard() constructs without error (en)", {
  expect_no_warning(
    app <- launch_specimen_import_wizard(lang = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

# ── launch_data_update_app ───────────────────────────────────────────────────

test_that("launch_data_update_app() constructs without error (fr)", {
  expect_no_warning(
    app <- launch_data_update_app(lang = "fr")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_data_update_app() constructs without error (en)", {
  expect_no_warning(
    app <- launch_data_update_app(lang = "en")
  )
  expect_s3_class(app, "shiny.appobj")
})

test_that("launch_data_update_app() rejects unsupported language", {
  expect_error(
    launch_data_update_app(lang = "de"),
    "should be one of"
  )
})
