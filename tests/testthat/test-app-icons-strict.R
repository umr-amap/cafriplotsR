# Guard against positional-argument bugs that misbind a UI element to a shiny
# constructor's `icon` argument. Such bugs are invisible on shiny >= 1.12.0
# (which dropped validateIcon) but break the app on shiny <= 1.11.1.
#
# Each app's UI is built under local_strict_icons() (see helper-shiny.R), which
# enforces shiny <= 1.11.1-style icon validation on every shiny version. A
# misbound `icon` therefore fails here regardless of the developer's installed
# shiny.
#
# Builders are chosen so the UI is constructed eagerly and no app is actually
# run (launch_browser = FALSE / direct appobj return / NULL pools).

app_builders <- list(
  "shiny_app_query_plots" =
    function() shiny_app_query_plots(pool_main = NULL, language = "fr"),
  "shiny_app_taxo_backbone" =
    function() shiny_app_taxo_backbone(pool_taxa = NULL, language = "fr"),
  "app_taxonomic_match" =
    function() CafriplotsR:::app_taxonomic_match(pool_taxa = NULL, language = "fr"),
  "launch_feature_wizard" =
    function() launch_feature_wizard(launch_browser = FALSE, language = "fr"),
  "launch_import_wizard" =
    function() launch_import_wizard(launch_browser = FALSE, language = "fr"),
  "launch_taxa_traits_import" =
    function() launch_taxa_traits_import(launch_browser = FALSE, language = "fr"),
  "launch_individual_specimen_linking_app" =
    function() launch_individual_specimen_linking_app(lang = "fr"),
  "launch_specimen_import_wizard" =
    function() launch_specimen_import_wizard(lang = "fr"),
  "launch_specimen_identification_app" =
    function() launch_specimen_identification_app(lang = "fr"),
  "launch_data_update_app" =
    function() launch_data_update_app(lang = "fr")
)

for (nm in names(app_builders)) {
  local({
    name    <- nm
    builder <- app_builders[[nm]]
    test_that(paste0(name, "() builds its UI with no positional-icon bug"), {
      local_strict_icons()
      expect_s3_class(builder(), "shiny.appobj")
    })
  })
}

# Sanity check: the guard actually fires when a UI element is misbound to
# `icon` (the exact mistake that broke launch_specimen_identification_app on
# shiny <= 1.11.1). This protects the guard itself from silently no-op'ing.
test_that("local_strict_icons() rejects a non-icon bound to `icon`", {
  local_strict_icons()
  expect_error(
    # div falls onto the positional `icon` arg, mirroring the original bug
    shiny::actionLink("id", label = NULL, style = "x", shiny::div("content")),
    "Non-icon bound to the `icon` argument"
  )
})
