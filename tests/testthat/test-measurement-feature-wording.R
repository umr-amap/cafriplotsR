# The measurement step calls what it collects a "feature", not a "trait"
#
# What this step records is whatever is measured on a stem: a diameter, yes,
# but also position_x, a quadrat, a growth form. "Trait" is too narrow for
# half of them, and the rest of the wizard already says "feature" (Add Plot
# Features, the feature catalog, query_individual_features()). The wording is
# pinned here because it is spread across two panels and two languages, and a
# single missed string reads as a different concept to the user.
#
# Note the one word that must NOT change: `features_field` is a distinct thing
# in this module — metadata attached to a measurement — so its radio label
# stays "Measurement metadata" and never collapses into "feature".

fake_i18n <- list(t = function(x) x)

render_mapping_panels <- function() {
  ns <- shiny::NS("m")
  traits <- data.frame(
    trait = c("stem_diameter", "position_x"),
    valuetype = "numeric", stringsAsFactors = FALSE
  )

  wide <- .render_wide_trait_mapping_ui(
    ns, fake_i18n,
    data.frame(plot_name = "P1", tag = 1, stem_diameter = 12.3,
               position_x = 4.5, stringsAsFactors = FALSE),
    traits,
    list(map_plot_name = "plot_name", map_tag = "tag")
  )
  long <- .render_long_trait_mapping_ui(
    ns, fake_i18n,
    data.frame(plot_name = "P1", tag = 1,
               trait_type = c("stem_diameter", "position_x"),
               value = c(12.3, 4.5), stringsAsFactors = FALSE),
    traits,
    list(map_plot_name = "plot_name", map_tag = "tag",
         map_trait_type = "trait_type")
  )

  paste(
    as.character(htmltools::renderTags(wide)$html),
    as.character(htmltools::renderTags(long)$html)
  )
}

test_that("the mapping panels ask for features, not traits", {
  html <- render_mapping_panels()

  expect_true(grepl("Map Columns to Features", html, fixed = TRUE))
  expect_true(grepl("Map Feature Names", html, fixed = TRUE))
  expect_true(grepl("Map each data column to a feature from the database",
                    html, fixed = TRUE))
  expect_true(grepl("Map each feature name from your file to a feature",
                    html, fixed = TRUE))
  expect_true(grepl("(Skip this feature)", html, fixed = TRUE))
})

test_that("no user-facing label in the mapping panels still says Trait", {
  html <- render_mapping_panels()

  # Lowercase `trait` survives legitimately in element ids (trait_map_*,
  # map_trait_type) and in the sample data's own column name, so only the
  # capitalised label form is asserted against.
  expect_false(grepl("Trait", html, fixed = TRUE))
})

test_that("measurement metadata keeps its own name", {
  # `features_field` is measurement metadata, not a measured feature. If this
  # label ever became "feature" the two roles in the same radio group would be
  # indistinguishable.
  html <- render_mapping_panels()

  expect_true(grepl("Measured feature", html, fixed = TRUE))
  expect_true(grepl("Measurement metadata", html, fixed = TRUE))
})

test_that("the reworded strings are translated into French", {
  translation_file <- system.file("translations", "translation.json",
                                  package = "CafriplotsR")
  skip_if(!nzchar(translation_file) || !file.exists(translation_file),
          "translation file not installed")
  skip_if_not_installed("jsonlite")

  tr <- jsonlite::fromJSON(translation_file, simplifyDataFrame = FALSE)
  en <- vapply(tr$translation,
               function(x) if (is.null(x$en)) "" else x$en, character(1))

  reworded <- c(
    "Map Columns to Features",
    "Map Feature Names",
    "Measured feature",
    "Feature type / name column *",
    "Wide format (one column per feature)",
    "Long format (feature type + value columns)",
    "No feature names mapped. Please map at least one feature.",
    "Please map the feature type column."
  )

  for (s in reworded) {
    i <- which(en == s)
    expect_length(i, 1)
    fr <- tr$translation[[i[1]]]$fr
    expect_true(nzchar(fr), info = s)
    expect_false(identical(fr, s), info = s)
  }
})

test_that("the replaced trait wording is gone from the translation file", {
  translation_file <- system.file("translations", "translation.json",
                                  package = "CafriplotsR")
  skip_if(!nzchar(translation_file) || !file.exists(translation_file),
          "translation file not installed")
  skip_if_not_installed("jsonlite")

  tr <- jsonlite::fromJSON(translation_file, simplifyDataFrame = FALSE)
  en <- vapply(tr$translation,
               function(x) if (is.null(x$en)) "" else x$en, character(1))

  # "Trait data" is deliberately absent from this list: other modules still
  # use it.
  expect_false("Map Trait Names" %in% en)
  expect_false("Map Columns to Traits" %in% en)
  expect_false("Trait type / name column *" %in% en)
  expect_false("Metadata (features_field)" %in% en)
})
