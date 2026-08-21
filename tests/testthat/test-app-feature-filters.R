# The plot feature filter rows in launch_query_plots_app().
#
# A feature filter is not a fixed input the way country or method is: the user
# picks a feature first and its values second, in as many rows as they need.
# These tests drive the module without a database -- the filter rows are
# assembled from the inputs, and only the value dropdowns need a connection.

test_i18n <- function() {
  shiny.i18n::Translator$new(
    translation_json_path = system.file(
      "translations", "translation.json", package = "CafriplotsR"
    )
  )
}

filters_args <- function() {
  list(pool = shiny::reactive(NULL), i18n = shiny::reactive(test_i18n()))
}

test_that("no feature rows means no feature_filters in the query", {
  # NULL, not an empty list: query_plots() must not see an argument at all.
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    expect_null(filters()$feature_filters)
  })
})

test_that("a feature row becomes the named list query_plots() expects", {
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    session$setInputs(add_feature_filter = 1)
    session$setInputs(
      feature_type_f1  = "data_provider",
      feature_value_f1 = "IRD"
    )

    expect_equal(filters()$feature_filters, list(data_provider = "IRD"))
  })
})

test_that("several values in one row are kept together", {
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    session$setInputs(add_feature_filter = 1)
    session$setInputs(
      feature_type_f1  = "principal_investigator",
      feature_value_f1 = c("Dauby", "Sonke")
    )

    expect_equal(
      filters()$feature_filters,
      list(principal_investigator = c("Dauby", "Sonke"))
    )
  })
})

test_that("two rows on different features become two entries", {
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    session$setInputs(add_feature_filter = 1)
    session$setInputs(add_feature_filter = 2)
    session$setInputs(
      feature_type_f1  = "data_provider",
      feature_value_f1 = "IRD",
      feature_type_f2  = "principal_investigator",
      feature_value_f2 = "Dauby"
    )

    expect_equal(
      filters()$feature_filters,
      list(data_provider = "IRD", principal_investigator = "Dauby")
    )
  })
})

test_that("two rows on the same feature are merged, not repeated", {
  # query_plots() refuses a repeated name in feature_filters, and the user
  # plainly meant "either of these".
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    session$setInputs(add_feature_filter = 1)
    session$setInputs(add_feature_filter = 2)
    session$setInputs(
      feature_type_f1  = "data_provider",
      feature_value_f1 = "IRD",
      feature_type_f2  = "data_provider",
      feature_value_f2 = c("IRD", "CNRS")
    )

    ff <- filters()$feature_filters
    expect_equal(names(ff), "data_provider")
    expect_equal(ff$data_provider, c("IRD", "CNRS"))
  })
})

test_that("a row with no feature or no value is ignored", {
  # A freshly added row is empty until the user fills it in; it must not turn
  # into a filter that matches nothing.
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    session$setInputs(add_feature_filter = 1)
    session$setInputs(feature_type_f1 = "", feature_value_f1 = "IRD")
    expect_null(filters()$feature_filters)

    session$setInputs(feature_type_f1 = "data_provider", feature_value_f1 = NULL)
    expect_null(filters()$feature_filters)

    session$setInputs(feature_value_f1 = c("", "   "))
    expect_null(filters()$feature_filters)
  })
})

test_that("removing a row drops its filter", {
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    session$setInputs(add_feature_filter = 1)
    session$setInputs(
      feature_type_f1  = "data_provider",
      feature_value_f1 = "IRD"
    )
    expect_false(is.null(filters()$feature_filters))

    session$setInputs(remove_feature_filter = "f1")
    expect_null(filters()$feature_filters)
  })
})

# ── the panel itself ─────────────────────────────────────────────────────────

test_that("the panel says so when no feature can be filtered", {
  # No connection, so no feature list: the section must still render.
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    html <- as.character(output$advanced_filters_ui$html)
    expect_match(html, "No filterable plot features available", fixed = TRUE)
  })
})

test_that("the panel offers the features and a way to add a row", {
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    feature_choices(data.frame(
      feature     = c("data_provider", "principal_investigator"),
      valuetype   = c("character", "table_colnam"),
      category    = c("provenance", "people"),
      description = c("", ""),
      stringsAsFactors = FALSE
    ))

    html <- as.character(output$advanced_filters_ui$html)
    expect_match(html, "add_feature_filter", fixed = TRUE)
    expect_match(html, "Plot feature filters", fixed = TRUE)
  })
})

test_that("a row keeps its value when the panel is rebuilt", {
  # The panel is a renderUI: it is rebuilt whenever a row is added or the
  # language changes. Row state is therefore held outside the inputs, and this
  # is the assertion that it is actually restored.
  shiny::testServer(mod_plot_filters_server, args = filters_args(), {
    feature_choices(data.frame(
      feature     = "data_provider",
      valuetype   = "character",
      category    = "provenance",
      description = "",
      stringsAsFactors = FALSE
    ))

    session$setInputs(add_feature_filter = 1)
    session$setInputs(
      feature_type_f1  = "data_provider",
      feature_value_f1 = "IRD"
    )

    # Adding a second row rebuilds the whole panel.
    session$setInputs(add_feature_filter = 2)

    html <- as.character(output$advanced_filters_ui$html)
    expect_match(html, "IRD", fixed = TRUE)
    expect_match(html, "feature_value_f2", fixed = TRUE)

    # And the filter that was already set is still set.
    expect_equal(filters()$feature_filters, list(data_provider = "IRD"))
  })
})

# ── the generated R code ─────────────────────────────────────────────────────

test_that("the code preview reproduces the feature filter, not resolved ids", {
  # The point of passing feature_filters through to query_plots() rather than
  # resolving to plot ids in the app is that this stays runnable and readable.
  shiny::testServer(
    mod_code_preview_server,
    args = list(
      filters = shiny::reactive(list(
        country = "Gabon",
        feature_filters = list(
          data_provider          = "IRD",
          principal_investigator = c("Dauby", "Sonke")
        )
      )),
      selected_plots        = shiny::reactive(integer(0)),
      extraction_options    = shiny::reactive(list()),
      metadata_available    = shiny::reactive(TRUE),
      individuals_available = shiny::reactive(FALSE),
      i18n                  = shiny::reactive(test_i18n())
    ),
    {
      code <- generate_metadata_code(filters())

      expect_match(code, "feature_filters = list(", fixed = TRUE)
      expect_match(code, 'data_provider = "IRD"', fixed = TRUE)
      expect_match(code, 'principal_investigator = c("Dauby", "Sonke")', fixed = TRUE)

      # It has to parse: this is code the user copies and runs.
      expect_silent(parse(text = sub("^# [^\n]*\n", "", code)))
    }
  )
})

test_that("the code preview says nothing when no feature filter is set", {
  shiny::testServer(
    mod_code_preview_server,
    args = list(
      filters               = shiny::reactive(list(country = "Gabon")),
      selected_plots        = shiny::reactive(integer(0)),
      extraction_options    = shiny::reactive(list()),
      metadata_available    = shiny::reactive(TRUE),
      individuals_available = shiny::reactive(FALSE),
      i18n                  = shiny::reactive(test_i18n())
    ),
    {
      expect_false(grepl("feature_filters", generate_metadata_code(filters()), fixed = TRUE))
    }
  )
})

# ── translations ─────────────────────────────────────────────────────────────

test_that("every string the feature filter UI shows is translated", {
  path <- system.file("translations", "translation.json", package = "CafriplotsR")
  tr <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
  en <- vapply(tr$translation, function(x) x$en, character(1))
  fr <- vapply(tr$translation, function(x) x$fr, character(1))

  needed <- c(
    "Plot feature filters",
    "No filterable plot features available",
    "Feature",
    "Choose a feature...",
    "Value(s)",
    "Type or choose a value",
    "Remove this feature filter",
    "Add feature filter"
  )

  expect_true(all(needed %in% en))
  # A French translation identical to the English one is an untranslated entry.
  expect_true(all(fr[match(needed, en)] != needed))
})
