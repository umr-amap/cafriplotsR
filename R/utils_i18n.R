# Internationalization (i18n) Utilities
#
# Provides translation support using shiny.i18n package for the taxonomic matching Shiny app.

#' Initialize Translator for Shiny App
#'
#' Creates a shiny.i18n Translator object loaded with translations from the JSON file.
#' This should be called once at app initialization.
#'
#' @return shiny.i18n::Translator object
#'
#' @details
#' The translator reads from `inst/translations/translation.json` which contains
#' translations for English (en) and French (fr).
#'
#' @examples
#' \dontrun{
#' translator <- init_translator()
#' translator$set_translation_language("fr")
#' translator$t("app_title")
#' }
#'
#' @keywords internal
#' @export
init_translator <- function() {

  # Locate translation file in package installation
  translation_file <- system.file(
    "translations/translation.json",
    package = "CafriplotsR"
  )

  # Fallback for development (when package not installed)
  if (!file.exists(translation_file) || translation_file == "") {
    # Try relative path from package root
    translation_file <- file.path("inst/translations/translation.json")

    if (!file.exists(translation_file)) {
      cli::cli_alert_danger(
        "Translation file not found. Expected location: inst/translations/translation.json"
      )
      stop("Translation file not found", call. = FALSE)
    }
  }

  cli::cli_alert_info("Loading translations from: {translation_file}")

  # Create translator
  translator <- shiny.i18n::Translator$new(translation_json_path = translation_file)

  # Set default language
  translator$set_translation_language("fr")

  # Verify languages loaded
  available_langs <- translator$get_languages()
  cli::cli_alert_success("Translator initialized with languages: {paste(available_langs, collapse = ', ')}")

  return(translator)
}


#' Create Reactive Translator
#'
#' Creates a reactive expression that returns the translator after setting the language.
#' This is the recommended pattern for using shiny.i18n with reactive language switching.
#'
#' @param translator shiny.i18n::Translator object (from init_translator())
#' @param language_reactive Reactive expression returning current language code ("en" or "fr")
#'
#' @return Reactive expression returning the translator
#'
#' @keywords internal
#' @export
create_reactive_translator <- function(translator, language_reactive) {

  shiny::reactive({
    selected <- language_reactive()

    # Validate language is available
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    } else {
      cli::cli_alert_warning(
        "Invalid language '{selected}'. Available: {paste(translator$get_languages(), collapse = ', ')}"
      )
    }

    translator
  })
}


#' Get Available Translation Languages
#'
#' Helper to get list of available languages from translator
#'
#' @param translator shiny.i18n::Translator object
#'
#' @return Character vector of language codes
#'
#' @keywords internal
#' @export
get_available_languages <- function(translator) {
  translator$get_languages()
}
