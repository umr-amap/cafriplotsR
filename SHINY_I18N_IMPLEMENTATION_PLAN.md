# Shiny.i18n Implementation Plan for Taxonomic Matching App

## Overview

This plan outlines the migration from the current custom translation system to the `shiny.i18n` package for the taxonomic matching Shiny app.

**Branch:** `feature/shiny-i18n-taxonomic-app`

---

## Current State

### Existing Translation Infrastructure

The app currently has a **custom translation system**:

- **File:** `R/utils_translations.R`
- **Functions:**
  - `get_translations(language)` - Returns nested list with translations
  - `t_(key, language)` - Helper to translate single keys
- **Format:** Hardcoded R lists with "en" and "fr" translations
- **Usage:** Modules receive `language` reactive and call `get_translations(language())`

### What's Already Working

✅ Language toggle module (`mod_language_toggle`)
✅ All UI text translated (English/French)
✅ Reactive language switching throughout app
✅ Comprehensive translation coverage (~140 translation keys)

---

## Migration Goals

1. **Use standard shiny.i18n package** for better maintainability
2. **Keep existing functionality** - no breaking changes to UI/UX
3. **Maintain all existing translations** - migrate all 140+ keys
4. **Improve extensibility** - easier to add new languages in future
5. **External translation file** - JSON file instead of hardcoded R code

---

## Implementation Steps

### Phase 1: Setup (✅ Complete)

**1.1 Create translation.json file**
- [x] Location: `inst/translations/translation.json`
- [x] Format: Standard shiny.i18n JSON format
- [x] Content: All existing translations migrated from `utils_translations.R`

**1.2 Add shiny.i18n dependency**
- [ ] Update `DESCRIPTION` file:
  ```r
  Imports:
      shiny.i18n
  ```
- [ ] Run `devtools::document()` to update NAMESPACE

---

### Phase 2: Core Translation System

**2.1 Create new translation utility**

Create `R/utils_i18n.R` to replace `R/utils_translations.R`:

```r
#' Initialize Translator
#'
#' Creates shiny.i18n Translator object for the app
#'
#' @return shiny.i18n::Translator object
#' @keywords internal
init_translator <- function() {
  translation_file <- system.file(
    "translations/translation.json",
    package = "CafriplotsR"
  )

  if (!file.exists(translation_file)) {
    stop("Translation file not found: ", translation_file)
  }

  translator <- shiny.i18n::Translator$new(translation_json_path = translation_file)
  translator$set_translation_language("en")  # Default language

  return(translator)
}
```

**2.2 Update mod_language_toggle**

Modify `R/mod_language_toggle.R` to work with shiny.i18n:

```r
mod_language_toggle_server <- function(id, initial = "en", translator) {
  shiny::moduleServer(id, function(input, output, session) {

    # Initialize with default language
    current_language <- shiny::reactiveVal(initial)

    # Update translator when language changes
    shiny::observe({
      translator$set_translation_language(current_language())
    })

    shiny::observeEvent(input$language_toggle, {
      new_lang <- if (current_language() == "en") "fr" else "en"
      current_language(new_lang)
    })

    return(current_language)
  })
}
```

---

### Phase 3: Update Main App

**3.1 Modify `app_taxonomic_match()`**

Update `R/shiny_app_taxonomic_match.R`:

```r
app_taxonomic_match <- function(...) {

  # Initialize translator ONCE at app start
  translator <- init_translator()

  ui <- shiny::fluidPage(
    # Add i18n to UI
    shiny.i18n::usei18n(translator),

    # Use translator$t() for static text
    shiny::h1(translator$t("app_title")),

    # For reactive text, use textOutput as before
    shiny::textOutput("app_subtitle")
  )

  server <- function(input, output, session) {

    # Language management - pass translator to module
    current_language <- mod_language_toggle_server(
      "language",
      initial = language,
      translator = translator
    )

    # Reactive translations - translator updates automatically
    output$app_subtitle <- shiny::renderText({
      translator$t("app_subtitle")
    })

    # Pass translator to other modules instead of language reactive
    mod_data_input_server("data_input", translator = translator)
    mod_column_select_server("column_select", translator = translator)
    # ... etc
  }

  shiny::shinyApp(ui, server)
}
```

---

### Phase 4: Update All Modules

Update each module to use `translator` instead of `language` reactive:

**Before (custom system):**
```r
mod_data_input_server <- function(id, language = shiny::reactive("en")) {
  shiny::moduleServer(id, function(input, output, session) {

    t <- shiny::reactive({
      get_translations(language())
    })

    output$title <- shiny::renderText({
      t()$data_input_title
    })
  })
}
```

**After (shiny.i18n):**
```r
mod_data_input_server <- function(id, translator) {
  shiny::moduleServer(id, function(input, output, session) {

    output$title <- shiny::renderText({
      translator$t("data_input_title")
    })
  })
}
```

**Modules to update:**
1. `R/mod_data_input.R`
2. `R/mod_column_select.R`
3. `R/mod_auto_matching.R`
4. `R/mod_progress_tracker.R`
5. `R/mod_name_review.R`
6. `R/mod_fuzzy_suggestions.R`
7. `R/mod_results_export.R`
8. `R/mod_traits_enrichment.R`

---

### Phase 5: Testing & Validation

**5.1 Functional Testing**
- [ ] Launch app: `launch_taxonomic_match_app()`
- [ ] Test language toggle switches between EN/FR
- [ ] Verify all UI elements translate correctly
- [ ] Test each module's translations:
  - Data input
  - Column selection
  - Auto matching
  - Progress tracker
  - Review interface
  - Export options
  - Traits enrichment

**5.2 Edge Cases**
- [ ] Test with provided data vs uploaded file
- [ ] Test all export formats with different languages
- [ ] Verify error messages translate correctly
- [ ] Test with browser language detection (if implemented)

**5.3 Regression Testing**
- [ ] Ensure app functionality unchanged
- [ ] Verify matching algorithm still works
- [ ] Check export file names use correct language

---

### Phase 6: Cleanup & Documentation

**6.1 Remove old system**
- [ ] Delete `R/utils_translations.R` (old custom system)
- [ ] Update any documentation referencing old system
- [ ] Run `devtools::check()` to ensure no broken references

**6.2 Update documentation**
- [ ] Add roxygen docs to `init_translator()`
- [ ] Update `launch_taxonomic_match_app()` documentation
- [ ] Add vignette section on extending translations
- [ ] Document JSON format for future maintainers

**6.3 Version control**
- [ ] Commit with descriptive message
- [ ] Update NEWS.md under "Infrastructure" section
- [ ] Consider adding to package README if notable

---

## Key Design Decisions

### 1. Single Translator Instance

**Decision:** Create ONE translator object at app initialization, pass to all modules

**Rationale:**
- shiny.i18n Translator is stateful - changing language updates all references
- More efficient than creating multiple instances
- Ensures consistent state across all modules

### 2. Translator as Module Parameter

**Decision:** Pass `translator` object to module servers (not as reactive)

**Rationale:**
- Translator itself is reactive - methods like `t()` automatically update
- Cleaner API than passing `language` reactive and recreating translator
- Aligns with shiny.i18n best practices

### 3. Keep JSON External

**Decision:** Store translations in `inst/translations/` (not in R code)

**Rationale:**
- Easier for non-R users to update translations
- Standard practice for i18n systems
- Can use translation management tools
- Reduces code complexity

---

## Future Enhancements (Optional)

### Additional Languages

To add a new language (e.g., Spanish):

1. Update `translation.json`:
   ```json
   {
     "languages": ["en", "fr", "es"],
     "translation": {
       "app_title": {
         "en": "...",
         "fr": "...",
         "es": "Título en español"
       }
     }
   }
   ```

2. Update language toggle UI to show 3 options

### Browser Language Detection

Add automatic language detection:

```r
server <- function(input, output, session) {
  # Detect browser language
  browser_lang <- shiny::reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)
    query$lang %||% "en"  # Default to English
  })

  # Initialize with browser language
  translator$set_translation_language(browser_lang())
}
```

### Translation Validation Script

Create `data-raw/validate_translations.R`:

```r
# Ensure all keys have translations in all languages
library(jsonlite)

trans <- jsonlite::read_json("inst/translations/translation.json")
languages <- trans$languages
translation <- trans$translation

# Check each key has all languages
for (key in names(translation)) {
  for (lang in languages) {
    if (is.null(translation[[key]][[lang]])) {
      warning("Missing ", lang, " translation for: ", key)
    }
  }
}
```

---

## Testing Checklist

### Before Starting Development
- [x] Branch created: `feature/shiny-i18n-taxonomic-app`
- [x] Translation JSON file created with correct format
- [ ] shiny.i18n package installed: `install.packages("shiny.i18n")`

### During Development
- [ ] Each module tested individually after updating
- [ ] No breaking changes to existing functionality
- [ ] All translation keys accessible via `translator$t(key)`

### Before Merging
- [ ] Full app test in both languages
- [ ] `devtools::check()` passes with no errors
- [ ] NEWS.md updated
- [ ] Old translation system removed
- [ ] Documentation updated

---

## Rollback Plan

If issues arise during migration:

1. **Keep old system temporarily:** Don't delete `utils_translations.R` until fully tested
2. **Feature flag:** Add parameter to `launch_taxonomic_match_app(use_i18n = TRUE)` to toggle between systems
3. **Git revert:** Branch can be abandoned if migration fails

---

## Estimated Effort

- **Phase 1 (Setup):** ✅ Complete (1 hour)
- **Phase 2 (Core System):** 2-3 hours
- **Phase 3 (Main App):** 1-2 hours
- **Phase 4 (Modules):** 3-4 hours (8 modules × ~30 min each)
- **Phase 5 (Testing):** 2-3 hours
- **Phase 6 (Cleanup):** 1 hour

**Total:** ~10-15 hours

---

## Questions / Decisions Needed

1. **Should we keep old system temporarily?** (Recommended: Yes, until fully tested)
2. **Add browser language detection?** (Optional enhancement)
3. **Add validation script?** (Recommended for long-term maintenance)
4. **Plan for additional languages?** (Consider in design even if not implementing now)

---

## References

- shiny.i18n documentation: https://github.com/Appsilon/shiny.i18n
- shiny.i18n examples: https://github.com/Appsilon/shiny.i18n/tree/master/examples
- JSON format spec: https://github.com/Appsilon/shiny.i18n#translation-json-file
