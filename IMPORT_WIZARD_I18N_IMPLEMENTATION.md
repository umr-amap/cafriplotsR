# Import Wizard Internationalization Implementation Guide

## Summary

This document describes how to add i18n support to the Import Wizard Shiny app, following the existing pattern from the Taxonomic Match app.

## Completed Steps

### 1. Translation File Updates ✓

- **File**: `inst/translations/translation.json`
- **Status**: Updated with 243 new translation entries (549 total entries now)
- **Duplicates removed**: 2 (were already in the main translation file)
- **Languages**: English (en) and French (fr)

### 2. Helper Files ✓

The following helper files already exist and don't need modification:
- `R/utils_i18n.R` - Contains `init_translator()` and `create_reactive_translator()`
- These functions are used by both apps

## Implementation Steps Required

You need to update the following files to add i18n support:

### Step 1: Main Import Wizard App (`R/shiny_app_import_wizard.R`)

Add the following changes:

#### At the top of `import_wizard_ui()` function (before `shiny::fluidPage`):
```r
#' @param language Character, initial language ("en" or "fr"), default: "fr"
```

#### Inside `import_wizard_ui()` function (before the `shiny::fluidPage` call):
```r
# Initialize translator (must be before UI for usei18n)
translator <- init_translator()
```

#### Inside `shiny::fluidPage()` (at the very top, before shinyjs):
```r
# Add shiny.i18n (required for automatic translation)
shiny.i18n::usei18n(translator),
```

#### Add language toggle in header (after the main title div):
```r
# Language toggle (top right)
shiny::absolutePanel(
  top = 10,
  right = 20,
  fixed = TRUE,
  draggable = FALSE,
  style = "z-index: 1000;",
  shiny::radioButtons(
    inputId = "selected_language",
    label = NULL,
    choices = c("EN" = "en", "FR" = "fr"),
    selected = "fr",
    inline = TRUE
  )
),
```

#### In `import_wizard_server()` function, add after authentication setup:
```r
# Create reactive translator (shiny.i18n recommended pattern)
i18n <- shiny::reactive({
  selected <- input$selected_language
  if (length(selected) > 0 && selected %in% translator$get_languages()) {
    translator$set_translation_language(selected)
  }
  translator
})

# Current language reactive (for modules)
current_language <- shiny::reactive({
  input$selected_language
})
```

#### Pass `i18n` to all module servers:
```r
# Example for step 1:
step1_result <- mod_step1_choose_type_server("step1", i18n = i18n)

# Example for step 2:
step2_result <- mod_step2_upload_server("step2", config = reactive(rv$config), i18n = i18n)

# etc. for all steps
```

### Step 2: Update All Module Files

For **each module file** (`R/mod_step*.R`, `R/mod_database_login.R`, `R/mod_lookup_matcher.R`), follow this pattern:

#### 1. Add `i18n` parameter to server function signature:
```r
mod_step1_choose_type_server <- function(id, i18n) {
  # ... existing code
}
```

#### 2. Replace all user-facing text with `i18n()$t("text")`:

**Before**:
```r
shiny::h3("Step 1: Choose Import Type")
```

**After**:
```r
shiny::h3(shiny::textOutput("step1_title", inline = TRUE))
```

Then add in server:
```r
output$step1_title <- shiny::renderText({
  i18n()$t("Step 1: Choose Import Type")
})
```

**OR** for simpler cases, use directly in UI (but this requires reactive context):
```r
shiny::h3(i18n()$t("Step 1: Choose Import Type"))
```

### Translation Key Mapping

Here's a mapping of common strings to their translation keys (all keys are the English text):

| UI Element | Translation Key |
|------------|----------------|
| Step titles | "Step 1: Choose Import Type", "Step 2: Upload Data or Download Template", etc. |
| Buttons | "Back", "Next", "Cancel", "Execute Import", "Run Validation", etc. |
| Status messages | "Loading...", "Complete!", "Error:", "Success:", etc. |
| Data labels | "Rows", "Columns", "Size", "Total Rows", "Errors", "Warnings", etc. |
| Actions | "Download Template", "Upload Your Data", "Apply Matches", etc. |

### Step 3: Module-Specific Updates

#### `mod_step1_choose_type.R`
- Translate: titles, descriptions, requirement texts, card labels, confirmation messages
- Key strings: "Step 1: Choose Import Type", "Select the type of data you want to import.", "Important Requirements", etc.

#### `mod_step2_upload.R`
- Translate: upload/download options, template types, file format messages, preview labels
- Key strings: "Option 1: Download Template", "Template Type:", "Generating template...", etc.

#### `mod_step3_mapping.R`
- Translate: mapping status labels, column descriptions, validation messages
- Key strings: "Auto-Mapped", "Review Suggested", "Needs Mapping", "Mapping Complete:", etc.

#### `mod_step4_lookup_matching.R`
- Translate: analysis results, matching interface, modal dialogs
- Key strings: "Analyze Lookup Values", "Match Lookup Values", "Add New Method", "Add New Person", etc.

#### `mod_step5_validation.R`
- Translate: validation status, error/warning labels, summary cards
- Key strings: "Run Validation", "Validation Passed!", "Errors", "Warnings", "Auto-Fixed", etc.

#### `mod_step6_preview.R`
- Translate: preview labels, download options, change summaries
- Key strings: "Preview Your Data", "Download Cleaned Data", "Rows to Import", etc.

#### `mod_step7_import.R`
- Translate: import options, confirmation dialogs, success/error messages
- Key strings: "Execute Import", "Dry Run (Preview)", "Import Successful:", "Admin Access Code", etc.

#### `mod_database_login.R`
- Translate: login form labels, credential messages, connection status
- Key strings: "Database Connection", "Username", "Password", "Connect to Database", etc.

#### `mod_lookup_matcher.R`
- Translate: matching interface, dropdown options, modal forms
- Key strings: "Interactive Lookup Matching", "Select correct match:", "Add New Method", etc.

### Step 4: Test Implementation

1. Launch the Import Wizard app
2. Toggle between EN and FR using the language selector
3. Verify all text updates correctly on each step
4. Test modal dialogs and dynamic content
5. Verify error/success messages are translated

### Step 5: Common Patterns

#### Pattern 1: Static text in UI (Simple)
```r
# Before
shiny::p("This is some help text")

# After
shiny::p(i18n()$t("This is some help text"))
```

#### Pattern 2: Dynamic text in UI (Reactive output)
```r
# In UI
shiny::h3(shiny::textOutput("dynamic_title", inline = TRUE))

# In Server
output$dynamic_title <- shiny::renderText({
  i18n()$t("Dynamic Title Text")
})
```

#### Pattern 3: Text with sprintf formatting
```r
# Before
sprintf("Found %d errors", n_errors)

# After
sprintf(i18n()$t("Found %d error(s)"), n_errors)
```

Note: The French translation should also use %d placeholder:
```json
{"en": "Found %d error(s)", "fr": "Trouvé %d erreur(s)"}
```

#### Pattern 4: Notification messages
```r
# Before
shiny::showNotification("Operation complete!", type = "message")

# After
shiny::showNotification(i18n()$t("Operation complete!"), type = "message")
```

#### Pattern 5: Modal dialog titles
```r
# Before
shiny::modalDialog(title = "Confirm Action", ...)

# After
shiny::modalDialog(title = i18n()$t("Confirm Action"), ...)
```

## Technical Terms NOT to Translate

The following should remain in English (technical/code terms):
- Column names: `plot_name`, `idtax_n`, `method`, `country`, etc.
- Table names: `data_liste_plots`, `methodslist`, `table_colnam`, etc.
- File extensions: `.xlsx`, `.csv`, `.R`
- Function names: `query_plots()`, `setup_db_credentials()`, etc.
- Code snippets in HTML

## Validation

After implementing, validate that:
1. ✓ JSON is valid (already validated - 549 entries)
2. All user-facing text uses `i18n()$t()`
3. Language toggle works on all screens
4. No console errors related to missing translations
5. French translations are grammatically correct and make sense

## Files Modified

### Completed ✓
- `inst/translations/translation.json` - Added 243 new entries
- `inst/translations/import_wizard_translations.json` - Created (source file)
- `merge_translations.R` - Created (merge script)

### To Be Modified
- `R/shiny_app_import_wizard.R` - Add translator initialization and language toggle
- `R/mod_step1_choose_type.R` - Add i18n to all text
- `R/mod_step2_upload.R` - Add i18n to all text
- `R/mod_step3_mapping.R` - Add i18n to all text
- `R/mod_step4_lookup_matching.R` - Add i18n to all text
- `R/mod_step5_validation.R` - Add i18n to all text
- `R/mod_step6_preview.R` - Add i18n to all text
- `R/mod_step7_import.R` - Add i18n to all text
- `R/mod_database_login.R` - Add i18n to all text
- `R/mod_lookup_matcher.R` - Add i18n to all text

## Notes

- Total translation entries: **549** (was 306, added 243 new)
- Languages supported: **English (en)** and **French (fr)**
- Translation keys: Use exact English text as the key
- Default language: French ("fr")
- The implementation follows the exact same pattern as `R/shiny_app_taxonomic_match.R`

## Support

If you encounter missing translations:
1. Check if the English key matches exactly (case-sensitive)
2. Add missing entries to `inst/translations/translation.json`
3. Run the app again to verify

For questions about the i18n implementation pattern, refer to:
- `R/shiny_app_taxonomic_match.R` (reference implementation)
- `R/utils_i18n.R` (helper functions)
- shiny.i18n documentation: https://github.com/Appsilon/shiny.i18n
