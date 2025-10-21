# Modular Shiny App Design: Taxonomic Name Standardization

**Date**: 2025-10-20
**Branch**: feature/taxonomic-name-standardization
**Purpose**: Replace monolithic `launch_stand_tax_app()` with modular, maintainable Shiny app

## Current App Analysis

### Existing App (`shiny_app_taxo_match.R`)

**Problems**:
- 1299 lines in a single function
- French-only interface
- Hardcoded UI elements
- Complex reactive value management
- Difficult to test or extend
- Uses deprecated matching functions

**Workflow** (from code analysis):
1. Upload Excel file with taxonomic names
2. Select column to standardize
3. Auto-match exact names against backbone (species, genus, family)
4. For unmatched names:
   - Show fuzzy match suggestions (top 10/20/30)
   - Allow manual selection or custom input
   - Navigate through names one by one
5. Export results with matched IDs and corrected names

### Key Features to Preserve

✅ **File upload**: Excel (.xlsx) format
✅ **Auto-matching**: Exact match on species/genus/family
✅ **Fuzzy matching**: Similarity-based suggestions
✅ **Manual review**: Interactive correction of unmatched names
✅ **Progress tracking**: Show number of names remaining
✅ **Export**: Download results with matched IDs

### Features to Add/Improve

🆕 **Bilingual interface** (English/French)
🆕 **Direct R data mode** (not just file upload)
🆕 **Better matching** (use new `match_taxonomic_names()`)
🆕 **Batch review** (not just one-by-one)
🆕 **Match quality indicators** (score, confidence)
🆕 **Undo functionality**
🆕 **Filter/search** in suggestions

---

## Modular Architecture

### Module Structure

```
R/
├── shiny_modules/
│   ├── mod_data_input.R         # File upload or R data input
│   ├── mod_column_select.R      # Select column to standardize
│   ├── mod_auto_matching.R      # Automatic exact matching
│   ├── mod_name_review.R        # Review unmatched names
│   ├── mod_fuzzy_suggestions.R  # Show similarity suggestions
│   ├── mod_progress_tracker.R   # Progress summary
│   ├── mod_results_export.R     # Export results
│   └── mod_language_toggle.R    # Language switcher
├── shiny_app_taxonomic_match.R  # Main app orchestration
└── launch_taxonomic_match_app.R # Launch function
```

### Launch Function API

```r
launch_taxonomic_match_app <- function(
  data = NULL,                    # Optional: provide data directly
  name_column = NULL,             # Optional: pre-select column
  language = c("en", "fr"),       # Default language
  min_similarity = 0.3,           # Fuzzy match threshold
  max_suggestions = 10,           # Max suggestions per name
  mode = c("interactive", "batch"), # Review mode
  launch.browser = TRUE           # Launch in browser
) {
  # ...
}
```

**Usage examples**:
```r
# Launch with file upload
launch_taxonomic_match_app()

# Launch with R data
data <- read.csv("my_data.csv")
launch_taxonomic_match_app(data = data, name_column = "species")

# Launch in French
launch_taxonomic_match_app(language = "fr")

# Batch mode (review all at once)
launch_taxonomic_match_app(mode = "batch")
```

---

## Module Specifications

### 1. Data Input Module (`mod_data_input`)

**Purpose**: Upload file or provide R data

**UI**:
```
┌─────────────────────────────────┐
│ Data Input                       │
├─────────────────────────────────┤
│ ○ Upload Excel file              │
│   [Choose file...]               │
│                                  │
│ ○ Use R data (from environment)  │
│   [Data already provided]        │
└─────────────────────────────────┘
```

**Inputs**:
- File upload widget
- Option to use pre-provided data

**Outputs**:
- Reactive data frame

**State**:
- `original_data`: Unmodified user data
- `file_name`: Name of uploaded file

---

### 2. Column Select Module (`mod_column_select`)

**Purpose**: Select which column contains taxonomic names

**UI**:
```
┌─────────────────────────────────┐
│ Column Selection                 │
├─────────────────────────────────┤
│ Select name column:              │
│ [dropdown: species ▼]            │
│                                  │
│ ☑ Match with author names        │
└─────────────────────────────────┘
```

**Inputs**:
- Dropdown of character columns
- Checkbox for author matching

**Outputs**:
- Selected column name
- Include authors flag

---

### 3. Auto Matching Module (`mod_auto_matching`)

**Purpose**: Automatically match names against backbone

**Logic**:
1. Extract unique names from selected column
2. Call `match_taxonomic_names()` for each unique name
3. Update data with matched IDs and corrected names
4. Return unmatched names for review

**Outputs**:
- Updated data (with matched IDs)
- List of unmatched names
- Match summary stats

**Internal columns added**:
- `idtax_n`: Matched taxon ID
- `idtax_good_n`: Accepted taxon ID (for synonyms)
- `matched_name`: Matched name from backbone
- `match_method`: How matched (exact, genus_constrained, fuzzy)
- `match_score`: Similarity score
- `corrected_name`: Final standardized name

---

### 4. Progress Tracker Module (`mod_progress_tracker`)

**Purpose**: Show progress and summary statistics

**UI**:
```
┌─────────────────────────────────────────┐
│ Progress Summary                         │
├─────────────────────────────────────────┤
│ Total unique names:        245           │
│ Exact matches:             198 (81%)     │
│ Genus-level matches:        23 (9%)      │
│ Fuzzy matches:              12 (5%)      │
│ Requiring review:           12 (5%)      │
│                                          │
│ Progress: ████████████░░░░ 95%           │
└─────────────────────────────────────────┘
```

**Inputs**:
- Match results
- Review status

**Outputs**:
- Progress bar
- Summary stats

---

### 5. Name Review Module (`mod_name_review`)

**Purpose**: Review and manually correct unmatched names

**Two modes**:

#### A. Interactive Mode (one-by-one)
```
┌──────────────────────────────────────────────────┐
│ Review Unmatched Names (12 remaining)            │
├──────────────────────────────────────────────────┤
│ Input name: Gilbertodendron dewevrei             │
│                                                  │
│ Suggestions (ranked by similarity):              │
│                                                  │
│ ○ Gilbertiodendron dewevrei   (score: 0.95) ✓  │
│ ○ Gilbertiodendron ogoouense  (score: 0.72)     │
│ ○ Gilbertiodendron bilineatum (score: 0.68)     │
│ ○ Enter custom name: [_____________]             │
│ ○ Mark as unresolved                             │
│                                                  │
│ [< Previous] [Skip] [Accept] [Next >]            │
└──────────────────────────────────────────────────┘
```

#### B. Batch Mode (table review)
```
┌────────────────────────────────────────────────────────┐
│ Review All Unmatched Names                              │
├────────────────────────────────────────────────────────┤
│ [Filter: _________] [Show: All ▼]                      │
│                                                        │
│  Input Name          | Best Match      | Score | Action│
│ ─────────────────────────────────────────────────────  │
│  Brachysteiga        | Brachystegia    | 0.95  | [✓][✗]│
│  Garcinea kola       | Garcinia kola   | 0.92  | [✓][✗]│
│  Unknown species     | No match        | -     | [✓][✗]│
│                                                        │
│ [Accept All] [Reject All] [Export]                     │
└────────────────────────────────────────────────────────┘
```

**Inputs**:
- List of unmatched names
- Fuzzy match suggestions
- User selection/custom input

**Outputs**:
- Updated matches
- Navigation state

---

### 6. Fuzzy Suggestions Module (`mod_fuzzy_suggestions`)

**Purpose**: Generate and display similarity-based suggestions

**Logic**:
1. Call `match_taxonomic_names()` with `max_matches = N`
2. Rank by similarity score
3. Display with match metadata

**UI**:
```
┌─────────────────────────────────────────────────┐
│ Suggestions for: "Brachysteiga laurentii"       │
├─────────────────────────────────────────────────┤
│ Number of suggestions: [10 ▼] [20] [30]         │
│ Sort by: [Similarity ▼] [Alphabetical]          │
│ Show authors: ☐                                  │
│                                                  │
│ 1. Brachystegia laurentii      [95%] [SELECT]   │
│    Family: Fabaceae | Genus: Brachystegia       │
│    Match: genus_constrained                      │
│                                                  │
│ 2. Brachystegia leonensis      [68%] [SELECT]   │
│    Family: Fabaceae | Genus: Brachystegia       │
│    Match: genus_constrained                      │
│                                                  │
│ [Show more...]                                   │
└─────────────────────────────────────────────────┘
```

**Inputs**:
- Input name
- Number of suggestions
- Sort order
- Include authors

**Outputs**:
- Ranked suggestions with scores
- Match metadata

---

### 7. Results Export Module (`mod_results_export`)

**Purpose**: Export standardized results

**UI**:
```
┌─────────────────────────────────────────┐
│ Export Results                           │
├─────────────────────────────────────────┤
│ Export format:                           │
│ ○ Excel (.xlsx) - recommended            │
│ ○ CSV (.csv)                             │
│ ○ R data (.rds)                          │
│                                          │
│ Include columns:                         │
│ ☑ Original data                          │
│ ☑ Matched IDs (idtax_n, idtax_good_n)   │
│ ☑ Corrected names                        │
│ ☑ Match metadata (method, score)        │
│ ☐ Taxonomic hierarchy (family, genus)   │
│                                          │
│ [Download]                               │
└─────────────────────────────────────────┘
```

**Outputs**:
- Downloadable file
- Optional: return to R environment

---

### 8. Language Toggle Module (`mod_language_toggle`)

**Purpose**: Switch between English and French

**UI**:
```
┌────────────────┐
│ Language       │
│ [EN] [FR]      │
└────────────────┘
```

**Implementation**:
- Reactive language variable
- Translation dictionary
- `i18n()` helper function

**Translation structure**:
```r
translations <- list(
  en = list(
    title = "Taxonomic Name Standardization",
    upload_file = "Upload Excel file",
    select_column = "Select name column",
    ...
  ),
  fr = list(
    title = "Standardisation de noms taxonomiques",
    upload_file = "Télécharger un fichier Excel",
    select_column = "Sélectionner la colonne de noms",
    ...
  )
)
```

---

## Main App Orchestration

### File: `R/shiny_app_taxonomic_match.R`

```r
#' Main Shiny app for taxonomic name standardization
#'
#' @keywords internal
app_taxonomic_match <- function(
  data = NULL,
  name_column = NULL,
  language = "en",
  min_similarity = 0.3,
  max_suggestions = 10,
  mode = "interactive"
) {

  ui <- fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),

    # Language toggle (top right)
    absolutePanel(
      top = 10, right = 10,
      mod_language_toggle_ui("language")
    ),

    # Title
    titlePanel(
      textOutput("app_title")
    ),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        width = 3,
        mod_data_input_ui("data_input"),
        hr(),
        mod_column_select_ui("column_select"),
        hr(),
        mod_progress_tracker_ui("progress")
      ),

      # Main panel
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "main_tabs",
          type = "pills",

          tabPanel(
            "Auto Match",
            mod_auto_matching_ui("auto_match")
          ),

          tabPanel(
            "Review",
            mod_name_review_ui("review")
          ),

          tabPanel(
            "Export",
            mod_results_export_ui("export")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

    # Language management
    language <- mod_language_toggle_server("language", initial = language)

    # Translations
    t <- reactive({
      get_translations(language())
    })

    output$app_title <- renderText({
      t()$title
    })

    # Data input
    user_data <- mod_data_input_server(
      "data_input",
      provided_data = data,
      language = language
    )

    # Column selection
    column_info <- mod_column_select_server(
      "column_select",
      data = user_data,
      initial_column = name_column,
      language = language
    )

    # Auto matching
    match_results <- mod_auto_matching_server(
      "auto_match",
      data = user_data,
      column_name = column_info$column,
      include_authors = column_info$include_authors,
      min_similarity = min_similarity,
      language = language
    )

    # Progress tracking
    mod_progress_tracker_server(
      "progress",
      match_results = match_results,
      language = language
    )

    # Manual review
    reviewed_results <- mod_name_review_server(
      "review",
      match_results = match_results,
      mode = mode,
      max_suggestions = max_suggestions,
      min_similarity = min_similarity,
      language = language
    )

    # Export
    mod_results_export_server(
      "export",
      results = reviewed_results,
      original_data = user_data,
      language = language
    )
  }

  shinyApp(ui, server)
}
```

---

## Implementation Plan

### Phase 1: Core Infrastructure (Current Sprint)

1. ✅ Create matching functions (`match_taxonomic_names()`)
2. ✅ Refactor `query_taxa()` to use new functions
3. ⏳ Create translation system
4. ⏳ Set up module structure

### Phase 2: Basic Modules (Next Sprint)

1. Implement `mod_data_input`
2. Implement `mod_column_select`
3. Implement `mod_auto_matching`
4. Implement `mod_progress_tracker`

### Phase 3: Review & Export (Following Sprint)

1. Implement `mod_name_review` (interactive mode)
2. Implement `mod_fuzzy_suggestions`
3. Implement `mod_results_export`
4. Implement `mod_language_toggle`

### Phase 4: Advanced Features (Future)

1. Implement batch review mode
2. Add undo/redo functionality
3. Add match history
4. Add advanced filtering

---

## File Structure Summary

```
R/
├── shiny_modules/
│   ├── mod_data_input.R            # ~100 lines
│   ├── mod_column_select.R         # ~80 lines
│   ├── mod_auto_matching.R         # ~150 lines
│   ├── mod_name_review.R           # ~200 lines
│   ├── mod_fuzzy_suggestions.R     # ~120 lines
│   ├── mod_progress_tracker.R      # ~80 lines
│   ├── mod_results_export.R        # ~100 lines
│   ├── mod_language_toggle.R       # ~60 lines
│   └── utils_translations.R        # ~50 lines
├── shiny_app_taxonomic_match.R    # ~200 lines (main app)
└── launch_taxonomic_match_app.R   # ~50 lines (launch function)

Total: ~1190 lines (similar to current, but modular)
```

---

## Benefits of Modular Architecture

### Maintainability
✅ Each module has single responsibility
✅ Easier to locate and fix bugs
✅ Changes isolated to specific modules

### Testability
✅ Modules can be tested independently
✅ Mock reactive values for unit tests
✅ Integration tests for full workflow

### Extensibility
✅ Easy to add new modules
✅ Easy to add features to existing modules
✅ Can reuse modules in other apps

### User Experience
✅ Cleaner UI with tabbed interface
✅ Better progress feedback
✅ More intuitive workflow
✅ Bilingual support

---

## Next Steps

1. Create module directory structure
2. Implement translation system
3. Build modules one by one (following phases above)
4. Test each module independently
5. Integrate into main app
6. User testing and feedback
7. Deprecate old `launch_stand_tax_app()`

---

**Author**: Claude Code Assistant
**Date**: 2025-10-20
**Status**: Design Complete - Ready for Implementation
