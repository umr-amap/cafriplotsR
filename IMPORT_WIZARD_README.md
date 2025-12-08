# Import Wizard - Shiny App

**Status:** 🚧 Work in Progress (Steps 1-2 implemented)

---

## Overview

The **Import Wizard** is an interactive Shiny app that provides a user-friendly graphical interface for importing plot metadata and individual tree data into the CafriPlots database. It wraps the existing robust import functions from CafriplotsR in a step-by-step wizard interface.

---

## Features

### ✅ Implemented (Steps 1-2)

- **Step 1: Choose Import Type**
  - Visual card selection (Plot Metadata vs Individual Trees)
  - Clear descriptions and requirements for each type

- **Step 2: Upload Data or Download Template**
  - Download pre-formatted Excel templates
  - Support for multiple template types (minimal, permanent plot, transect, full)
  - Upload Excel (.xlsx, .xls) or CSV files
  - Live data preview with summary statistics
  - Interactive data table showing first 100 rows

### 🚧 In Progress (Steps 3-6)

- **Step 3: Map Columns** (Coming next)
  - Visual column mapping interface with dropdowns
  - Auto-mapping with confidence indicators
  - Shows sample values for context

- **Step 4: Validate Data** (To do)
  - Visual validation dashboard
  - Interactive error fixing
  - Warnings and passed checks display

- **Step 5: Preview Import** (To do)
  - Dry run preview
  - Summary of what will be imported
  - Tabbed data preview

- **Step 6: Execute Import** (To do)
  - Progress tracking
  - Transaction-based import
  - Admin code generation and copy
  - Import report download

---

## How to Launch

```r
library(CafriplotsR)

# Launch the wizard
launch_import_wizard()

# Or launch in RStudio Viewer pane
launch_import_wizard(launch_browser = FALSE)
```

---

## Requirements

### R Packages

**Required:**
- `shiny` (>= 1.7.0)
- `shinyjs` (for JavaScript interactions)
- `DT` (for interactive tables)
- `readxl` (for reading Excel files)
- `writexl` (for writing Excel templates)

**All other functionality uses existing CafriplotsR functions**

### Installation

```r
# Install required packages
install.packages(c("shiny", "shinyjs", "DT", "readxl", "writexl"))

# Load CafriplotsR (assumes you have the package installed/loaded)
library(CafriplotsR)
```

---

## Architecture

### Modular Design

The app is built using **Shiny modules** for clean code organization:

```
R/
├── shiny_app_import_wizard.R      # Main app launcher & UI framework
├── mod_step1_choose_type.R        # Step 1: Choose import type
├── mod_step2_upload.R             # Step 2: Upload data/template
├── mod_step3_mapping.R            # Step 3: Column mapping (TO DO)
├── mod_step4_validation.R         # Step 4: Validation (TO DO)
├── mod_step5_preview.R            # Step 5: Preview (TO DO)
└── mod_step6_execute.R            # Step 6: Import execution (TO DO)
```

### State Management

The app uses **reactive values** to store state across steps:

```r
rv <- reactiveValues(
  step = 1,                    # Current step (1-6)
  max_step_reached = 1,        # Track progress
  import_type = NULL,          # "plots" or "individuals"
  config = NULL,               # Import configuration
  data = NULL,                 # Uploaded data
  mappings = NULL,             # Column mappings
  validation = NULL,           # Validation results
  dry_run_result = NULL,       # Dry run preview
  import_result = NULL         # Final import result
)
```

### Wrapper Approach

**The wizard does NOT reimplement any logic.** It wraps existing CafriplotsR functions:

| Step | Wrapped Function |
|------|------------------|
| Step 2 | `get_plot_metadata_template()` |
| Step 3 | `map_user_columns()` |
| Step 4 | `validate_plot_metadata()` |
| Step 5 | `import_plot_metadata(..., dry_run = TRUE)` |
| Step 6 | `import_plot_metadata(..., dry_run = FALSE)` |

---

## UI Design Principles

### Visual Hierarchy

- **Progress indicator** at top shows current step
- **Color coding**:
  - Blue = Active step
  - Green = Completed step
  - Gray = Future step

### User Guidance

- **Icons** for visual recognition
- **Tooltips and help text** explain each option
- **Example values** shown in context
- **Validation feedback** in real-time

### Responsive Layout

- **Two-column layouts** for choices (e.g., template download vs upload)
- **Card-based design** for options
- **Mobile-friendly** (will work on tablets)

---

## Current Step Details

### Step 1: Choose Import Type

**Purpose:** Select whether importing plot metadata or individual tree data

**UI Elements:**
- Two clickable cards (plots vs individuals)
- Icons for visual distinction
- Lists of required/included fields
- Info box about import order

**Output:** Sets `rv$import_type` and loads configuration

---

### Step 2: Upload Data or Download Template

**Purpose:** Get data into the wizard via template or upload

**UI Elements:**

**Left Side (Template Download):**
- Radio buttons for template type selection
- Checkbox to include examples
- Download button
- Generates Excel file with `get_plot_metadata_template()`

**Right Side (File Upload):**
- File input (accepts .xlsx, .xls, .csv)
- Format and size information
- Supported formats list

**Data Preview (after upload):**
- Summary statistics (rows, columns, size)
- Column names display
- Interactive DT table (first 100 rows)

**Output:** Sets `rv$data` with uploaded data frame

---

## Testing

### Manual Testing Checklist (Steps 1-2)

- [x] Launch app successfully
- [x] Step 1: Click "Plot Metadata" card - highlights correctly
- [x] Step 1: Click "Individual Trees" card - switches selection
- [x] Step 2: Download minimal template - file downloads
- [x] Step 2: Download permanent plot template with examples - includes data
- [x] Step 2: Upload Excel file - loads and previews correctly
- [x] Step 2: Upload CSV file - loads and previews correctly
- [ ] Step 2: Upload invalid file - shows error gracefully
- [ ] Navigation: Back button works
- [ ] Navigation: Next button disabled until data loaded
- [ ] Navigation: Next button enabled after data loaded

### Unit Testing (Future)

Once all steps are complete, add unit tests for:
- Module servers returning correct reactives
- Data validation logic
- Column mapping accuracy
- Import transaction success/rollback

---

## Next Steps (Development Roadmap)

### Immediate (This Week)

1. **Implement Step 3: Column Mapping**
   - Visual dropdown interface
   - Auto-mapping with confidence scores
   - Sample value display
   - Wrapper for `map_user_columns()`

2. **Test Steps 1-3** together
   - Full workflow from type selection → upload → mapping
   - Edge cases (missing columns, wrong formats)

### Short-term (Next 2 Weeks)

3. **Implement Step 4: Validation**
   - Visual dashboard for errors/warnings
   - Interactive fixing options
   - Wrapper for `validate_plot_metadata()`

4. **Implement Step 5: Preview**
   - Dry run execution
   - Summary statistics
   - Tabbed data preview
   - Wrapper for `import_plot_metadata(..., dry_run = TRUE)`

5. **Implement Step 6: Execute Import**
   - Progress bar with real-time updates
   - Transaction execution
   - Success/error handling
   - Admin code display and copy
   - Wrapper for `import_plot_metadata(..., dry_run = FALSE)`

### Medium-term (1 Month)

6. **Add Individual Tree Import** workflow (similar steps for individuals)

7. **Polish & UX improvements**
   - Better error messages
   - Keyboard shortcuts
   - Help tooltips
   - Tutorial mode

8. **Testing & Documentation**
   - Unit tests for all modules
   - User guide vignette
   - Video tutorial

---

## Known Issues

- None yet (only Steps 1-2 implemented)

---

## Contributing

When adding new features:

1. **Create feature branch**: `git checkout -b feature/wizard-step-X`
2. **Use modular approach**: Each step in separate `mod_stepX_*.R` file
3. **Reuse existing functions**: Wrapper approach, no reimplementation
4. **Document thoroughly**: Roxygen comments + README updates
5. **Test**: Manual testing checklist before committing

---

## License

Same as CafriplotsR package (GPL-2)

---

## Questions?

Contact: [Your contact info]

---

**Version:** 0.1.0 (Alpha - Steps 1-2 only)
**Last Updated:** 2025-11-27
**Branch:** feature/import-wizard-shiny-app
