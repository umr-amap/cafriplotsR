# Implementation: User-Friendly Plot Features Addition

## Summary

Created a new user-friendly function `add_plot_features()` that wraps the low-level `add_subplot_features()` function with intelligent column mapping, validation, and clear user feedback.

## Files Created

### 1. Main Function Implementation
**File**: `R/add_plot_features_user_friendly.R`

**Key Features**:
- ✅ Intelligent column mapping (exact, synonym, fuzzy, interactive)
- ✅ Automatic plot ID detection (`plot_name` or `id_liste_plots`)
- ✅ Data validation (plots exist, feature types valid)
- ✅ Dry run mode for previewing changes
- ✅ People linking via `.link_colnam()` for people-related features
- ✅ Detailed progress feedback with `cli` package
- ✅ Comprehensive error handling

**Main Function**:
```r
add_plot_features(
  data,                    # User's data frame
  plot_id_column = NULL,   # Auto-detects plot_name or id_liste_plots
  column_mapping = NULL,   # Optional pre-defined mapping
  interactive = TRUE,      # Interactive column mapping
  dry_run = TRUE,          # ALWAYS preview first!
  con = NULL,
  similarity_threshold = 0.6,
  ask_before_update = TRUE,
  verbose = TRUE
)
```

**Internal Helper Functions**:
- `.identify_plot_id_column()` - Detects plot identifier column
- `.map_subplot_feature_columns()` - Intelligent column mapping
- `.get_subplot_feature_synonyms()` - Common synonym dictionary
- `.find_synonym_match()` - Synonym matching
- `.find_fuzzy_match()` - Fuzzy string matching using `stringdist`
- `.validate_plot_features_data()` - Data validation
- `.prepare_subplot_features()` - Data preparation for `add_subplot_features()`
- `print.plot_features_result()` - Pretty printing of results

### 2. Comprehensive Vignette
**File**: `vignettes/adding-plot-features.Rmd`

**Contents**:
- Quick start example
- Step-by-step workflow
- Advanced usage (custom mappings, multiple people, census dates)
- Common scenarios with code examples
- Troubleshooting guide
- Best practices

**Coverage**:
- 8 complete examples
- Column mapping details (exact, synonym, fuzzy, interactive)
- People field handling (comma-separated, automatic linking)
- Census date handling
- Plot ID vs plot name usage
- Verification after import

### 3. Example Script
**File**: `examples/example_add_plot_features.R`

**8 Examples Included**:
1. Simple team information
2. Census date information
3. Custom column names with interactive mapping
4. Pre-defined column mapping (non-interactive)
5. Multiple people (comma-separated)
6. Using plot IDs instead of names
7. Verifying features were added
8. Listing available subplot features

---

## Architecture

### Workflow

```
USER DATA
    ↓
1. IDENTIFY PLOT ID COLUMN
   - Auto-detect plot_name or id_liste_plots
   - Fuzzy matching if needed
   - Interactive selection as fallback
    ↓
2. MAP COLUMNS TO FEATURES
   - Exact match: "team_leader" → "team_leader"
   - Synonym match: "PI" → "principal_investigator"
   - Fuzzy match: "TeamLeader" → "team_leader"
   - Interactive selection for unmapped columns
    ↓
3. VALIDATE DATA
   - Check plots exist in database
   - Verify feature types are valid
   - Warn about empty values
    ↓
4. PREPARE DATA
   - Link plot names to IDs (if needed)
   - Handle people features (link to table_colnam)
   - Format for add_subplot_features()
    ↓
5. DRY RUN OR IMPORT
   - Preview changes (dry_run = TRUE)
   - Actually import (dry_run = FALSE)
   - Use add_subplot_features() internally
    ↓
RESULT
   - Success/failure status
   - Number of records added
   - Feature types processed
   - Column mapping used
```

### Synonym Dictionary

The function recognizes common synonyms:

```r
team_leader: teamleader, team_lead, teamlead, leader, chef_equipe
principal_investigator: pi, princ_invest, investigator, lead_researcher
data_manager: datamanager, data_mgr, manager, gestionnaire
data_provider: dataprovider, provider, fournisseur
additional_people: additional_person, other_people, others
census_date: date, census, survey_date, date_census
plot_area: area, surface, plot_size
vegetation_type: vegetation, veg_type, forest_type
locality_name: locality, location, site, lieu
```

### Fuzzy Matching

Uses Jaro-Winkler distance via `stringdist` package:
- Threshold: 0.6 (60% similarity)
- Converts to similarity score (1 = identical, 0 = completely different)
- Suggests best match if similarity ≥ threshold
- Interactive confirmation before auto-mapping

---

## Usage Examples

### Basic Usage

```r
library(CafriplotsR)

# Prepare data
plot_features <- data.frame(
  plot_name = c("Plot-A", "Plot-B", "Plot-C"),
  team_leader = c("John Doe", "Jane Smith", "Bob Wilson"),
  principal_investigator = c("Dr. Smith", "Dr. Smith", "Dr. Jones")
)

# DRY RUN (always do this first!)
result <- add_plot_features(data = plot_features, dry_run = TRUE)

# Actual import
result <- add_plot_features(data = plot_features, dry_run = FALSE)
```

### Custom Column Names

```r
# Data with non-standard names
my_data <- data.frame(
  PlotName = c("Plot-A", "Plot-B"),
  TeamLead = c("John Doe", "Jane Smith"),
  PI = c("Dr. Smith", "Dr. Jones")
)

# Interactive mode will help map columns
result <- add_plot_features(data = my_data, interactive = TRUE, dry_run = TRUE)

# Or provide explicit mapping
mapping <- list(PlotName = "plot_name", TeamLead = "team_leader", PI = "principal_investigator")
result <- add_plot_features(data = my_data, column_mapping = mapping, interactive = FALSE, dry_run = FALSE)
```

### Multiple People

```r
# Comma-separated people
plot_features <- data.frame(
  plot_name = c("Plot-A", "Plot-B"),
  team_leader = c("John Doe, Jane Smith", "Bob Wilson"),
  additional_people = c("Alice, Tom, Emma", "Chris")
)

# Function handles splitting and linking automatically
result <- add_plot_features(data = plot_features, dry_run = FALSE)
```

---

## Key Design Decisions

### 1. Reuse Existing Infrastructure
- Uses `add_subplot_features()` internally (no duplication)
- Uses `.link_colnam()` for people linking
- Uses `subplot_list()` for feature discovery
- Uses `query_plots()` for validation
- Follows existing package patterns and conventions

### 2. Safe by Default
- `dry_run = TRUE` by default (must explicitly set FALSE)
- Interactive mode by default (guides new users)
- Comprehensive validation before import
- Clear error messages and warnings

### 3. Progressive Disclosure
- Simple usage for basic cases
- Advanced options for power users
- Non-interactive mode for scripts
- Detailed feedback at each step

### 4. Column Mapping Intelligence
Uses multiple strategies in order:
1. Exact match (fastest)
2. Synonym match (common variations)
3. Fuzzy match (typos, case differences)
4. Interactive selection (fallback)

### 5. People Feature Handling
Special handling for `valuetype = "table_colnam"`:
- Splits comma-separated names
- Uses `.link_colnam()` to match/add people
- Converts to `id_table_colnam` for storage
- Interactive prompts for new people (if not in dry run)

---

## Testing Checklist

### Unit Tests (TODO)
- [ ] Column mapping (exact, synonym, fuzzy)
- [ ] Plot ID detection
- [ ] Data validation
- [ ] People feature handling
- [ ] Date feature handling
- [ ] Dry run mode
- [ ] Error handling

### Integration Tests (TODO)
- [ ] Complete workflow with real database
- [ ] Multiple feature types simultaneously
- [ ] Custom column mappings
- [ ] Non-interactive mode
- [ ] Verify with `query_subplot_features()`

### Manual Testing
- [x] Created comprehensive example script
- [x] Tested column mapping logic
- [x] Verified integration with existing functions
- [ ] Test with actual database (requires user credentials)

---

## Future Enhancements

### 1. Shiny App (as requested)
Create an interactive Shiny app for adding plot features:
- File upload (Excel/CSV)
- Interactive column mapping interface
- Visual data preview
- Feature type selector with descriptions
- Real-time validation feedback
- Dry run preview before import
- One-click import button

**Proposed structure**:
```r
launch_add_plot_features_app <- function() {
  # UI with:
  # - File upload
  # - Column mapping interface (drag-and-drop?)
  # - Data preview table
  # - Feature type descriptions
  # - Validation results
  # - Dry run preview
  # - Import button
}
```

### 2. Batch Operations
- Import from multiple files
- Update existing features (not just add)
- Delete features
- Export feature templates

### 3. Enhanced Validation
- Check for duplicate features (same plot + feature type + date)
- Validate date ranges (year >= 1900, <= current year)
- Cross-reference with existing census data
- Warn about outliers or suspicious values

### 4. Additional Features
- Support for more date formats (ISO 8601, etc.)
- Automatic date parsing from single date column
- Support for hierarchical features (subplot → observation)
- Bulk people import/management

---

## Dependencies

### Required Packages
- `DBI` - Database interface
- `dplyr` - Data manipulation
- `tidyr` - Data tidying (`separate_rows`)
- `rlang` - Non-standard evaluation (`!!`, `sym()`)
- `cli` - Command line interface (messages, alerts)

### Optional Packages
- `stringdist` - Fuzzy string matching (gracefully degrades if not available)
- `readxl` - Reading Excel files (for user workflows)

### Existing Package Functions
- `call.mydb()` - Database connection
- `subplot_list()` - Get available features
- `query_plots()` - Validate plots exist
- `.link_colnam()` - Link people to table_colnam
- `try_open_postgres_table()` - Safe table access
- `add_subplot_features()` - Low-level import (main workhorse)

---

## Documentation Generated

Running `roxygen2::roxygenise()` generated:
- `man/add_plot_features.Rd` - Function documentation
- Updated `NAMESPACE` with export for `add_plot_features()`
- S3 method for `print.plot_features_result()`

---

## Next Steps

### Immediate
1. ✅ Function implementation complete
2. ✅ Comprehensive vignette created
3. ✅ Example script created
4. ✅ Documentation generated
5. ⏳ **Test with actual database** (requires user credentials)

### Short-term
1. Create unit tests
2. Create integration tests
3. Add to package README
4. Update NEWS.md

### Medium-term
1. **Develop Shiny app** (as requested)
2. Add more examples to vignette
3. Create video tutorial/walkthrough
4. Add French translations (vignette-fr, function messages)

### Long-term
1. Extend to handle updates (not just additions)
2. Add batch operations
3. Enhanced validation rules
4. Feature template generator

---

## Questions for User

1. **Testing**: Do you have test plots we can use for testing?
2. **Shiny App Priority**: Should we start on the Shiny app next?
3. **Validation Rules**: Any specific validation rules for certain feature types?
4. **Date Handling**: How should we handle partial dates (year only, year+month only)?
5. **Multilingual**: Need French translations for messages?
6. **Feature Discovery**: Should we add a helper function to explore available features with examples?

---

## Summary

The `add_plot_features()` function is now ready for use! It provides:

✅ **User-friendly interface** - Simple for beginners, powerful for experts
✅ **Intelligent mapping** - Auto-maps columns with minimal user input
✅ **Safe operations** - Dry run by default, comprehensive validation
✅ **Clear feedback** - Detailed progress messages and error handling
✅ **Well documented** - Vignette, examples, roxygen documentation
✅ **Follows conventions** - Consistent with existing package patterns

**Ready for**: Testing with actual database and development of Shiny app interface.
