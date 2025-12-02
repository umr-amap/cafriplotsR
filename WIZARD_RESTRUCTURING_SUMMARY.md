# Import Wizard Restructuring Summary

## Overview

The import wizard has been restructured to make **lookup matching a proactive, dedicated step** instead of a reactive error-fixing process. This significantly improves the user experience and workflow logic.

## Rationale

### Previous Problems:
- ❌ **Reactive approach**: Users discovered lookup errors only after running validation
- ❌ **Inefficient workflow**: Validate → see errors → fix → re-validate
- ❌ **Poor UX**: Lookup mismatches felt like errors when they're actually expected
- ❌ **Exact matches are rare**: Most lookup values won't match exactly

### New Approach:
- ✅ **Proactive matching**: Handle lookup values upfront before validation
- ✅ **Expected workflow**: Users know matching is a standard step
- ✅ **Cleaner validation**: Becomes final verification, not error discovery
- ✅ **Better UX**: Matching feels like data preparation, not error fixing
- ✅ **One-time process**: All lookup values handled together

## New Workflow Structure

### Previous (6 steps):
1. Choose Type
2. Upload Data
3. Map Columns
4. **Validate** (includes reactive lookup matching)
5. Preview
6. Import

### New (7 steps):
1. Choose Type
2. Upload Data
3. Map Columns
4. **Match Lookups** (NEW - proactive, dedicated step)
5. Validate (simplified - uses matched data)
6. Preview
7. Import

## Files Modified

### 1. **R/mod_step4_lookup_matching.R** (NEW)
**Purpose**: Proactive lookup matching before validation

**Key Functions**:
- `mod_step4_lookup_matching_ui()` - UI with analyze button and matching interface
- `mod_step4_lookup_matching_server()` - Server logic
- `.analyze_lookup_columns()` - Analyzes data to find non-exact matches

**Workflow**:
1. User clicks "Analyze Lookup Values"
2. System checks each lookup column against database
3. Identifies exact matches vs. values needing matching
4. Shows summary cards (total, exact matches, need matching)
5. If non-exact matches found, shows interactive matcher
6. User resolves matches or adds new values
7. Applies matches to data
8. Returns matched data for validation

**Features**:
- Fuzzy matching with similarity scores
- Add new methods/people (NOT countries)
- Clear analysis summary
- Optional skip if user wants to handle errors later

### 2. **R/mod_step5_validation.R** (renamed from mod_step4_validation.R)
**Changes**:
- Renamed functions: `mod_step4_*` → `mod_step5_*`
- Updated step labels: "Step 4" → "Step 5"
- **Removed all lookup matching integration**:
  - Removed `current_data` reactive value
  - Removed `lookup_errors` reactive value
  - Removed lookup error extraction logic
  - Removed lookup matcher UI rendering
  - Removed matcher server initialization
  - Removed match application logic
  - Removed `.extract_lookup_errors()` helper function
- Now receives matched data from Step 4
- Pure validation logic only

### 3. **R/mod_step6_preview.R** (renamed from mod_step5_preview.R)
**Changes**:
- Renamed functions: `mod_step5_*` → `mod_step6_*`
- Updated step labels: "Step 5" → "Step 6"
- No functional logic changes

### 4. **R/shiny_app_import_wizard.R**
**Changes**:
- **Step indicator**: Updated from 6 to 7 steps
- **Step labels**: Added "Match Lookups" as step 4
- **Step content rendering**: Added step 4 UI, renumbered 4→5, 5→6, 6→7
- **Reactive values**: Added `matched_data_result` and `matched_data`
- **Navigation logic**: Updated `can_proceed_to_next_step()`:
  - Step 4: Check lookup matching complete
  - Step 5: Check validation passed
  - Step 6: Check validation passed (preview)
  - Step 7: Import (FALSE - different logic)
- **Module initialization**:
  - Added Step 4 (lookup matching) server
  - Updated Step 5 to use matched data instead of raw data
  - Updated Step 6 (preview) server call
- **Final step check**: Changed from step 6 to step 7

## Data Flow

```
Step 1: Choose Type → rv$import_type
                    → rv$config

Step 2: Upload Data → rv$data

Step 3: Map Columns → rv$mapping_result
                    → rv$mappings

Step 4: Match Lookups → rv$matched_data_result
                       → rv$matched_data (used in validation)

Step 5: Validate → rv$validation (uses matched_data)

Step 6: Preview → (uses rv$validation)

Step 7: Import → (to be implemented)
```

## User Experience Comparison

### Before:
```
User: [Uploads data] → [Maps columns] → [Clicks "Validate"]
System: "Validation failed: 15 errors found"
User: [Sees lookup errors] → [Fixes matches] → [Re-validates]
System: "Still 3 errors remaining"
User: [Fixes more] → [Re-validates again]
```

### After:
```
User: [Uploads data] → [Maps columns] → [Clicks "Match Lookups"]
System: "Found 12 values needing matching"
User: [Reviews matches, adds new values if needed] → [Applies]
System: "Matching complete!"
User: [Proceeds to validation]
System: "Validation passed!" (or only non-lookup errors shown)
```

## Technical Implementation Details

### Step 4 Lookup Matching Module

**Analysis Process**:
1. Gets lookup columns: method, country, people fields
2. Extracts unique values from user data
3. Queries database for each lookup table
4. Compares normalized values (lowercase, trimmed)
5. Categorizes as exact match or needs matching

**Matching Process**:
- Reuses existing `mod_lookup_matcher` module
- Shows fuzzy matches with Jaro-Winkler similarity
- Allows adding new values where permitted
- Updates data by replacing user values with matched IDs

**Return Value**:
```r
list(
  data = reactive(matched_data),   # Updated data
  complete = reactive(matching_complete)  # TRUE when done
)
```

### Validation Module Changes

**Before** (reactive matching):
- Ran validation first
- Extracted lookup errors
- Showed matcher if errors
- Re-validated after matches
- Complex state management

**After** (receives matched data):
- Receives already-matched data from Step 4
- Runs pure validation
- No lookup error handling
- Simple, focused logic

### Main Wizard Integration

**Step 4 Initialization**:
```r
step4_result <- mod_step4_lookup_matching_server(
  "step4",
  data = reactive(rv$data),
  mappings = reactive(rv$mappings),
  con = pool_main_reactive
)

observeEvent(step4_result$complete(), {
  rv$matched_data <- step4_result$data()
  # Enables Step 5
})
```

**Step 5 Validation (uses matched data)**:
```r
step5_result <- mod_step5_validation_server(
  "step5",
  data = reactive({
    if (!is.null(rv$matched_data)) rv$matched_data else rv$data
  }),
  ...
)
```

## Benefits

1. **Clearer workflow**: Each step has a single, focused purpose
2. **Better UX**: Users understand what's happening at each stage
3. **Proactive approach**: Handle expected issues upfront, not reactively
4. **Simpler code**: Validation module is now pure and focused
5. **Reusable matching**: Same `mod_lookup_matcher` used in dedicated step
6. **Efficient**: All lookup values handled in one pass
7. **Flexible**: Users can still skip matching if they prefer manual fixes

## Testing Checklist

- [ ] Step 4 analyzes lookup values correctly
- [ ] Exact matches identified properly
- [ ] Non-exact matches shown with fuzzy suggestions
- [ ] Add new method functionality works
- [ ] Add new person functionality works
- [ ] Country cannot add new values
- [ ] Matched data passed to Step 5 correctly
- [ ] Step 5 validation uses matched data
- [ ] Navigation enables/disables at correct times
- [ ] Back button works from Step 5 to Step 4
- [ ] Can skip matching and proceed with original data
- [ ] Preview shows correct matched data

## Migration Notes

**For users**: The wizard now has 7 steps instead of 6. The new Step 4 ("Match Lookups") is mandatory and guides you through matching your lookup values before validation.

**For developers**:
- Old `mod_step4_validation.R` is now `mod_step5_validation.R`
- Old `mod_step5_preview.R` is now `mod_step6_preview.R`
- New `mod_step4_lookup_matching.R` handles proactive matching
- Main wizard updated to use new step structure
- Validation module simplified significantly
