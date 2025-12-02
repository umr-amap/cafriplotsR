# Step 4 Validation: Lookup Matching Implementation

## Overview

Step 4 validation now includes **interactive lookup matching** that allows users to resolve invalid lookup table values without manually editing their data files.

## How It Works

### 1. Initial Validation

When the user clicks "Run Validation":
- `validate_plot_metadata()` runs on the user's data
- Validation results are displayed (errors, warnings, auto-fixes)

### 2. Lookup Error Detection

If validation fails, the system automatically:
- Extracts invalid values for lookup columns: `method`, `country`, `principal_investigator`, `team_leader`, `data_manager`, `data_provider`
- Uses `.extract_lookup_errors()` helper to identify unique invalid values
- Stores these in `lookup_errors` reactive value

### 3. Interactive Matching Interface

If lookup errors are detected, the system displays `mod_lookup_matcher_ui`:
- **Fuzzy Matching**: Shows top 5 similar values from database with similarity scores
- **Method Descriptions**: Displays `protocol_description` for methods to help selection
- **Dropdown Selection**: User selects correct match for each invalid value
- **Add New Value**: For method and people columns (NOT for country)

### 4. Adding New Values

Users can add new entries to lookup tables:

**Methods** (`methodslist` table):
- Modal dialog with fields: `method`, `protocol_description`
- Inserts into database with `RETURNING id_method`
- Auto-updates dropdown with new ID

**People** (`table_colnam` table):
- Modal dialog with fields: `first_name`, `last_name`
- Inserts into database with `RETURNING id_table_colnam`
- Auto-updates dropdown with new ID

**Countries**: No "Add New" option (all countries exist in database)

### 5. Applying Matches

When user clicks "Apply Matches":
1. Collects all user selections from dropdowns
2. Updates `current_data` by replacing user values with matched IDs
3. Automatically re-runs validation with updated data
4. Updates UI with new validation results
5. If still errors, shows remaining lookup errors for another round

### 6. Skip Option

Users can click "Skip (Keep as Errors)" to:
- Proceed without resolving lookup errors
- Validation will still show errors
- Useful if user wants to fix data manually later

## Key Functions

### Helper Functions

**`.extract_lookup_errors(validation_result, data, mappings)`**
- Identifies validation errors related to lookup columns
- Extracts unique invalid values from user data
- Returns list: `list(method = c("val1", "val2"), country = c("val3"))`

**`.get_lookup_info(column_name, con)`**
- Returns lookup table configuration for each column
- Specifies: table name, value column, ID column, description column
- Controls `allow_add_new` and `add_fields` per column

**`.get_fuzzy_matches(user_value, lookup_info, n = 5)`**
- Uses Jaro-Winkler string distance for similarity matching
- Returns top N matches with similarity scores
- Includes descriptions for better user decision-making

**`.create_matching_row(user_value, column_name, lookup_info, session)`**
- Generates UI for one invalid value
- Shows: user value → dropdown selector → status indicator
- Includes reactive description output

### Module Structure

**mod_lookup_matcher_ui(id)**
- Header with instructions
- Dynamic matching interface (one row per invalid value)
- Action buttons: "Apply Matches", "Skip"
- Modal dialogs for adding new values

**mod_lookup_matcher_server(id, invalid_values, con)**
- Renders matching interface based on invalid_values
- Handles dropdown selections
- Manages modal dialogs for adding new entries
- Returns: `list(matches = reactive(), applied = reactive())`

## Workflow Diagram

```
[Run Validation]
      ↓
[Validation Result]
      ↓
   Passed? ──YES──> [Continue to Step 5]
      ↓
     NO
      ↓
[Extract Lookup Errors]
      ↓
 Lookup errors? ──NO──> [Show other errors only]
      ↓
    YES
      ↓
[Show Lookup Matcher]
      ↓
[User Selects Matches or Adds New Values]
      ↓
[User Clicks "Apply Matches"]
      ↓
[Update Data with Matches]
      ↓
[Re-run Validation]
      ↓
[Update UI with New Results]
      ↓
Still errors? ──YES──> [Repeat matching]
      ↓
     NO
      ↓
[Continue to Step 5]
```

## Configuration per Column

| Column | Table | Allow Add New | Add Fields |
|--------|-------|---------------|------------|
| `method` | `methodslist` | ✅ Yes | method, protocol_description |
| `country` | `table_countries` | ❌ No | - |
| `principal_investigator` | `table_colnam` | ✅ Yes | first_name, last_name |
| `team_leader` | `table_colnam` | ✅ Yes | first_name, last_name |
| `data_manager` | `table_colnam` | ✅ Yes | first_name, last_name |
| `data_provider` | `table_colnam` | ✅ Yes | first_name, last_name |

## Files Modified

- **R/mod_step4_validation.R**: Added lookup error extraction and matcher integration
- **R/mod_lookup_matcher.R**: New module for interactive lookup matching

## User Experience

1. User uploads data and maps columns
2. Clicks "Run Validation"
3. If lookup errors detected, sees friendly matching interface
4. Reviews fuzzy matches with similarity scores
5. Can add new methods/people if needed
6. Clicks "Apply Matches"
7. System automatically re-validates
8. Process repeats until all errors resolved or user skips

## Benefits

- ✅ No manual data file editing required
- ✅ Fuzzy matching reduces cognitive load
- ✅ Method descriptions help decision-making
- ✅ Can add new values directly from wizard
- ✅ Automatic re-validation ensures correctness
- ✅ Iterative workflow handles complex cases
