# Individual Import Templates - Test Results

**Date**: 2025-11-10
**Phase**: Phase 1 - Templates
**Branch**: `feature/add-individual-import`

## Overview

This document contains test results for the individual data import template generation system. The template system creates Excel files for importing individual tree data with proper structure, validation rules, and examples.

## Functions Tested

### 1. `get_individual_template()`
Main function for generating individual data import templates.

**Parameters**:
- `method`: Optional method filter (e.g., "1ha-IRD", "Large")
- `include_features`: Include features sheet (default TRUE)
- `output_file`: Output Excel file path
- `con`: Database connection (optional)

### 2. `print_individual_template_info()`
Displays template structure and requirements to console.

**Parameters**:
- `con`: Database connection (optional)

## Test Cases

### Test 1: Basic Template Generation

**Command**:
```r
get_individual_template(
  output_file = "test_individual_template.xlsx",
  con = call.mydb(pass = "AmapENS2024", user = "dauby")
)
```

**Result**: ✅ **SUCCESS**

**Output**:
```
── Generating Individual Data Import Template ──────────────────────────────────
ℹ Fetching column definitions from database...
ℹ Building 'individuals' sheet...
ℹ Building 'features' sheet...
ℹ Writing template to: test_individual_template.xlsx
✔ Template created successfully!
── Important Notes ─────────────────────────────────────────────────────────────
! Before filling the template:
• Standardize taxonomic names using `match_taxonomic_names()` or the Shiny app
• Ensure idtax_n column has values for ALL individuals
• Keep original_tax_name for traceability
ℹ Next steps:
1. Fill in the template with your data
2. Use `map_individual_columns()` to map columns
3. Use `validate_individual_data()` to check data quality
4. Use `import_individual_data()` to import to database
```

**Template Structure**:
- **Sheet 1: "individuals"** - Core individual data
  - 7 columns (4 mandatory + 3 optional)
  - 4 rows (1 header + 3 example rows)

- **Sheet 2: "features"** - Individual traits
  - 10 columns (3 linking + 7 trait columns)
  - 3 rows (1 header + 2 example rows)

**Columns in "individuals" sheet**:
1. `plot_name` (mandatory) - Plot identifier
2. `tag` (mandatory) - Individual tag number
3. `idtax_n` (mandatory) - Taxonomy ID (pre-matched)
4. `original_tax_name` (mandatory) - Original taxonomic name
5. `herbarium_nbe_type` (optional) - Herbarium specimen type
6. `herbarium_nbe_char` (optional) - Herbarium specimen code
7. `multi_tiges_id` (optional) - Multi-stem identifier

**Columns in "features" sheet**:
1. `plot_name` (linking) - Plot identifier
2. `tag` (linking) - Individual tag number
3. `census_date` (linking) - Census date
4. `stem_diameter` - Stem diameter (cm)
5. `height_of_stem_diameter` - Height where diameter was measured (m)
6. `tree_height` - Tree height (m)
7. `crown_width` - Crown width (m)
8. `specific_leaf_area` - Specific leaf area (cm²/g)
9. `wood_specific_gravity` - Wood specific gravity (g/cm³)
10. `leaf_area` - Leaf area (cm²)

### Test 2: Method-Specific Template

**Command**:
```r
get_individual_template(
  method = "1ha-IRD",
  output_file = "test_method_template.xlsx",
  con = call.mydb(pass = "AmapENS2024", user = "dauby")
)
```

**Result**: ✅ **SUCCESS**

**Notes**:
- Template structure identical to basic template
- Method-specific validation rules applied (tag mandatory for "1ha-IRD")
- Same columns as basic template

### Test 3: Template Without Features Sheet

**Command**:
```r
get_individual_template(
  include_features = FALSE,
  output_file = "test_no_features.xlsx",
  con = call.mydb(pass = "AmapENS2024", user = "dauby")
)
```

**Result**: ✅ **SUCCESS**

**Output**:
- Single sheet: "individuals" only
- Features sheet omitted as expected
- Proper messaging about next steps

### Test 4: Template Information Display

**Command**:
```r
print_individual_template_info(
  con = call.mydb(pass = "AmapENS2024", user = "dauby")
)
```

**Result**: ✅ **SUCCESS**

**Output**:
```
── Individual Data Template Information ────────────────────────────────────────

── Core Individual Columns (Sheet 1) ──

── Mandatory Columns
ℹ plot_name (character)
Plot name (must exist in database and be accessible to user)
Example: "PLOT001"
ℹ tag (numeric)
Individual tag/number (unique within plot, must be numeric)
Example: "1234"
ℹ idtax_n (integer)
Taxonomy ID from taxa database (pre-matched using taxonomy tools)
Example: "12345"
ℹ original_tax_name (character)
Original taxonomic name before standardization (for traceability)
Example: "Coula edulis"

── Optional Columns
ℹ herbarium_nbe_type (character)
Herbarium specimen reference type (if specimen exists)
ℹ herbarium_nbe_char (character)
Herbarium specimen reference code/number
ℹ multi_tiges_id (character)
Multi-stem identifier (for trees with multiple stems)

── Individual Features (Sheet 2 - Optional) ──

ℹ Common traits available:
• stem_diameter
• height_of_stem_diameter
• tree_height
• crown_width
• specific_leaf_area
• wood_specific_gravity
• leaf_area
ℹ Use `traits_list()` to see all 93 available traits

── Workflow ──

1. Standardize taxonomy (`match_taxonomic_names()` or Shiny app)
2. Generate template (`get_individual_template()`)
3. Fill in your data
4. Map columns (`map_individual_columns()`)
5. Validate (`validate_individual_data()`)
6. Import (`import_individual_data()`)
```

## Important Features Verified

### 1. Taxonomy Pre-requisite Warnings
✅ Template prominently displays warnings about taxonomy standardization:
- Must use `match_taxonomic_names()` or Shiny app first
- `idtax_n` column must have values for ALL individuals
- `original_tax_name` kept for traceability

### 2. Two-Sheet Structure
✅ Template follows same pattern as plot metadata:
- Sheet 1: Core individual data (flat columns)
- Sheet 2: Optional individual features (traits)

### 3. Dynamic Trait Columns
✅ Features sheet includes 7 most common traits:
- `stem_diameter`: Stem diameter (cm)
- `height_of_stem_diameter`: Height where diameter was measured (m)
- `tree_height`: Tree height (m)
- `crown_width`: Crown width (m)
- `specific_leaf_area`: Specific leaf area (cm²/g)
- `wood_specific_gravity`: Wood specific gravity (g/cm³)
- `leaf_area`: Leaf area (cm²)

**Note**: Trait names match actual database `traits_list()` output (93 total traits available).

### 4. Example Data
✅ Template includes realistic example data:
- 3 example individuals with varied data
- Shows optional fields as both filled and empty
- Includes multiple plots (PLOT001, PLOT002)

### 5. Flexible Configuration
✅ Template supports:
- Method-specific requirements
- Optional features sheet inclusion
- Custom output file paths
- Connection reuse or auto-creation

## Helper Functions Tested

### `.get_individual_columns_from_db()`
✅ Retrieves column definitions correctly:
- Returns 4 mandatory columns
- Returns 3 optional columns
- Includes descriptions and validation rules
- Includes example values

### `.get_trait_columns_from_db()`
✅ Retrieves trait definitions correctly:
- Filters to 7 common traits when `common_traits_only = TRUE`
- Returns all 93 traits when `common_traits_only = FALSE`
- Includes validation rules (min/max ranges)
- Includes appropriate example values per trait

### `.build_individuals_sheet()`
✅ Builds individuals sheet correctly:
- Creates proper column structure
- Includes 3 example rows
- Preserves column order

### `.build_features_sheet()`
✅ Builds features sheet correctly:
- Includes linking columns (plot_name, tag, census_date)
- Includes 7 dynamic trait columns
- Includes 2 example rows

## Issues Found and Fixed

### Issue 1: Incorrect Trait Names
**Problem**: Initial implementation used incorrect trait names:
- `pom` instead of `height_of_stem_diameter`
- `bark_thickness` instead of actual database trait
- `wood_density` instead of `wood_specific_gravity`
- `sla` instead of `specific_leaf_area`

**Result**: Only 3 out of 7 traits appeared in template.

**Fix**: Updated `.get_trait_columns_from_db()` to use actual database trait names:
```r
common_trait_names <- c(
  "stem_diameter", "tree_height", "height_of_stem_diameter",
  "wood_specific_gravity", "leaf_area", "specific_leaf_area",
  "crown_width"
)
```

**Verification**: ✅ All 7 traits now appear in template and info output.

## Database Compatibility

✅ **Tested with**:
- PostgreSQL database `plots_transects`
- User: `dauby` (admin credentials for testing)
- 93 traits available in `traits_list()`

## Documentation Generated

✅ **roxygen2 documentation created**:
- `man/get_individual_template.Rd`
- `man/print_individual_template_info.Rd`

✅ **NAMESPACE updated**:
- `get_individual_template` exported
- `print_individual_template_info` exported

## Next Steps (Phase 2)

Following the same pattern as plot metadata import:

1. **Column Mapping** (`R/import_individuals_column_mapping.R`)
   - `map_individual_columns()` - Interactive column mapping
   - Handle flat columns (plot_name, tag, idtax_n, etc.)
   - Handle feature columns (traits)
   - Support for method-specific requirements

2. **Validation** (`R/import_individuals_validation.R`)
   - `validate_individual_data()` - Data quality checks
   - Plot existence and access validation
   - Tag uniqueness validation
   - Taxonomy ID validation (must exist, not NULL/0)
   - Method-specific field requirements
   - Trait value type and range validation

3. **Import with Transactions** (`R/import_individuals_with_transactions.R`)
   - `import_individual_data()` - Main import function
   - Transaction-based import with rollback
   - Link to plots
   - Insert individuals into `data_individuals`
   - Insert features into `data_ind_measures_feat`
   - Census linking for temporal data

## Conclusion

Phase 1 (Templates) is **COMPLETE** and **WORKING**.

All template functions tested successfully with:
- ✅ Correct column structure
- ✅ Proper trait names from database
- ✅ Clear user guidance and warnings
- ✅ Flexible configuration options
- ✅ Comprehensive documentation
- ✅ Example data for user reference

Ready to proceed to **Phase 2 - Column Mapping**.
